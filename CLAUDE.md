# CLAUDE.md — kworb

R package for scraping and analyzing Spotify streaming data from [kworb.net](https://kworb.net). Daily/weekly chart scraping, per-track history with per-country breakdowns, and stream-projection models. Sister package to [`spotify/`](../spotify/) (Spotify Charts) — kworb tends to have richer per-track time-series, spotify has cleaner regional snapshots.

## Development Commands

```r
devtools::load_all()    # Iterative dev
devtools::document()    # Regenerate NAMESPACE + man/ (never edit NAMESPACE manually)
devtools::test()        # Run tests
devtools::check()       # Full R CMD check
```

## Pipeline

```
scrape_chart(country)            → snapshot of current chart (~200 tracks)
scrape_track_history(track_id)   → per-country daily stream history
  → save_chart_data() / save_track_history()  → local parquet
  → upload_release_data()        → GitHub Releases (large datasets out of git)
  → load_*_from_release()        → fetch back from releases
  → aggregate_global_streams()   → roll per-country up to global daily series
  → create_temporal_features()   → DOW / seasonal / monthly + rolling stats
  → build_projection_model()     → forecast future streams
  → predict_streams()            → apply
```

41 exports across 12 R files.

## The kworb database (docs/plans/kworb-db.md) — the current path

The old scrape pipeline above is mostly broken (plan section 1.1) and is being replaced in stages:

```
capture-raw.yml (daily 21:30 UTC, on master)
  inst/scripts/capture_raw.R   → raw pages saved exactly as received → mediadata release kworb-raw-YYYY-MM (one tar + manifest a run)
  inst/scripts/parse_raw.R     → parse_raw_file() per page → checks PER CAPTURE (bad captures → <db>/_quarantine.csv, the run
                                 fails only if the newest day's capture is bad) → <db>/<table>/month=<capture month>/part.parquet
  inst/scripts/publish_db.R    → mediadata release kworb-db: <table>__YYYY-MM.parquet (sizes verified)
inst/scripts/backfill_wayback.R → Wayback copies (one a day) of artists / global daily / weekly / daily totals → data/raw_wayback/
                                 (<slug>__wb<archive ts>.html.gz + manifest.csv); parsed to data/db_wayback, published as
                                 <table>__wayback__YYYY-MM.parquet (publish_db.R prefix 'wayback__') so the daily job never touches them
```

- Tables: `chart_entry` (daily top-200, 77 countries), `chart_totals` (global daily Totals), `artist_snapshot` (artists.html,
  no page date: dated by capture). Every row has both clocks: the day the numbers describe (`chart_date` / `snapshot_date`) and
  `captured_at` (for Wayback: when the archive captured it). An as-of query must use `captured_at <= date`.
- **Two eras on the artists page, never mix:** `era = "chart"` (to Mar 2023: streams while on the charts, exact integers) and
  `era = "all"` (from Apr 2023: every stream, millions to 0.1). `layout` names the page layout the row came from.
- Cut-off archive copies are parsed up to the cut and flagged `page_truncated`; frozen markets (Cyprus, Malta, Russia) `chart_frozen`.
- **`inst/scripts/*.R` must call unexported functions as `kworb:::name`.** Locally they run under `devtools::load_all()`, where a
  bare internal name works; the capture-raw workflow runs them on the installed package, where it does not (10 Nov 2026: a bare
  `kworb_parser_version()` failed the CI rebuild on 834 of 834 files; fixed in kworb#13). Test a script from outside the repo root.
- `kworb_db_summary(db_dir)` lists tables x months x rows. Local folders: `data/raw`, `data/raw_wayback`, `data/db`, `data/db_wayback`
  (all under `data/`, gitignored).

## Module Map

| File | Purpose |
|------|---------|
| `scrape_chart.R` | `scrape_chart()`, `scrape_charts()` (multi-country), `get_country_codes()` — daily/weekly charts |
| `scrape_track.R` | `scrape_track_history()`, `scrape_all_tracks()`, `get_track_metadata()`, `debug_track_page()` |
| `data_releases.R` | GitHub Releases I/O: `upload_*`, `download_release_data()`, `list_release_files()`, `delete_release_files()` |
| `data_storage.R` | Local persistence — `save_chart_data()`, `save_track_history()`, `load_chart_data()`, `load_track_history()`, `list_stored_tracks()`, `get_storage_summary()` |
| `projection_models.R` | `calculate_rolling_stats()`, `aggregate_global_streams()`, `create_temporal_features()`, `add_yoy_features()`, `build_projection_model()`, `predict_streams()`, `evaluate_model()`, `prepare_model_features()` |
| `fetch_raw.R` | `fetch_raw()`, `fetch_raw_pages()`: one request, body saved untouched, manifest row, STOP on 429/403 (Stage 1) |
| `parse_raw.R` | `parse_raw_file()` and the page parsers (every layout back to 2016), `assert_coverage()` (Stage 2-3) |
| `db.R` | `kworb_db_summary()` |
| `asof.R` | `kworb_artist_snapshots()` (load `artist_snapshot` from `data/db` + `data/db_wayback`), `kworb_asof()` (plan 3.4: an artist's total or window streams as of a day, strict on `captured_at`; method exact / last_before / interpolated / not_listed / missing; cut-off copies never read as "not listed") |
| `utils.R` + `globals.R` + `zzz.R` | Helpers + NSE globals + onLoad |

## Data Persistence

Two layers:
- **Local parquet** via `data_storage.R` — `save_chart_data()` / `save_track_history()` write to a configurable directory. Prefer parquet/CSV over RDS (global rule from `~/.claude/CLAUDE.md`).
- **GitHub Releases** via `data_releases.R` — the large per-track histories (~thousands of tracks) live in Releases, not git. `upload_release_data()` pushes the local store; `download_release_data()` mirrors it back.

## Key Conventions

- **Country codes** — `get_country_codes()` returns the canonical list kworb accepts. `"global"` is the default for chart scrapes; per-track history fans out across many countries automatically.
- **`pos_change` is character** — kworb encodes chart movement as `"NEW"`, `"+5"`, `"-2"`, `"="`. Don't try to coerce to numeric without handling those tokens.
- **`days` is cumulative chart tenure**, not streams. `streams_daily` / `streams_7day` / `streams_total` are the streaming columns.
- **Rolling stats**: default windows are `c(7, 14, 30)` days. `calculate_rolling_stats()` requires the input sorted by date; it sorts defensively but the caller should still pass sorted data.
- **`aggregate_global_streams()` collapses per-country → global daily** before projection. Always feed the aggregated series into `build_projection_model()`, not raw per-country.

## Analysis Pattern

```r
devtools::load_all()
history  <- load_track_history("53iuhJlwXhSER5J2IYYv1W")  # Spotify track ID
global   <- aggregate_global_streams(history)
features <- create_temporal_features(global) |>
            calculate_rolling_stats(windows = c(7, 30, 90)) |>
            add_yoy_features()
model    <- build_projection_model(features)
predict_streams(model, horizon_days = 180)
```

## Data Locations

- Local store: configurable (defaults to project-local). Inspect with `get_storage_summary()`.
- Releases: `peteowen1/kworb` GitHub releases (managed via `data_releases.R`).
