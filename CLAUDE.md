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

33 exports across 9 R files.

## Module Map

| File | Purpose |
|------|---------|
| `scrape_chart.R` | `scrape_chart()`, `scrape_charts()` (multi-country), `get_country_codes()` — daily/weekly charts |
| `scrape_track.R` | `scrape_track_history()`, `scrape_all_tracks()`, `get_track_metadata()`, `debug_track_page()` |
| `data_releases.R` | GitHub Releases I/O: `upload_*`, `download_release_data()`, `list_release_files()`, `delete_release_files()` |
| `data_storage.R` | Local persistence — `save_chart_data()`, `save_track_history()`, `load_chart_data()`, `load_track_history()`, `list_stored_tracks()`, `get_storage_summary()` |
| `projection_models.R` | `calculate_rolling_stats()`, `aggregate_global_streams()`, `create_temporal_features()`, `add_yoy_features()`, `build_projection_model()`, `predict_streams()`, `evaluate_model()`, `prepare_model_features()` |
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

## CI / Operations

- **Daily scrape workflow** (`.github/workflows/daily-scrape.yml`) runs at 06:00 UTC, scrapes ~200 tracks, then uploads to the `data` release tag via `upload_data_directory()`.
- **`GITHUB_PAT` must be a real PAT** (`ghp_…`, `github_pat_…`, or 40 hex chars). The auto-issued `secrets.GITHUB_TOKEN` has a `ghs_` prefix that current `gh` R package versions reject in `validate_gh_pat()`, causing the upload step to fail with "Invalid GitHub PAT format". Use `secrets.WORKFLOW_PAT` (or a kworb-scoped fine-grained PAT). Symptom of regression: failed runs complete in ~7m at the upload step while successful runs take 25-30m.
