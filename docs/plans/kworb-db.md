# kworb database: audit and design

Written 5 Nov 2026 (Sydney) after Pete asked: "can we fix and improve kworb ... should we be creating a db of kworb data?"
Status: design only. Nothing in this repo was changed except this file.

Answer in one line: yes, build a database, but its job is different from what the package does today. kworb.net keeps almost no history itself (the longest daily window on any page is 31 days), so the database has to be built by taking dated snapshots of every page type every day, forward from now, and backfilling the past from the Wayback Machine. Every day not captured is lost for good.

Evidence folder (raw pages saved once each, 5 Nov 2026):
`C:\Users\peteo\AppData\Local\Temp\claude\dd2fc6323a0ea413\1efbc8f2-bbf0-4043-9782-402cac1adb14\scratchpad\kworb_audit\`
Requests made: 12 to kworb.net (one each), 6 to the Wayback CDX API, all with a descriptive user agent, 4 to 5 seconds apart, no 429 or 403.

---

## 1. What the package does today, and what is wrong with it

### 1.1 Defects (still live unless marked)

All are live on `dev` (HEAD 6a881aa) and in the running daily workflow on `master`.

| # | Defect | Where | Evidence |
|---|--------|-------|----------|
| D1 | **Track history does not accumulate.** The workflow downloads the release flat into `data/` (piggyback drops directories), but the scraper saves into `data/tracks/` and merges only with a file there, so the downloaded history is never merged. Upload then sends both `data/<id>.csv` (stale) and `data/tracks/<id>.csv` (new) under one asset name. Result: each release file holds one 31-day window at most. | `inst/scripts/daily_scrape.R:10`, `R/data_releases.R:37-42`, `.github/workflows/daily-scrape.yml:51,66`, `R/data_storage.R:342-348` | Release asset `53iuhJlwXhSER5J2IYYv1W.csv` (a track on the chart for a year) has 703 rows, 2026-10-04 to 2026-11-03, 31 dates. Two other assets: `02MWAaff...` 2026-06-25 to 2026-07-25 (31 dates), `04ld6agK...` 2 dates. The mechanism is my inference from the code; the symptom is measured. |
| D2 | **`aggregate_global_streams()` double counts.** The track page has a `Global` column plus one column per country. The parser keeps `Global` as if it were a country, then the function sums every country row. | `R/scrape_track.R:378-384`, `R/scrape_track.R:93-97` | Stored file for `53iuhJlw...` on 2026-10-20: `Global` = 1,936,693; sum of all rows = 2,869,709 (48% too high). Even without `Global`, summing countries misses every country where the track is outside the top 200, so the right number is the `Global` column. |
| D3 | **The release is at GitHub's 1,000-asset cap.** 999 assets: 151 chart files + 848 track files. Charts for 2026-11-03 and 2026-11-04 were saved by the runs (log line "Saved chart data to data/charts/global_daily_2026-11-04.csv") and are not in the release; the last chart in the release is 2026-11-02. Both runs were green. | `R/data_releases.R:96-105`, `.github/workflows/daily-scrape.yml:66` | `gh release view data -R peteowen1/kworb --json assets`: n = 999, all `updatedAt` 2026-11-04. Cause of the missing two files is inferred (cap), not read from an error message. |
| D4 | **Silent partial failure.** Upload errors, per-track errors and the cap are not surfaced. A per-track failure is swallowed without a message (`error = function(e)` only counts). The 20% failure gate only looks at track scrapes, never at the upload. `last_scrape.json` is written but the upload pattern is `\.csv$`, so it never reaches the release. | `inst/scripts/daily_scrape.R:63-67,96-101`, `R/data_releases.R:121` | Release has no non-CSV asset. |
| D5 | **The chart date is wrong.** `scrape_date` is `Sys.Date()` (run day). The page says which day it shows: `Spotify Daily Chart - Global - 2026/11/03` on 5 Nov; weekly `2026/10/29`. The run day is 1 to 2 days later, and the cron fires at 06:00 UTC but runs start 11:00 to 14:00 UTC. Every stored chart is stamped with the wrong day. | `R/scrape_chart.R:437`, `R/data_storage.R:222-237` | `global_daily.html` and `global_weekly.html` page titles. |
| D6 | **Weekly charts are mis-parsed.** The parser assumes the 11-column daily layout. The weekly table has 9 columns (Pos, P+, title, Weeks, Pk, (x?), Streams, Streams+, Total), so `streams_7day` receives the Total and the last two fields are NA. Weekly files are also named `_daily_`. | `R/scrape_chart.R:530-536`, `R/data_storage.R:237` | `global_weekly.html` colgroup has 9 `<col>`s; `global_daily.html` has 11. |
| D7 | **Fields dropped at parse time.** The peak-count cell `(x16)` (cell 6) is discarded. On the track page the position half of every `pos (streams)` cell is parsed and thrown away. Rows with 0 streams are dropped, so "zero" and "not on the chart" look the same. Total and Peak rows of the track page are skipped. The weekly table is never stored (`view` is not even a column, so weekly and daily rows would collide on the merge key). | `R/scrape_chart.R:523-537`, `R/scrape_track.R:142-148`, `R/scrape_track.R:110` | Global rule: capture every field. |
| D8 | **No raw responses saved.** HTML is parsed in memory and discarded, so a parser bug (D2, D6) cannot be fixed by re-parsing. One file per request is the standing rule. | `R/utils.R:97-130` | Nothing under `data-raw/` for scrapes. |
| D9 | **Revisions are ignored.** `merge_data()` keeps the existing row on a key clash (`distinct(.keep_all = TRUE)` after `bind_rows(existing, new)`), so a later corrected figure is never applied. | `R/data_storage.R:434-435` | Code reading; not observed live. |
| D10 | **The retry loop does not back off on a non-200.** A 429 or 403 returns NULL immediately and the loop re-requests at once, up to 3 times; backoff only happens on a thrown error. The rules elsewhere in the verse (awards `R/fetch.R`) are: wait 60 s on a 429, halve the rate, stop the host on a second 429 or any 403. The `kworb.rate_limit` option is defined and never read. | `R/utils.R:109-114,118`, `R/zzz.R:8` | Code reading. |
| D11 | **Only one page is scraped.** Global daily top 200 and the tracks on it. The package ignores 76 country charts, weekly charts, the two "Totals" pages (12,076 and 8,144 tracks), the artists list and the per-artist `_songs` pages, which hold the only true all-time stream counts. A track that drops off the global chart is never scraped again. | `inst/scripts/daily_scrape.R:22,33` | Section 2. |
| D12 | **Country list is stale.** 71 codes; the site's index lists 77. Missing: `ad by kz mt ng pk`. | `R/scrape_chart.R:588-597` | Codes parsed from `index_spotify.html`. |
| D13 | **`get_track_metadata()` guesses.** `total_streams` is "the largest number on the page" (that picks a chart-time total, which is not the track's streams); the title comes from `h1, .title, title`, which returns the whole browser title ("Taylor Swift - The Fate of Ophelia - Spotify Chart History"); the page has a plain `Title:` line. | `R/scrape_track.R:192-220` | `track_53iu.html`. |
| D14 | **No tests for any scraper or parser.** The 508 test lines cover string helpers, storage and projection maths. `parse_chart_table`, `parse_chart_row`, `parse_track_history`, `polite_read_html`, the workflow script: no fixtures. D2, D6 and D13 would each have been caught by one saved page. | `tests/testthat/*.R` | `grep` for scrape/parse in tests: none. |
| D15 | **CSV, whole-file rewrite, no schema or coverage checks.** Default should be parquet. There is no `nrow`/date-range/column-coverage assertion after a scrape; a layout change on kworb would produce an empty or shifted file and the job would still pass if 80% of tracks parsed. | `R/data_storage.R:241,351` | Global rules. |
| D16 | **Runtime is close to the limit and mostly waste.** The 4 Nov run took 44m50s of a 60-minute timeout: 6 minutes scraping 200 tracks, then 31 minutes re-uploading all 1,200 files (`overwrite = TRUE`). There is no `concurrency:` guard. | `.github/workflows/daily-scrape.yml:19`, `R/data_releases.R:96-105` | `gh run view 41837061890`: scrape ends 13:33:29, "Upload complete" 14:04:19 (UTC). |
| D17 | **Visibility history.** 25 and 26 Oct runs failed (12 and 14 min), then the 27 Oct fix ran. Charts in the release are missing 2026-02-28 to 2026-04-27 and 2026-07-28 to 2026-10-24, plus scattered days: 151 of 311 days since 2025-12-29 (49%). Nothing alerted. | `gh run list`, release asset names | |
| D18 | Housekeeping: stray empty file `_ul` at the repo root; `Authors@R` email was `pete@example.com` (now the GitHub noreply address, 10 Nov); `CLAUDE.md` says "33 exports across 9 R files" (there are 8); `jsonlite` is used by the workflow script but only in Suggests (works because `install_deps(dependencies = TRUE)`); default branch is `master` while the verse convention is `dev` then `main`, so the schedule runs from a branch that is not the working one. | repo root, `DESCRIPTION`, `CLAUDE.md` | |

Things that are fine and worth keeping: `validate_spotify_id`, `extract_spotify_id`, `parse_streams`, `parse_position_change` (all tested), the 3-row skip logic for the track page header, the chunked upload and raw-connection fix (a real fix for a real crash), the idea of a release as data bus.

### 1.2 What I did not check
- Whether `piggyback::pb_upload` raises or warns when the 1,000-asset cap is hit (D3 cause is inferred).
- Whether the stale-vs-new overwrite order in D1 is deterministic (the symptom is measured).
- I did not run `devtools::check()` or the test suite (task was an audit, no code changes).

---

## 2. What kworb.net offers, measured

Every file is in the evidence folder above. Sizes are uncompressed bytes of the saved HTML, gzip ratio roughly 10:1.

| Page (URL under `https://kworb.net/spotify/`) | Saved file | Size | Rows / content | History it carries |
|---|---|---|---|---|
| `track/<id>.html` | `track_53iu.html` (new song), `track_0VjI.html` (Blinding Lights) | 338 KB, 241 KB (26 KB gzipped) | Two tables behind a Weekly/Daily toggle, both with 70 country columns (`Global` + 69). Each cell is `position (streams)`. Row 1 header, row 2 Total, row 3 Peak. | **Daily: exactly 31 days** on both tracks (2026-10-04 to 2026-11-03). **Weekly: one row per week the track was on a chart.** The new song has 42 complete weeks (2025-10-09 to 2026-10-29) and the `Total` row equals the sum of the weekly rows (1,072,369,605). Blinding Lights has only 52 rows for a six-year run: 2019-12-05 to 2020-03-19 (16 weeks), one row 2023-08-31, then 2026-03-05 on; its `Total` (898,813,309) equals the sum of those 52 rows, not the 5.57 billion shown for it on the global Totals page. So weekly history is complete for a song under about a year old and **incomplete for long runners; why is unconfirmed**. `Total` is chart-time only. |
| `country/global_daily.html` and `<cc>_daily.html` | `global_daily.html`, `us_daily.html` | 89 KB, 86 KB | Top 200: Pos, P+ (movement), artist and title with ids, Days, Pk, (xN), Streams, Streams+, 7Day, 7Day+, Total. Date in page title (`2026/11/03`). | One day. |
| `country/global_weekly.html` and `<cc>_weekly.html` | `global_weekly.html` | 76 KB | Top 200: 9 columns (Weeks, Pk, (xN), Streams, Streams+, Total). Date `2026/10/29`. | One week (week ending Thursday). |
| `country/global_daily_totals.html` | `global_daily_totals.html` | 3.37 MB (645 KB over the wire) | **12,076 tracks**: Days, T10, Pk, (xN), PkStreams, Total. "Covers charts from 2014/08/10 to 2026/11/03. Totals do not include time spent outside the daily chart." | A cumulative snapshot of every track that ever charted globally. Differences between two snapshots give chart-time streams in between. |
| `country/global_weekly_totals.html` | `global_weekly_totals.html` | 2.28 MB | **8,144 tracks**, from 2013/09/29 to 2026/10/29. | Same, weekly. |
| `artists.html` | `artists.html` | 743 KB (152 KB gzipped) | **3,000 artists** (hard cap): Streams and Daily **in millions to one decimal** (so 100,000 resolution), As lead, Solo, As feature. Link goes to `<id>_songs.html`. "Section still under construction." | Snapshot of all-time streams at the page date. |
| `artist/<id>_songs.html` | `artist_06HL_songs.html` | 99 KB (23 KB gz) | Artist totals (Streams 133,027,028,861; Daily; Tracks 670; lead/solo/feature) and **a per-track table of exact all-time Streams and Daily with Spotify track ids**. Table holds **500 rows while the page says 670 tracks** (top 500 by streams). Feature credits marked `*`. `Last updated: 2026/11/04`. | Snapshot of true all-time per-track totals. This is the only page with real totals rather than chart-time totals. |
| `artist/<id>.html` | `artist_06HL.html` | 339 KB | One row per charted track: peak date, title, chart-time streams, peak position per country (70 columns). | Chart-time only. |
| `/spotify/` (index) | `index_spotify.html` | 23 KB | 77 countries x (daily, daily_totals, weekly, weekly_totals) links. | Country list for discovery. |
| `/robots.txt` | `robots.txt` | 142 B | `User-agent: * / Allow: /` | Scraping allowed. |

Summary of what this means:
1. **A song's daily stream history since release does not exist on kworb.** The track page gives 31 days. Daily totals before that exist only inside charts (top 200 per country per day) and in snapshots.
2. **True all-time streams per track exist only on `_songs` pages**, and per artist only for the top 500 tracks. All other "totals" are chart-time totals and exclude days outside the top 200.
3. **Artists list is hard-capped at 3,000 and rounded to 0.1 million.** The awards scripts' "top 10k / 3k" truncation is the site's, not theirs.

### 2.1 What the Wayback Machine holds (CDX, collapsed to one per day)

| Page | Day-collapsed copies | First | Last | By year |
|---|---|---|---|---|
| `country/global_daily.html` | 791 | 2016-04-17 | 2026-11-03 | 2016:3 2017:3 2019:14 2020:7 2021:14 2022:165 2023:108 2024:120 2025:217 2026:140 |
| `country/global_weekly.html` | 189 | 2016-07-06 | 2026-11-02 | 2022:43 2023:6 2024:13 2025:60 2026:46 |
| `country/global_daily_totals.html` | 70 | 2016-07-06 | 2026-10-09 | 2025:14 2026:14 |
| `artists.html` | 684 | 2016-07-06 | 2026-11-01 | 2023:118 2024:135 2025:238 2026:130 |
| `track/53iuhJlw...html` (one song) | 8 | 2025-10-09 | 2026-10-27 | too thin to rely on |
| `artist/06HL...songs.html` (one big artist) | 175 | 2023 | 2026 | 2023:25 2024:41 2025:81 2026:28 |

Files: `cdx_global_daily.json`, `cdx_global_weekly.json`, `cdx_global_daily_totals.json`, `cdx_artists.json`, `cdx_track_53iu.json`, `cdx_artist_songs.json`. Not measured: the 76 other country charts, and the weekly-totals count (the awards pool script reports 43 usable copies from 2016-07). Awards already holds 87 raw artists copies and 1 weekly-totals copy (`C:\dev\mediaverse\awards\data-raw\kworb_wayback\`, 23 MB; and 39 MB in `mediaverse_grammy_pool\`), saved raw and gzipped, ready to ingest with no new requests.

Wayback density is high from 2022 on (about one copy every 2 to 3 days for the global daily chart and the artists list) and sparse before. Pre-2022 there are only a handful per year.

### 2.2 Why the awards reconstruction leaks and is approximate (from reading 157 and 178)
- Artists copies come from Wayback, and `asof` is "latest copy on or before the checkpoint", or in 178 "up to 30 days after nominations if nothing nearer" (`NOMS_TOL`). Rows in `early_grammy_pool_ceremonies.csv` show `asof_days_after_noms` of 580 and 213 days (57th, 58th: no copy before 2016-07), 18, 24, 25 (66th to 68th). Those are leaks or stale windows.
- Two eras on different scales (chart-time top-10,000 to March 2023; all streams from April 2023), so windows across the boundary are dropped.
- `window` = copy dates, not the eligibility dates, e.g. 59th "EARLIEST same-era copy: shorter window".

---

## 3. Design

### 3.1 Principles
1. **Raw first, one file per request, never re-fetched, never edited.** Everything else is rebuilt from raw.
2. **Two clocks on every row.** `as_of_date` = the day the number describes. `captured_at` = when we (or Wayback) saw it. Leakage is a question about `captured_at`; windows are a question about `as_of_date`. Keeping both is what lets the awards code ask a clean question.
3. **Keep every field**, including the ones we think are useless (`(xN)`, positions, change columns, Total/Peak rows, feature flags, the page's own date string, `Last updated`).
4. **Parse is a pure function of the raw file**, versioned (`parser_version`). A parser bug is fixed by re-parsing, not re-scraping.
5. **Quality is data.** Each answer says how it was obtained (exact page, interpolated between two snapshots, chart-time only) and how far away the evidence is.
6. **Assert coverage, not presence**, after every parse: row counts, expected columns populated above a floor, date in page equals date expected.

### 3.2 Storage

- **Raw**: `data-raw/<page_type>/<yyyy>/<mm>/<name>.html.gz` plus a manifest row per request. Not in git. Published monthly as one tar (`raw-2026-11.tar.zst`, a few hundred MB at most; see 3.6) to a **private** release, because it is a third party's pages. Recommend the private `mediadata` repo (releases only) over the public `kworb` repo. Needs Pete's decision (Section 7).
- **Tables**: parquet, hive-partitioned by month (`chart_entry/month=2026-11/part.parquet`). Published as one asset per table per month to a release tag `kworb-db` (about 6 tables x 12 months = 72 assets a year, so the 1,000-asset cap is 10+ years away; D3 cannot recur). Current month's assets are replaced each day; closed months never change.
- **Query engine**: DuckDB reading the parquet (`duckdb` R package: a single file, no `arrow` dependency, which segfaults under Git Bash R per the global notes; not tested here). A local `kworb.duckdb` is a cache built by `kworb_sync()`, not a published artefact.
- **Why not SQLite/Postgres/Supabase**: the data is append-mostly, read in bulk by R scripts, and published through releases; parquet + DuckDB fits the rest of the verse and costs nothing to host. A hosted database is only worth it if something other than R scripts needs live queries.

### 3.3 Tables

Grain, key, then columns. All tables also carry `request_id` (foreign key to `raw_request`) and `parser_version`.

**`raw_request`** (manifest; the first table to exist)
- Grain: one HTTP request. Key: `request_id` (hash of url + fetched_at).
- `url, page_type, entity_id (country code | spotify id | NULL), source ('live'|'wayback'), wayback_ts, fetched_at (UTC), http_status, bytes, sha256, raw_path, page_date_text, page_date (parsed from the page itself), parse_status, rows_parsed, notes`.

**`chart_entry`** (daily and weekly top-200 charts)
- Grain: one track at one position on one chart. Key: `(country, frequency, chart_date, pos)`.
- `country, frequency, chart_date (from the page), pos, pos_change_raw, pos_change_type, pos_change_value, track_id, title, artist_ids (list), artist_names, periods_on_chart (Days or Weeks), peak_pos, peak_count (the xN), streams, streams_change, streams_7day, streams_7day_change (daily only), streams_total, captured_at, as_of_date (= chart_date)`.
- Size: 77 countries x 200 x 365 = 5.6 million rows a year daily (rough parquet estimate 60 to 100 MB; not measured).

**`chart_totals`** (the two "Totals" pages)
- Grain: one track in one snapshot. Key: `(country, frequency, snapshot_date, track_id)`.
- `periods, top10_periods, peak_pos, peak_count, peak_streams, total_streams (chart-time), covers_from, covers_to, artist_ids, title`. 12,076 rows a day for global daily.

**`artist_snapshot`** (artists.html)
- Grain: one artist in one snapshot. Key: `(snapshot_date, artist_id)`.
- `rank, artist_name, streams, daily, as_lead, solo, as_feature` stored in **raw units** (millions x 1e6) with `unit_resolution = 100000`, plus `layout_era ('chart'|'all')` so the two eras cannot be mixed by accident.

**`artist_totals`** and **`artist_song_snapshot`** (the `_songs` pages)
- `artist_totals`: key `(snapshot_date, artist_id)`: `streams, daily, tracks, lead..., page_updated`.
- `artist_song_snapshot`: key `(snapshot_date, artist_id, track_id)`: `rank, title, is_feature, streams, daily`. Marked `truncated = TRUE` when rows < `tracks`.

**`track_daily`** and **`track_weekly`** (from track pages)
- `track_daily` grain: `(track_id, country, date)`; `track_weekly`: `(track_id, country, week_ending)`.
- `position, streams, first_captured_at, last_captured_at, n_obs, first_streams, last_streams` (so revisions are visible; the value used is the latest, the first is kept). A track page's `Total` and `Peak` rows go to `track_page_summary(track_id, country, captured_at, total_streams, peak_pos, peak_streams)`.
- Empty cell stays absent; a real 0 is stored as 0 (fixes D7).

**`entity`** (track and artist dimension)
- `track_id, title, artist_ids, first_seen, last_seen`; `artist_id, name, first_seen`; no fuzzy matching in this table. Mapping to Wikidata Spotify ids (P1902) stays in awards.

**`scrape_log`** (one row per scheduled run and page type)
- `run_id, page_type, planned, ok, failed, http_429, http_403, started, finished, coverage_flags`. This is the table the workflow reads to go red.

### 3.4 `as_of(date)`: the API the awards scripts call

```r
kworb_asof(ids, date, what = c("artist_total", "track_total", "window_streams"),
           window_days = 365, strict = TRUE, max_gap_days = 45)
# returns one row per id:
#   id, as_of_date (the requested day), value,
#   method ('exact' | 'interpolated' | 'chart_time_only' | 'extrapolated' | 'missing'),
#   snap_before_date, snap_after_date, gap_days, source ('live'|'wayback'),
#   era ('chart'|'all'), resolution
```

Rules that fix the problems in 157 and 178:
- `strict = TRUE` uses a snapshot only if `captured_at <= date`. No copy that lands after nominations can be used for a pre-nomination date. (`strict = FALSE` allowed for reporting.)
- Between two snapshots on either side of the day, interpolate using the artist's `daily` (or the track's daily streams) rather than a straight line, and label `interpolated` with `gap_days`. Beyond `max_gap_days`, return `missing` rather than a stale number.
- `window_streams(id, start, end)` = value at `end` minus value at `start`, both from the same `era`; if the eras differ the method is `missing`, not silently shortened.
- Track-level `track_total` for dates before 2026-11 can only be `chart_time_only` (from `chart_totals` differences and `chart_entry` sums) or from `_songs` Wayback copies; it never claims to be all-time unless a `_songs` snapshot is within the gap limit.
- From the first live capture onward every day is `exact` (daily `artist_snapshot`, weekly `_songs`).
- Honest limit: for dates before about 2022 there are only a handful of copies a year; those answers will be `interpolated` with large gaps or `missing`. That is the correct answer, and the awards code can decide to drop the feature for old ceremonies instead of getting a fake number.

### 3.5 Daily update (GitHub Actions)

Per-request delay 3 seconds (matches awards `fetch_raw`: wait 60 s on a 429, double the delay, stop the host on a second 429 or any 403).

| Job | Requests | Time at 3 s | Frequency |
|---|---|---|---|
| Daily charts, 77 countries | 77 | 3.9 min | daily |
| `global_daily_totals` | 1 | 0.1 min | daily |
| `artists.html` | 1 | 0.1 min | daily |
| Weekly charts, 77 countries + `global_weekly_totals` | 78 | 3.9 min | weekly (Fri) |
| Artist `_songs` pages, watchlist of N artists | N / 7 a day | 1,500 artists: 215/day = 11 min | rotation, each artist weekly |
| Track pages, watchlist of M tracks | M / 7 a day | 3,000 tracks: 430/day = 21 min | each track every 7 days (window is 31 days, so a missed week is covered) |

About 40 minutes a day at the sizes above (about 700 requests; the watchlist sizes are my assumptions, not measured). The watchlist = every track on any chart in the last 60 days, plus every awards-relevant artist (Wikidata Spotify ids from awards). Daily sets are small enough to fit in one job; the backfill is sharded (3.6).

Workflow shape: fetch -> save raw -> parse -> **assert** (row counts, date equals expected, coverage floors) -> write month partitions -> upload only changed assets -> write `scrape_log` -> job fails if any assertion fails or any planned page type is under 95% ok. Add `concurrency:` and a failure notification (issue comment or `ops` sentinel). Cron stays, but the date used is the page's date, not the clock.

### 3.6 Backfill plan and size

Arithmetic at 3 s per request, serial. Pages are from the saved files above (gzipped size = transfer).

| Step | What | Requests | Time | Data (gz) | Basis |
|---|---|---|---|---|---|
| B0 | Ingest awards' existing raw copies (artists 87, weekly totals 1, others) | 0 web | minutes | 62 MB on disk | `awards\data-raw` listing |
| B1 | Wayback `artists.html`, copies not already held | 684 - 87 = about 600 | 30 min | about 130 MB (awards avg 217 KB/file) | CDX 684 |
| B2 | Wayback `global_daily.html` | 791 | 40 min | about 19 MB (24 KB each) | CDX 791 |
| B3 | Wayback `global_weekly.html` | 189 | 9.5 min | about 4 MB | CDX 189 |
| B4 | Wayback `global_daily_totals.html` | 70 | 3.5 min | about 58 MB (826 KB gz each; the 2 MB+ weekly-totals pages are similar) | CDX 70 |
| B5 | Wayback `_songs` for watchlist artists | 1,500 artists x ~100 copies (Taylor Swift has 175; assume 100) = 150,000 | 125 h | 3.5 GB | **extrapolation, not measured**. Do not do this. Take the nearest-to-checkpoint copies only, about 1,500 x 12 = 18,000 requests, 15 h. |
| B6 | Live track pages, all 12,076 ever-charted global tracks (weekly rows + 31 daily days) | 12,076 | 10.1 h | 314 MB (26 KB gz each) | measured page size |
| B7 | Live `_songs` for the 3,000 listed artists | 3,000 | 2.5 h | 69 MB | measured page size |
| B8 | Wayback country charts, 76 countries | unmeasured | | | needs 76 CDX calls first (about 4 min). If each country is near the global 791, that is about 60,000 requests (50 h); recommend top ~10 markets only, or skip. |

Total for the recommended core (B0 to B4, B6, B7): about 4,600 + 12,076 + 3,000 = about 19,700 requests, about 16.4 hours of polite wall-clock, roughly 0.6 GB gzipped. B6 and B7 run as separate Actions jobs of 3 hours each (6 hour job limit), over about 4 nights. B1 to B4 are 1.5 hours.

What the backfill buys: artists series back to 2016 at the density in 2.1; daily chart streams for the top 200 on about 790 days; chart-time totals at 70 dates; weekly rows from track pages for everything under a year old. It does **not** produce a complete daily series for older songs. That does not exist anywhere on kworb.

### 3.7 Tests

- **Fixtures**: commit trimmed copies of the saved pages (first 5 rows + header, a handful of KB each) under `tests/testthat/fixtures/` for every page type, in each layout era (artists `chart` and `all`; weekly 9-column; daily 11-column; track page).
- **Parser tests**: for each page, exact expected values (e.g. `53iuhJlw` weekly Total 1,072,369,605 equals sum of weekly rows; Global peak `1 (128,896,137)`; artists row `Drake 144,103.6 -> 144,103,600`).
- **Regression tests for D2, D6, D7, D13** by name.
- **Property checks**: parsed `Total` row equals sum of weekly rows; chart pos is 1..200 with no gaps; `chart_date` equals the date in the title.
- **Coverage assertions** as a runtime function (`assert_coverage(table)`) used by the workflow and tested with deliberately broken input (a column renamed upstream must fail).
- **as_of tests**: a synthetic snapshot set with known answers: exact, interpolated, gap too wide, strict excludes a later capture, era change yields missing.
- **Network**: mocked (`local_mocked_bindings`, already used in this repo); one opt-in live test per page type behind an env var.
- **Release round trip**: write to a temp release directory, read back, compare row counts and key uniqueness.

---

## 4. Staged rollout

The order is set by what can never be recovered: a day not captured is gone.

**Stage 1 (about 1 day): capture only.** Add `fetch_raw()` (copy the policy from awards `R/fetch.R`), the `raw_request` manifest, and a new scheduled workflow that saves raw gz for: 77 daily country charts, `global_daily_totals`, `artists.html`. No parsing in the job except reading the page date. Publish a monthly raw tar to the private release. Keep the old workflow running unchanged until Stage 3 (it is mostly broken, but harmless), or stop it at once to free the cap: Pete's call.
Done means: 7 consecutive days of 79 raw files each, manifest rows with `http_status = 200`, page dates equal to expected, tests for the fetch policy (429 retry, 403 stop) passing.

**Stage 2 (about 2 days): parsers + tests + parquet.** Fixtures and parsers for the five page types, `chart_entry`, `chart_totals`, `artist_snapshot`, coverage assertions, month partitions in `kworb-db`. Re-parse all Stage 1 raw. Ingest the 106 awards raw files (B0).
Done means: tests green, `kworb_db_summary()` shows non-empty, correctly dated rows, no 100%-NA column.

**Stage 3: `as_of()` and Wayback backfill B1 to B4.** First consumer: swap awards 157's artist lookup for `kworb_asof()` and compare to its current output on the 59th to 68th ceremonies (differences should be exactly the leak cases in 2.2).

**Stage 4: track-level.** `_songs` rotation for the awards watchlist, track-page rotation, B6 and B7, `track_daily`/`track_weekly`. Retire the old package's scrape path, fix D2 and D12 or delete the functions it superseded.

**Stage 5: housekeeping.** Remove the old `data` release (999 assets) after the new DB has the 151 chart files re-imported (they are the only record of Dec 2025 to Nov 2026 apart from Wayback), fix D18, `main` PR, projection models re-pointed at `kworb_asof()`.

---

## 5. Decisions for Pete (listed first because they gate the build)
1. **Where do raw pages live?** Private `mediadata` releases (recommended: third-party content, and `kworb` is a public repo) or the public `kworb` repo.
2. **Delete the old `data` release (999 assets) now or after import?** Recommendation: import the 151 chart CSVs and 848 track CSVs into the new tables first (cheap), then delete.
3. **Scope of watchlists** (artists and tracks): awards-only (1,500 artists) or all charted tracks (12,076)? Cost difference is Section 3.6 B6.
4. **Politeness**: 3 s per request and about 700 requests a day plus a one-off 20,000-request backfill. kworb's robots.txt allows all; there is no published rate limit that I could find. Worth a note to the site owner? (Unconfirmed: I did not look for contact details or terms of use.)

## 6. Not confirmed, and where I looked
- Why Blinding Lights' weekly table has 52 rows (kworb page only; compare with a second long-running track to see whether it is a row cap).
- Whether per-country track pages show more than 31 days anywhere (checked 2 track pages only).
- Whether `pb_upload` errors at 1,000 assets (checked: release asset list, run log; no error text found, but the log was 4,636 lines of mostly progress output and I searched for a few patterns only).
- Mechanism of D1: read from code and confirmed by 4 sampled release files; the exact overwrite order was not reproduced.
- Wayback counts for the 76 non-global country charts and for the weekly-totals page (not queried, to stay within the request budget).
- Whether DuckDB's R package behaves under Git Bash on this machine (not tested).
- kworb terms of use (not read).
