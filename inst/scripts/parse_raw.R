# Stage 2 of docs/plans/kworb-db.md: parse every saved raw capture into the database tables.
#
#   Rscript inst/scripts/parse_raw.R [raw_dir] [db_dir]
#   defaults: data/raw  data/db   (run from the package root; arrow segfaults under Git Bash R, so on Windows run it
#   through PowerShell: powershell.exe -Command "Rscript 'inst/scripts/parse_raw.R'")
#
# Tables (one parquet per table per CAPTURE month, hive style: <db>/<table>/month=YYYY-MM/part.parquet; by capture month, not by
# the day the numbers describe, so a closed month never changes and frozen pages (a 2017 chart captured today) stay in today's month):
#   chart_entry      daily top-200 charts, 77 countries   key country, frequency, chart_date, pos, captured_at
#   chart_totals     global daily Totals page            key country, frequency, snapshot_date, track_id, captured_at
#   artist_snapshot  artists.html (no page date: snapshot_date is the capture's UTC date)   key snapshot_date, artist_id, captured_at
#   track_chart      track pages: one row per track x chart (frequency, country) x chart_date, COLLAPSED over the month's
#                    captures (a page repeats its whole weekly history every day: 277k of 409k cells, 11 Nov). Kept: the
#                    first and last capture that showed the row, n_obs, and the position / streams of each. An as-of query
#                    uses first_captured_at <= date and the *_first values. A row seen in two months is in both months'
#                    files; take the earliest first_captured_at.   key track_id, frequency, country, chart_date
#   artist_totals    artist songs pages (artist/<id>_songs.html, weekly rotation): the artist's Streams / Daily / Tracks totals,
#                    Total, As lead, Solo, As feature; one row per capture.   key artist_id, captured_at
#   artist_song_snapshot  the same pages' song tables: exact all-time streams and daily per track, top 500 by streams
#                    (song_table_truncated on artist_totals when the artist has more tracks).   key artist_id, track_id, captured_at
#   track_page_summary  the Total and Peak rows of every track page capture (Total grows daily, so every capture is kept)
#                    key track_id, frequency, country, captured_at. The Global weekly Total is NOT the sum of the weekly rows
#                    on 154 of 200 pages (11 Nov; always larger, median 5%): kept as the page states it.
# Every capture is kept (captured_at is part of the key): a page captured twice gives two copies, so an as-of query can
# drop anything captured after its date. Each month present in raw_dir is rewritten from its raw files, so the output is a pure
# function of data/raw; the daily job passes only the current month's captures. Coverage is asserted per table before anything is written: a renamed column upstream fails the run.
suppressPackageStartupMessages({ library(arrow) })
if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".", quiet = TRUE) else library(kworb)   # CI: the installed package
args <- commandArgs(trailingOnly = TRUE)
raw_dir <- if (length(args) >= 1) args[1] else "data/raw"; db_dir <- if (length(args) >= 2) args[2] else "data/db"
files <- sort(list.files(raw_dir, "\\.html\\.gz$", full.names = TRUE, recursive = TRUE))
cat(format(Sys.time(), "%H:%M:%S"), "raw files:", length(files), "in", raw_dir, "\n")
if (!length(files)) stop("no raw files")
# Each file's parsed rows are cached as <db>/_parsed/<raw file>__v<parser version>.parquet, written as soon as the file is
# parsed: a rerun (or a run killed part way) reads them back instead of re-parsing (old artists pages hold 10,000 rows; the
# 741 Wayback artists / totals copies took 9.5 min). A new parser version parses everything again.
cache_dir <- file.path(db_dir, "_parsed"); dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
TABLE_OF <- function(f) { s <- sub("__.*$", "", basename(f)); if (grepl("^country-[a-z]+_(daily|weekly)_totals$", s)) "chart_totals" else if (grepl("^country-[a-z]+_(daily|weekly)$", s)) "chart_entry" else if (s == "artists") "artist_snapshot" else if (grepl("^track-[A-Za-z0-9]{22}$", s)) "track_chart" else if (grepl("^artist-[A-Za-z0-9]{22}_songs$", s)) "artist_songs" else NA_character_ }
parse_cached <- function(f) {
  cp <- file.path(cache_dir, paste0(sub("\\.html\\.gz$", "", basename(f)), "__v", kworb:::kworb_parser_version(), ".parquet"))
  if (file.exists(cp)) return(list(table = TABLE_OF(f), rows = as.data.frame(read_parquet(cp))))
  r <- parse_raw_file(f); if (!is.na(r$table)) { tmp <- paste0(cp, ".tmp"); write_parquet(r$rows, tmp); file.rename(tmp, cp) }; r }   # atomic: a kill mid-write leaves no corrupt cache file
# track pages (about 2,000 cells each, 200 a day) are parsed and collapsed a capture day at a time at the end, so a month of
# them is never held in memory at once (12 million cells by the 31st)
track_files <- files[grepl("^track-", basename(files))]; files <- setdiff(files, track_files)
song_files <- files[grepl("^artist-[A-Za-z0-9]{22}_songs__", basename(files))]; files <- setdiff(files, song_files)   # artist songs pages: own section at the end
t0 <- Sys.time(); res <- lapply(files, function(f) tryCatch(parse_cached(f), error = function(e) list(table = "ERROR", rows = NULL, error = conditionMessage(e), file = basename(f))))
tab <- vapply(res, `[[`, "", "table")
err <- res[tab %in% "ERROR"]
cat(format(Sys.time(), "%H:%M:%S"), "parsed in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min | by table:\n"); print(table(tab, useNA = "ifany"))
if (length(err)) { for (e in err) message("PARSE ERROR ", e$file, ": ", e$error); stop(length(err), " of ", length(files), " files failed to parse") }

bind <- function(name) { r <- res[tab %in% name]; if (!length(r)) return(NULL)
  cols <- unique(unlist(lapply(r, function(z) names(z$rows))))   # layouts add columns: fill the missing ones with NA
  proto <- list(); for (z in r) for (v in setdiff(names(z$rows), names(proto))) proto[[v]] <- z$rows[[v]][NA_integer_][1]   # a typed NA per column
  x <- do.call(rbind, lapply(r, function(z) { y <- z$rows; for (v in setdiff(cols, names(y))) y[[v]] <- rep(proto[[v]], nrow(y)); y[cols] })); rownames(x) <- NULL; x }
ce <- bind("chart_entry"); ct <- bind("chart_totals"); ar <- bind("artist_snapshot")
# a page type with no captures here (a Wayback set without daily charts) is skipped and said so; all three empty is an error
present <- c(chart_entry = !is.null(ce), chart_totals = !is.null(ct), artist_snapshot = !is.null(ar))
if (!any(present)) stop("no parsable captures of any page type in ", raw_dir)
# the daily live job always has all three: a missing one there is a failure (round-2 review). Only a run that sets
# KWORB_ALLOW_MISSING=1 (the Wayback archive parse) may skip a page type.
if (!all(present) && !identical(Sys.getenv("KWORB_ALLOW_MISSING"), "1")) stop("no captures of ", paste(names(present)[!present], collapse = ", "), " in ", raw_dir, " (set KWORB_ALLOW_MISSING=1 to skip)")
if (!all(present)) cat("NOTE: no captures of", paste(names(present)[!present], collapse = ", "), "in", raw_dir, "- those tables are skipped\n")

# Checks run PER CAPTURE (one raw file), not per month: a capture that fails is left out of the tables and listed in
# <db>/_quarantine.csv with its reason, so one bad page cannot block every later day of the month (the daily job rebuilds the
# whole month). The run fails only if a capture from the NEWEST capture day fails: that day's job goes red once, later days
# publish without it. Coverage floors: share of rows with a value; columns blank on the page by design (featured artists,
# the (xN) count, top-10 days for tracks that never made it) are not floored.
FL <- list(
  chart_entry = c(chart_date = 1, pos = 1, track_id = 0.99, title = 0.99, lead_artist_ids = 0.98, days = 0.99, peak_pos = 0.99, streams = 0.99,
                  streams_7day = 0.98, streams_total = 0.98, pos_change_raw = 0.99),
  chart_totals = c(snapshot_date = 1, covers_from = 1, track_id = 1, title = 0.999, lead_artist_ids = 0.99, days = 0.999, peak_pos = 0.999, peak_streams = 0.999, total_streams = 0.999),
  artist_snapshot = c(artist_id = 1, artist_name = 1, streams = 0.999, daily = 0.99, as_lead = 0.9, solo = 0.85, as_feature = 0.8))
KEY <- list(chart_entry = c("country", "frequency", "chart_date", "pos", "captured_at"), chart_totals = c("country", "frequency", "snapshot_date", "track_id", "captured_at"),
            artist_snapshot = c("snapshot_date", "artist_id", "captured_at"))
# a daily chart describes the day before (or two) its capture; capture_raw.R accepts up to 4 days, so 0-4 is fine here too.
# Pages kworb no longer updates (9 Nov 2026: Cyprus stuck at 2017-08-05, Malta 2019-12-25, Russia 2022-04-07, the month
# Spotify left) are kept and flagged chart_frozen.
if (!is.null(ce)) { ce$chart_lag_days <- as.numeric(as.Date(substr(ce$captured_at, 1, 10)) - ce$chart_date); ce$chart_frozen <- ce$chart_lag_days > 30 }
if (!is.null(ce) && any(ce$chart_frozen)) { fz <- unique(ce[ce$chart_frozen, c("country", "chart_date")]); cat("frozen chart pages (kept, flagged chart_frozen):", paste(fz$country, fz$chart_date, collapse = "; "), "\n") }
# floors follow the page layout: the artists page before May 2023 has no Solo column (a5), and before Apr 2023 only
# Pos | Artist | Total Streams (a3_chart), so those columns are NA by design and must not be floored (review 9 Nov)
floors_for <- function(x, name) { f <- FL[[name]]
  if (name == "chart_entry" && identical(x$frequency[1], "weekly")) f <- c(f[setdiff(names(f), c("days", "streams_7day"))], weeks = 0.99)   # weekly pages: weeks, no 7-day columns
  if (name == "chart_totals" && identical(x$frequency[1], "weekly")) f <- c(f[names(f) != "days"], weeks = 0.999)   # weekly totals count weeks, not days
  if (name == "artist_snapshot" && "layout" %in% names(x)) { lay <- x$layout[1]
    if (lay == "a5") f <- f[names(f) != "solo"]
    if (lay == "a3_chart") f <- f[c("artist_id", "artist_name", "streams")]
    # spring 2023 the page filled its new columns in gradually (Wayback, 9 Nov: daily 28% on 5 Apr, full from 28 Apr; Solo
    # 27% -> 87% over 7-12 May): real blanks on the page, so those columns are floored only after they filled in
    d <- substr(x$captured_at[1], 1, 10); if (d < "2023-04-28") f <- f[setdiff(names(f), c("daily", "as_lead", "as_feature"))]; if (d < "2023-05-12") f <- f[names(f) != "solo"] }
  f }
check_capture <- function(x, name) {
  why <- tryCatch({ assert_coverage(x, KEY[[name]], floors_for(x, name), name); "" }, error = function(e) conditionMessage(e))
  if (nzchar(why)) return(why)
  if (name == "chart_entry") {
    lg <- x$chart_lag_days[1]; mx <- if (x$frequency[1] == "weekly") 14 else 4   # a weekly page is up to a week older than its capture
    if (lg < 0 || (lg > mx && !x$chart_frozen[1])) return(sprintf("%s chart_date %s is %d days before capture (allowed 0-%d, or frozen > 30)", x$frequency[1], x$chart_date[1], lg, mx))
    if (!identical(sort(x$pos), seq_along(x$pos))) return("positions do not run 1..n with no gaps")
    if (x$country[1] == "global" && nrow(x) != 200) return(sprintf("global chart has %d positions, not 200", nrow(x))) }
  if (name == "chart_totals" && any(x$total_streams < x$peak_streams, na.rm = TRUE)) return("a total below its peak-day streams")
  "" }
quar <- list(); keep <- function(x, name) { if (is.null(x)) return(NULL)
  s <- split(x, x$raw_file); ok <- character()
  for (f in names(s)) { w <- check_capture(s[[f]], name); if (nzchar(w)) quar[[length(quar) + 1]] <<- data.frame(table = name, raw_file = f, captured_at = s[[f]]$captured_at[1], reason = w) else ok <- c(ok, f) }
  x[x$raw_file %in% ok, ] }
ce <- keep(ce, "chart_entry"); ct <- keep(ct, "chart_totals"); ar <- keep(ar, "artist_snapshot")
for (nm in names(KEY)) if (present[[nm]]) assert_coverage(get(c(chart_entry = "ce", chart_totals = "ct", artist_snapshot = "ar")[[nm]]), KEY[[nm]], if (nm == "artist_snapshot") FL[[nm]][c("artist_id", "artist_name", "streams")] else if (nm %in% c("chart_entry", "chart_totals")) FL[[nm]][setdiff(names(FL[[nm]]), c("days", "streams_7day"))] else FL[[nm]], nm)   # per-capture checks cover the layout-specific columns   # the kept rows, pooled (duplicate keys across captures)
dir.create(db_dir, recursive = TRUE, showWarnings = FALSE); qf <- file.path(db_dir, "_quarantine.csv")
if (length(quar)) { q <- do.call(rbind, quar); write.csv(q, qf, row.names = FALSE)
  if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) for (i in seq_len(nrow(q))) cat(sprintf("::warning title=kworb capture quarantined::%s %s: %s\n", q$table[i], q$raw_file[i], q$reason[i]))   # visible on a green run
cat("QUARANTINED", nrow(q), "capture(s), left out of the tables (", qf, "):\n"); print(q, row.names = FALSE)
  newest <- max(substr(c(ce$captured_at, ct$captured_at, ar$captured_at, q$captured_at), 1, 10))
  if (any(substr(q$captured_at, 1, 10) == newest)) stop("a capture from the newest day (", newest, ") failed its checks: see above") } else if (file.exists(qf)) file.remove(qf)

write_month <- function(x, name, date_col, month_col = "captured_at") { if (!NROW(x)) return(invisible())
  x$month <- substr(x[[month_col]], 1, 7); if (month_col != "captured_at") x[[month_col]] <- NULL
  for (m in sort(unique(x$month))) { d <- file.path(db_dir, name, paste0("month=", m)); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    y <- x[x$month == m, setdiff(names(x), "month")]; write_parquet(y, file.path(d, "part.parquet"))
    cat(sprintf("  %-16s month=%s  %8d rows  %3d captures  %s to %s\n", name, m, nrow(y), length(unique(if ("captured_at" %in% names(y)) y$captured_at else y$last_captured_at)), min(y[[date_col]]), max(y[[date_col]]))) } }
cat("writing", db_dir, "\n")
write_month(ce, "chart_entry", "chart_date"); write_month(ct, "chart_totals", "snapshot_date"); write_month(ar, "artist_snapshot", "snapshot_date")
cat(format(Sys.time(), "%H:%M:%S"), "done | chart_entry", nrow(ce), "rows,", length(unique(ce$country)), "countries,", length(unique(ce$chart_date)), "chart dates",
    "| chart_totals", nrow(ct), "| artist_snapshot", nrow(ar), "| skipped (no parser):", sum(is.na(tab)), "\n")

# ---- track pages -> track_chart (collapsed over captures) and track_page_summary (every capture) ----
# Per-capture checks (a failing page is quarantined like any other capture; the run fails only for the newest day):
# every filled cell parses, the weekly Peak equals the best weekly position on each chart (200 of 200 pages, 11 Nov),
# no chart date after the capture, positions 1-200. A missing track-page set is a note, not a failure (KWORB_TRACKS=0).
check_track <- function(x) {
  why <- tryCatch({ assert_coverage(x, c("frequency", "row_type", "country", "chart_date"), c(track_id = 1, title = 1, artist_ids = 1, country = 1, streams = 0.99), "track_chart"); "" }, error = function(e) conditionMessage(e))
  if (nzchar(why)) return(why)
  if (any(x$cell_odd)) return(sprintf("%d cells do not read as 'position (streams)', e.g. '%s'", sum(x$cell_odd), x$cell_raw[x$cell_odd][1]))
  d <- x[x$row_type == "date", ]
  if (anyNA(d$chart_date)) return("a date row whose label is not a real date")
  if (any(d$chart_date > as.Date(substr(x$captured_at[1], 1, 10)))) return("a chart date after the capture")
  # weekly charts of 22 Mar - 30 Jul 2015 ran past 200 (to 329; 629 cells on 200 pages, 11 Nov): allowed there only
  big <- d$position > 200 & !(d$frequency == "weekly" & d$chart_date < as.Date("2015-08-01"))
  if (any(d$position < 1 | big, na.rm = TRUE)) return("a position outside 1-200 (outside the long weekly charts of 2015)")
  w <- x[x$frequency == "weekly", ]
  if (nrow(w)) { pk <- w[w$row_type == "peak", c("country", "position")]; best <- tapply(w$position[w$row_type == "date"], w$country[w$row_type == "date"], min)
    if (nrow(pk) && any(pk$position != best[pk$country], na.rm = TRUE)) return("a weekly Peak is not the best weekly position") }
  "" }
if (length(track_files)) {
  t1 <- Sys.time(); day <- substr(sub("^.*__([0-9]{8})T.*$", "\\1", basename(track_files)), 1, 8)
  acc <- NULL; summ <- list(); n_ok <- 0L; tquar <- list()
  ID <- c("track_id", "frequency", "country", "chart_date", "cap_month")   # collapsed within a capture month, so each month's file stands alone
  for (dd in sort(unique(day))) {
    r <- lapply(track_files[day == dd], function(f) tryCatch(parse_cached(f), error = function(e) list(table = "ERROR", error = conditionMessage(e), file = basename(f))))
    # a page that does not parse is quarantined like one that fails its checks (review 11 Nov): one odd page among ~6,000 a
    # month must not block every later rebuild; only more than 5 bad pages on the newest day fail the run
    for (e in Filter(function(z) identical(z$table, "ERROR"), r)) { message("PARSE ERROR ", e$file, ": ", e$error)
      tquar[[length(tquar) + 1]] <- data.frame(table = "track_chart", raw_file = e$file, captured_at = format(as.POSIXct(sub("^.*__([0-9]{8}T[0-9]{6}Z)\\.html\\.gz$", "\\1", e$file), format = "%Y%m%dT%H%M%SZ", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), reason = paste("parse error:", e$error)) }
    r <- Filter(function(z) !identical(z$table, "ERROR"), r)
    ok <- list()
    for (z in r) { w <- check_track(z$rows); if (nzchar(w)) tquar[[length(tquar) + 1]] <- data.frame(table = "track_chart", raw_file = z$rows$raw_file[1], captured_at = z$rows$captured_at[1], reason = w) else ok[[length(ok) + 1]] <- z$rows }
    if (!length(ok)) next
    x <- do.call(rbind, ok); n_ok <- n_ok + length(ok)
    s <- x[x$row_type != "date", ]
    tot <- s[s$row_type == "total", c("track_id", "title", "artist_ids", "artist_names", "frequency", "country", "streams", "captured_at", "source", "raw_file", "parser_version", "layout", "page_truncated")]
    names(tot)[names(tot) == "streams"] <- "total_streams"
    pk <- s[s$row_type == "peak", c("track_id", "frequency", "country", "captured_at", "position", "streams")]; names(pk)[5:6] <- c("peak_pos", "peak_streams")
    summ[[dd]] <- merge(tot, pk, by = c("track_id", "frequency", "country", "captured_at"), all = TRUE)
    x$cap_month <- substr(x$captured_at, 1, 7)
    d <- x[x$row_type == "date", c(ID, "title", "artist_ids", "artist_names", "position", "streams", "captured_at")]
    d$first_captured_at <- d$last_captured_at <- d$captured_at; d$position_first <- d$position_last <- d$position; d$streams_first <- d$streams_last <- d$streams; d$n_obs <- 1L
    d <- d[, setdiff(names(d), c("position", "streams", "captured_at"))]
    acc <- if (is.null(acc)) d else rbind(acc, d)
    # collapse: first values from the earliest capture, last values (and the names) from the latest, n_obs summed
    o <- order(acc$track_id, acc$frequency, acc$country, acc$chart_date, acc$cap_month, acc$first_captured_at); acc <- acc[o, ]
    k <- do.call(paste, c(acc[ID], sep = "\r")); first <- !duplicated(k)
    o2 <- order(k, acc$last_captured_at, decreasing = TRUE); lastrow <- o2[!duplicated(k[o2])]; names(lastrow) <- k[lastrow]
    nobs <- tapply(acc$n_obs, k, sum)
    f <- acc[first, ]; kf <- k[first]; L <- acc[lastrow[kf], ]
    for (v in c("last_captured_at", "position_last", "streams_last", "title", "artist_ids", "artist_names")) f[[v]] <- L[[v]]
    f$n_obs <- as.integer(nobs[kf]); acc <- f; rownames(acc) <- NULL
    cat(sprintf("  track pages %s: %d ok, track_chart %d rows so far\n", dd, length(ok), nrow(acc)))
  }
  if (length(tquar)) { q2 <- do.call(rbind, tquar); write.csv(rbind(if (file.exists(qf)) read.csv(qf) else NULL, q2), qf, row.names = FALSE)
    if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) for (i in seq_len(nrow(q2))) cat(sprintf("::warning title=kworb capture quarantined::%s %s: %s\n", q2$table[i], q2$raw_file[i], q2$reason[i]))
    cat("QUARANTINED", nrow(q2), "track page(s):\n"); print(q2, row.names = FALSE)
    newest <- max(day); n_new <- sum(gsub("-", "", substr(q2$captured_at, 1, 10)) == newest)
    if (n_new > 5) stop(n_new, " track pages from the newest day (", newest, ") failed their checks: see above") }   # a few dead pages are not a failed day (capture_raw.R uses the same 5)
  if (is.null(acc)) stop("no track page passed its checks")
  sm <- do.call(rbind, summ); rownames(sm) <- NULL
  assert_coverage(acc, ID, c(track_id = 1, country = 1, chart_date = 1, streams_first = 0.99, position_first = 0.99, first_captured_at = 1), "track_chart")
  assert_coverage(sm, c("track_id", "frequency", "country", "captured_at"), c(total_streams = 0.99, peak_pos = 0.99, peak_streams = 0.99), "track_page_summary")
  write_month(acc, "track_chart", "chart_date", "cap_month"); write_month(sm, "track_page_summary", "captured_at")
  cat(format(Sys.time(), "%H:%M:%S"), "track pages:", n_ok, "of", length(track_files), "kept | track_chart", nrow(acc), "rows | track_page_summary", nrow(sm), "rows |",
      round(as.numeric(difftime(Sys.time(), t1, units = "mins")), 1), "min\n")
} else cat("NOTE: no track pages in", raw_dir, "- track_chart / track_page_summary skipped\n")

# ---- artist songs pages -> artist_totals (one row per capture) and artist_song_snapshot (one row per song per capture) ----
# Per-capture checks, quarantined like the track pages (the run fails only if more than 5 pages of the newest day fail): every
# song row has a track id, a title and streams; streams run highest first (the page's own order); the page date is not after
# the capture; the artist totals are filled. A missing set is a note (the rotation may not have run).
TOT_COLS <- c("artist_id", "artist_name", "page_updated", "snapshot_date", "captured_at",
  paste0("artist_", rep(c("streams", "daily", "tracks"), each = 4), "_", c("total", "as_lead", "solo", "as_feature")),
  "song_rows", "song_table_truncated", "layout", "page_truncated", "source", "raw_file", "parser_version")
SONG_COLS <- c("artist_id", "snapshot_date", "captured_at", "rank", "track_id", "title", "is_feature", "title_cell", "streams", "daily", "raw_file")
check_songs <- function(x) {
  why <- tryCatch({ assert_coverage(x, c("artist_id", "track_id", "captured_at"), c(track_id = 1, title = 1, streams = 1, daily = 0.99, artist_streams_total = 1, artist_tracks_total = 1), "artist_song_snapshot"); "" }, error = function(e) conditionMessage(e))
  if (nzchar(why)) return(why)
  if (is.unsorted(rev(x$streams))) return("song streams are not highest first")
  if (!is.na(x$page_updated[1]) && x$page_updated[1] > as.Date(substr(x$captured_at[1], 1, 10))) return("page date after the capture")
  if (sum(x$streams) > x$artist_streams_total[1] * 1.001) return("the songs add up to more than the artist's total")
  "" }
if (length(song_files)) {
  t2 <- Sys.time(); tots <- list(); songs <- list(); squar <- list()
  for (f in song_files) {
    z <- tryCatch(parse_cached(f), error = function(e) list(table = "ERROR", error = conditionMessage(e)))
    if (identical(z$table, "ERROR")) { message("PARSE ERROR ", basename(f), ": ", z$error)
      squar[[length(squar) + 1]] <- data.frame(table = "artist_song_snapshot", raw_file = basename(f), captured_at = format(as.POSIXct(sub("^.*__([0-9]{8}T[0-9]{6}Z)\\.html\\.gz$", "\\1", basename(f)), format = "%Y%m%dT%H%M%SZ", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), reason = paste("parse error:", z$error)); next }
    w <- check_songs(z$rows)
    if (nzchar(w)) { squar[[length(squar) + 1]] <- data.frame(table = "artist_song_snapshot", raw_file = basename(f), captured_at = z$rows$captured_at[1], reason = w); next }
    tots[[length(tots) + 1]] <- z$rows[1, TOT_COLS]; songs[[length(songs) + 1]] <- z$rows[, SONG_COLS]   # slim: the totals once per capture, not on every song
  }
  if (length(squar)) { q3 <- do.call(rbind, squar); write.csv(rbind(if (file.exists(qf)) read.csv(qf) else NULL, q3), qf, row.names = FALSE)
    if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) for (i in seq_len(nrow(q3))) cat(sprintf("::warning title=kworb capture quarantined::%s %s: %s\n", q3$table[i], q3$raw_file[i], q3$reason[i]))
    cat("QUARANTINED", nrow(q3), "artist songs page(s):\n"); print(q3, row.names = FALSE)
    newest <- max(substr(sub("^.*__([0-9]{8})T.*$", "\\1", basename(song_files)), 1, 8)); n_new <- sum(gsub("-", "", substr(q3$captured_at, 1, 10)) == newest)
    if (n_new > 5) stop(n_new, " artist songs pages from the newest day (", newest, ") failed their checks: see above") }
  if (!length(tots)) stop("no artist songs page passed its checks")
  at <- do.call(rbind, tots); ss <- do.call(rbind, songs); rownames(at) <- rownames(ss) <- NULL
  assert_coverage(at, c("artist_id", "captured_at"), c(artist_name = 1, artist_streams_total = 1, artist_tracks_total = 1, page_updated = 0.99), "artist_totals")
  assert_coverage(ss, c("artist_id", "track_id", "captured_at"), c(track_id = 1, title = 1, streams = 1, daily = 0.99), "artist_song_snapshot")
  write_month(at, "artist_totals", "snapshot_date"); write_month(ss, "artist_song_snapshot", "snapshot_date")
  cat(format(Sys.time(), "%H:%M:%S"), "artist songs pages:", nrow(at), "of", length(song_files), "kept | artist_song_snapshot", nrow(ss), "rows |",
      round(as.numeric(difftime(Sys.time(), t2, units = "mins")), 1), "min\n")
} else cat("NOTE: no artist songs pages in", raw_dir, "- artist_totals / artist_song_snapshot skipped\n")
