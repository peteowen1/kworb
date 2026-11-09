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
t0 <- Sys.time(); res <- lapply(files, function(f) tryCatch(parse_raw_file(f), error = function(e) list(table = "ERROR", rows = NULL, error = conditionMessage(e), file = basename(f))))
tab <- vapply(res, `[[`, "", "table")
err <- res[tab %in% "ERROR"]
cat(format(Sys.time(), "%H:%M:%S"), "parsed in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min | by table:\n"); print(table(tab, useNA = "ifany"))
if (length(err)) { for (e in err) message("PARSE ERROR ", e$file, ": ", e$error); stop(length(err), " of ", length(files), " files failed to parse") }

bind <- function(name) { x <- do.call(rbind, lapply(res[tab %in% name], `[[`, "rows")); rownames(x) <- NULL; x }
ce <- bind("chart_entry"); ct <- bind("chart_totals"); ar <- bind("artist_snapshot")

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
ce$chart_lag_days <- as.numeric(as.Date(substr(ce$captured_at, 1, 10)) - ce$chart_date); ce$chart_frozen <- ce$chart_lag_days > 30
if (any(ce$chart_frozen)) { fz <- unique(ce[ce$chart_frozen, c("country", "chart_date")]); cat("frozen chart pages (kept, flagged chart_frozen):", paste(fz$country, fz$chart_date, collapse = "; "), "\n") }
check_capture <- function(x, name) {
  why <- tryCatch({ assert_coverage(x, KEY[[name]], FL[[name]], name); "" }, error = function(e) conditionMessage(e))
  if (nzchar(why)) return(why)
  if (name == "chart_entry") {
    lg <- x$chart_lag_days[1]
    if (lg < 0 || (lg > 4 && !x$chart_frozen[1])) return(sprintf("chart_date %s is %d days before capture (allowed 0-4, or frozen > 30)", x$chart_date[1], lg))
    if (!identical(sort(x$pos), seq_along(x$pos))) return("positions do not run 1..n with no gaps")
    if (x$country[1] == "global" && nrow(x) != 200) return(sprintf("global chart has %d positions, not 200", nrow(x))) }
  if (name == "chart_totals" && any(x$total_streams < x$peak_streams, na.rm = TRUE)) return("a total below its peak-day streams")
  "" }
quar <- list(); keep <- function(x, name) { if (is.null(x) || !nrow(x)) stop(name, ": no rows (no captures of this page type in ", raw_dir, ")")
  s <- split(x, x$raw_file); ok <- character()
  for (f in names(s)) { w <- check_capture(s[[f]], name); if (nzchar(w)) quar[[length(quar) + 1]] <<- data.frame(table = name, raw_file = f, captured_at = s[[f]]$captured_at[1], reason = w) else ok <- c(ok, f) }
  x[x$raw_file %in% ok, ] }
ce <- keep(ce, "chart_entry"); ct <- keep(ct, "chart_totals"); ar <- keep(ar, "artist_snapshot")
for (nm in names(KEY)) assert_coverage(get(c(chart_entry = "ce", chart_totals = "ct", artist_snapshot = "ar")[[nm]]), KEY[[nm]], FL[[nm]], nm)   # the kept rows, pooled (duplicate keys across captures)
dir.create(db_dir, recursive = TRUE, showWarnings = FALSE); qf <- file.path(db_dir, "_quarantine.csv")
if (length(quar)) { q <- do.call(rbind, quar); write.csv(q, qf, row.names = FALSE)
  if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) for (i in seq_len(nrow(q))) cat(sprintf("::warning title=kworb capture quarantined::%s %s: %s\n", q$table[i], q$raw_file[i], q$reason[i]))   # visible on a green run
cat("QUARANTINED", nrow(q), "capture(s), left out of the tables (", qf, "):\n"); print(q, row.names = FALSE)
  newest <- max(substr(c(ce$captured_at, ct$captured_at, ar$captured_at, q$captured_at), 1, 10))
  if (any(substr(q$captured_at, 1, 10) == newest)) stop("a capture from the newest day (", newest, ") failed its checks: see above") } else if (file.exists(qf)) file.remove(qf)

write_month <- function(x, name, date_col) {
  x$month <- substr(x$captured_at, 1, 7)
  for (m in sort(unique(x$month))) { d <- file.path(db_dir, name, paste0("month=", m)); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    y <- x[x$month == m, setdiff(names(x), "month")]; write_parquet(y, file.path(d, "part.parquet"))
    cat(sprintf("  %-16s month=%s  %8d rows  %3d captures  %s to %s\n", name, m, nrow(y), length(unique(y$captured_at)), min(y[[date_col]]), max(y[[date_col]]))) } }
cat("writing", db_dir, "\n")
write_month(ce, "chart_entry", "chart_date"); write_month(ct, "chart_totals", "snapshot_date"); write_month(ar, "artist_snapshot", "snapshot_date")
cat(format(Sys.time(), "%H:%M:%S"), "done | chart_entry", nrow(ce), "rows,", length(unique(ce$country)), "countries,", length(unique(ce$chart_date)), "chart dates",
    "| chart_totals", nrow(ct), "| artist_snapshot", nrow(ar), "| skipped (no parser):", sum(is.na(tab)), "\n")
