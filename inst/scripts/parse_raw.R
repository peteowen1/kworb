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

# coverage floors: share of rows with a value. Columns that are blank on the page by design (featured artists, the (xN)
# count, top-10 days for tracks that never made it) are not floored; a page change that empties a floored column fails here.
assert_coverage(ce, c("country", "frequency", "chart_date", "pos", "captured_at"),
  c(chart_date = 1, pos = 1, track_id = 0.999, title = 0.999, lead_artist_ids = 0.99, days = 0.999, peak_pos = 0.999, streams = 0.999,
    streams_7day = 0.99, streams_total = 0.99, pos_change_raw = 0.999), "chart_entry")
assert_coverage(ct, c("country", "frequency", "snapshot_date", "track_id", "captured_at"),
  c(snapshot_date = 1, covers_from = 1, track_id = 1, title = 0.999, lead_artist_ids = 0.99, days = 0.999, peak_pos = 0.999, peak_streams = 0.999, total_streams = 0.999), "chart_totals")
assert_coverage(ar, c("snapshot_date", "artist_id", "captured_at"),
  c(artist_id = 1, artist_name = 1, streams = 0.999, daily = 0.99, as_lead = 0.9, solo = 0.85, as_feature = 0.8), "artist_snapshot")
# page date = the date the capture expected: a daily chart describes the day before (or two before) its capture
lag <- as.numeric(as.Date(substr(ce$captured_at, 1, 10)) - ce$chart_date)
# Pages kworb no longer updates (9 Nov 2026: Cyprus stuck at 2017-08-05, Malta 2019-12-25, Russia 2022-04-07, the month
# Spotify left) are kept, flagged chart_frozen; anything else outside 0-3 days is a capture problem and fails the run.
ce$chart_frozen <- lag > 30
if (any(ce$chart_frozen)) { fz <- unique(ce[ce$chart_frozen, c("country", "chart_date")]); cat("frozen chart pages (kept, flagged chart_frozen):", paste(fz$country, fz$chart_date, collapse = "; "), "\n") }
bad <- lag < 0 | (lag > 3 & !ce$chart_frozen)
if (any(bad)) { st <- unique(data.frame(country = ce$country, chart_date = ce$chart_date, captured = substr(ce$captured_at, 1, 10), lag = lag)[bad, ])
  print(st[order(st$country, st$captured), ], row.names = FALSE); stop("chart_entry: ", sum(bad), " rows whose chart_date is not 0-3 days before capture (above)") }
# anchors that must hold on any day
stopifnot("global chart has 200 positions" = all(table(ce$chart_date[ce$country == "global"] , ce$captured_at[ce$country == "global"]) %in% c(0L, 200L)),
          "positions start at 1 with no gaps" = all(tapply(ce$pos, paste(ce$country, ce$captured_at), function(p) identical(sort(p), seq_along(p)))),
          "totals: total >= peak-day streams" = all(ct$total_streams >= ct$peak_streams, na.rm = TRUE))

write_month <- function(x, name, date_col) {
  x$month <- substr(x$captured_at, 1, 7)
  for (m in sort(unique(x$month))) { d <- file.path(db_dir, name, paste0("month=", m)); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    y <- x[x$month == m, setdiff(names(x), "month")]; write_parquet(y, file.path(d, "part.parquet"))
    cat(sprintf("  %-16s month=%s  %8d rows  %3d captures  %s to %s\n", name, m, nrow(y), length(unique(y$captured_at)), min(y[[date_col]]), max(y[[date_col]]))) } }
cat("writing", db_dir, "\n")
write_month(ce, "chart_entry", "chart_date"); write_month(ct, "chart_totals", "snapshot_date"); write_month(ar, "artist_snapshot", "snapshot_date")
cat(format(Sys.time(), "%H:%M:%S"), "done | chart_entry", nrow(ce), "rows,", length(unique(ce$country)), "countries,", length(unique(ce$chart_date)), "chart dates",
    "| chart_totals", nrow(ct), "| artist_snapshot", nrow(ar), "| skipped (no parser):", sum(is.na(tab)), "\n")
