#!/usr/bin/env Rscript
# Stage 4 backfill B6 (docs/plans/kworb-db.md 3.6), scope "recent only" (Pete, 15 Nov 2026): the live track page of every
# track on the global daily chart since KWORB_SINCE (default 2025-10-01, which covers the Grammy windows awards uses).
# The daily capture only fetches today's top 200, so a track that dropped off the chart before 10 Nov 2026 has no page.
#
# Which tracks: a track's `days` on the global daily Totals page only grows on days it is on the chart, so the set is
# every track whose `days` on the newest live Totals snapshot is above its `days` on the last Wayback Totals snapshot
# on or before KWORB_SINCE, plus every track missing from that old snapshot. Read from the kworb-db release
# (chart_totals partitions). 15 Nov 2026 count: 1,340 (26 Sep 2025 -> 13 Nov 2026 snapshots; 778 new since).
#
# One request per page, 3 s apart (about 67 min for 1,340), saved gzipped exactly as received, bundled into one tar plus
# manifest named like the daily ones (kworb-raw-<UTC>.tar) and uploaded to that month's kworb-raw-YYYY-MM release, so the
# nightly month rebuild parses them into track_chart / track_page_summary with no other change.
#
# Exits non-zero (after uploading whatever was captured) if a 429/403 stopped the run, more than 1% of pages failed,
# or the upload could not be verified.
#
# Environment: GH_TOKEN (write access to mediadata), MEDIADATA_REPO (default peteowen1/mediadata), KWORB_SINCE,
# KWORB_DELAY (default 3), KWORB_RAW_ROOT (default tempdir), KWORB_SKIP_UPLOAD=1 to only capture,
# KWORB_MAX (cap on pages, for a trial run; default none).

suppressMessages(library(kworb))

repo <- Sys.getenv("MEDIADATA_REPO", "peteowen1/mediadata")
since <- as.Date(Sys.getenv("KWORB_SINCE", "2025-10-01"))
if (is.na(since)) stop("KWORB_SINCE is blank or not a YYYY-MM-DD date")
delay <- as.numeric(Sys.getenv("KWORB_DELAY", "3"))
root <- Sys.getenv("KWORB_RAW_ROOT", file.path(tempdir(), "kworb_raw"))
skip_upload <- nzchar(Sys.getenv("KWORB_SKIP_UPLOAD"))
cap <- suppressWarnings(as.integer(Sys.getenv("KWORB_MAX", "100000"))); if (is.na(cap)) cap <- 100000L

started <- Sys.time()
run_ts <- format(started, "%Y%m%dT%H%M%SZ", tz = "UTC")
tag <- paste0("kworb-raw-", format(started, "%Y-%m", tz = "UTC"))
out_dir <- file.path(root, run_ts)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
base <- "https://kworb.net/spotify/"

problems <- character()
note <- function(...) { msg <- paste0(...); message("PROBLEM: ", msg); problems <<- c(problems, msg) }
gh <- function(args) {
  tmp <- tempfile()
  rc <- system2("gh", args, stdout = tmp, stderr = tmp)
  list(rc = rc, out = readLines(tmp, warn = FALSE))
}

# 1. The track list, from the kworb-db release's chart_totals partitions.
db_dir <- file.path(root, "kworb-db")
dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
assets <- gh(c("release", "view", "kworb-db", "-R", repo, "--json", "assets", "--jq", shQuote(".assets[].name")))
if (assets$rc != 0) stop("cannot list kworb-db assets: ", paste(assets$out, collapse = " "))
tot <- grep("^chart_totals__.*\\.parquet$", assets$out, value = TRUE)
part_month <- function(f) as.Date(paste0(sub("^.*__([0-9]{4}-[0-9]{2})\\.parquet$", "\\1", f), "-01"))
old_parts <- tot[grepl("__wayback__", tot) & part_month(tot) <= since]
old_parts <- old_parts[order(part_month(old_parts), decreasing = TRUE)][seq_len(min(2, length(old_parts)))]
new_parts <- tot[!grepl("__wayback__", tot)]
new_parts <- new_parts[order(part_month(new_parts), decreasing = TRUE)][seq_len(min(1, length(new_parts)))]
if (!length(old_parts) || !length(new_parts)) stop("kworb-db lacks a Wayback Totals partition on or before ", since, " or a live one")
for (f in c(old_parts, new_parts)) {
  dl <- gh(c("release", "download", "kworb-db", "-R", repo, "-p", f, "-D", shQuote(db_dir), "--clobber"))
  if (dl$rc != 0) stop("download failed for ", f, ": ", paste(dl$out, collapse = " "))
}
read_tot <- function(fs) {
  x <- dplyr::bind_rows(lapply(file.path(db_dir, fs), function(p) as.data.frame(arrow::read_parquet(p))))
  x[x$country == "global" & x$frequency == "daily", c("track_id", "days", "snapshot_date")]
}
old <- read_tot(old_parts); new <- read_tot(new_parts)
old_d <- max(old$snapshot_date[old$snapshot_date <= since]); new_d <- max(new$snapshot_date)
old <- unique(old[old$snapshot_date == old_d, c("track_id", "days")]); new <- unique(new[new$snapshot_date == new_d, c("track_id", "days")])
if (anyDuplicated(old$track_id) || anyDuplicated(new$track_id)) stop("duplicate track ids within one Totals snapshot")
if (nrow(old) < 10000 || nrow(new) < 10000) stop("Totals snapshot too small (old ", nrow(old), ", new ", nrow(new), "): truncated page?")
m <- merge(new, old, by = "track_id", all.x = TRUE, suffixes = c("_new", "_old"))
if (any(m$days_new < m$days_old, na.rm = TRUE)) message("note: ", sum(m$days_new < m$days_old, na.rm = TRUE), " tracks have fewer days now than then")
ids <- m$track_id[is.na(m$days_old) | m$days_new > m$days_old]
ids <- ids[!is.na(ids) & grepl("^[0-9A-Za-z]{22}$", ids)]
message(sprintf("Totals %s (%d tracks) -> %s (%d tracks): %d charted in between (%d new); fetching %d",
                old_d, nrow(old), new_d, nrow(new), length(ids), sum(is.na(m$days_old)), min(cap, length(ids))))
if (length(ids) < 500) note("only ", length(ids), " tracks charted since ", old_d, " (expected about 1,300)")
ids_path <- file.path(root, paste0("backfill-tracks-ids-", run_ts, ".txt"))   # uploaded with the tar, so a partial run can be audited
writeLines(ids, ids_path)

# 2. Track pages.
urls <- paste0(base, "track/", head(ids, cap), ".html")
tm <- fetch_raw_pages(urls, out_dir, delay = delay)
if (isTRUE(attr(tm, "stopped"))) note("stopped by HTTP 429/403; not attempted: ", length(attr(tm, "not_attempted")))
bad <- tm[is.na(tm$http_status) | tm$http_status != 200L, ]
if (nrow(bad)) message("track pages not 200: ", nrow(bad), " (", paste(head(basename(bad$url), 5), collapse = ", "), ")")
if (nrow(bad) > max(5, 0.01 * length(urls))) note(nrow(bad), " of ", length(urls), " track pages failed")

# 3. Bundle and upload (always attempted, even after page failures), as capture_raw.R does.
manifest <- utils::read.csv(file.path(out_dir, "manifest.csv"), stringsAsFactors = FALSE)
message(sprintf("%d requests, %d ok", nrow(manifest), sum(manifest$http_status %in% 200L)))
tar_name <- paste0("kworb-raw-", run_ts, ".tar")
man_name <- paste0("kworb-raw-manifest-", run_ts, ".csv")
tar_path <- file.path(root, tar_name)
man_path <- file.path(root, man_name)
invisible(file.copy(file.path(out_dir, "manifest.csv"), man_path, overwrite = TRUE))
rc <- system2("tar", c("-cf", shQuote(tar_path), "-C", shQuote(out_dir), "."))
if (rc != 0 || !file.exists(tar_path)) note("tar failed")
message(sprintf("Bundle %s: %d page files, %.1f MB", tar_name,
                length(list.files(out_dir, pattern = "\\.gz$")), file.size(tar_path) / 1e6))

if (skip_upload) {
  message("KWORB_SKIP_UPLOAD set; not uploading")
} else if (file.exists(tar_path)) {
  if (gh(c("release", "view", tag, "-R", repo))$rc != 0) {
    cr <- gh(c("release", "create", tag, "-R", repo, "--title", shQuote(tag),
               "--notes", shQuote("Raw kworb.net page captures (private). One tar plus manifest per run."), "--latest=false"))
    if (cr$rc != 0 && gh(c("release", "view", tag, "-R", repo))$rc != 0) note("could not create release ", tag)
  }
  up <- gh(c("release", "upload", tag, shQuote(tar_path), shQuote(man_path), shQuote(ids_path), "-R", repo))
  if (up$rc != 0) note("upload failed: ", paste(up$out, collapse = " "))
  chk <- gh(c("release", "view", tag, "-R", repo, "--json", "assets", "--jq", shQuote(".assets[] | [.name, .size] | @tsv")))
  listed <- if (chk$rc == 0) utils::read.delim(text = chk$out, header = FALSE, col.names = c("name", "size")) else NULL
  if (is.null(listed) || !all(c(tar_name, man_name) %in% listed$name)) {
    note("uploaded assets not found on release ", tag)
  } else if (listed$size[listed$name == tar_name] != file.size(tar_path)) {
    note("uploaded tar size differs from local")
  } else {
    message(sprintf("Uploaded to https://github.com/%s/releases/tag/%s : %s (%.1f MB)", repo, tag, tar_name, file.size(tar_path) / 1e6))
  }
}

if (length(problems)) {
  message("\nBACKFILL FAILED with ", length(problems), " problem(s):")
  for (p in problems) message(" - ", p)
  quit(status = 1)
}
message("Backfill OK: ", nrow(manifest), " requests, ", sum(manifest$http_status %in% 200L), " returned 200")
