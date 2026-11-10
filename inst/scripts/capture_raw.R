#!/usr/bin/env Rscript
# Stage 1 raw capture (docs/plans/kworb-db.md).
#
# Fetches the kworb index page (https://kworb.net/spotify/), then the daily chart of every country the index
# lists, global_daily_totals, the global weekly chart and weekly totals, artists.html, then the track page of every song on
# today's global daily chart (KWORB_TRACKS, default 200; 0 = none). One request per page, 3 s apart,
# bodies saved gzipped exactly as received. The day's files plus the manifest are
# bundled into one tar and uploaded to a monthly release of the PRIVATE
# peteowen1/mediadata repo (tag kworb-raw-YYYY-MM).
#
# Exits non-zero (after uploading whatever was captured) if any page failed, a
# 429/403 stopped the run, or the upload could not be verified.
#
# Environment: GH_TOKEN (needs write access to the mediadata repo; `gh` CLI),
# MEDIADATA_REPO (default peteowen1/mediadata), KWORB_DELAY (default 3),
# KWORB_RAW_ROOT (default tempdir), KWORB_SKIP_UPLOAD=1 to only capture, KWORB_TRACKS (default 200).

suppressMessages(library(kworb))

repo <- Sys.getenv("MEDIADATA_REPO", "peteowen1/mediadata")
delay <- as.numeric(Sys.getenv("KWORB_DELAY", "3"))
root <- Sys.getenv("KWORB_RAW_ROOT", file.path(tempdir(), "kworb_raw"))
skip_upload <- nzchar(Sys.getenv("KWORB_SKIP_UPLOAD"))

started <- Sys.time()
run_ts <- format(started, "%Y%m%dT%H%M%SZ", tz = "UTC")
tag <- paste0("kworb-raw-", format(started, "%Y-%m", tz = "UTC"))
out_dir <- file.path(root, run_ts)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
base <- "https://kworb.net/spotify/"

problems <- character()
note <- function(...) {
  msg <- paste0(...)
  message("PROBLEM: ", msg)
  problems <<- c(problems, msg)
}

# Pages that are dead on the site itself (seen 5 Nov 2026): ad has no page (404);
# cy, mt and ru charts stopped updating (last dates 2017-08-05, 2019-12-25, 2022-04-07).
# They are still captured and logged, but do not fail the run; any OTHER page does.
known_dead <- paste0(base, "country/", c("ad", "cy", "mt", "ru"), "_daily.html")
report <- function(url, msg) {
  if (url %in% known_dead) message("known-dead page: ", msg) else note(msg)
}

message(sprintf("Capture %s -> %s (delay %gs)", run_ts, out_dir, delay))

# 1. Index page: the site's own country list (D12).
idx <- fetch_raw_pages(paste0(base), out_dir, delay = delay)
codes <- character()
if (!isTRUE(attr(idx, "stopped")) && identical(as.integer(idx$http_status), 200L)) {
  con <- gzfile(file.path(out_dir, idx$file), "rb")
  html <- rawToChar(readBin(con, "raw", 5e7))
  close(con)
  codes <- parse_index_country_codes(html)
  message("Index lists ", length(codes), " country daily charts")
  if (length(codes) < 70) {
    note("index page lists only ", length(codes), " countries (expected about 77); layout changed?")
  }
} else {
  note("index page not fetched (status ", idx$http_status[1], ")")
}

# 2. Daily charts, totals, artists.
urls <- c(
  if (length(codes)) paste0(base, "country/", codes, "_daily.html"),
  paste0(base, "country/global_daily_totals.html"),
  # weekly global chart and its totals (10 Nov: awards 178 ranks songs by weekly totals; Wayback holds only 64 copies of it
  # since 2016). A weekly page describes the last chart week, so it may be up to 14 days old (below).
  paste0(base, "country/global_weekly.html"),
  paste0(base, "country/global_weekly_totals.html"),
  paste0(base, "artists.html")
)
man <- NULL
if (!isTRUE(attr(idx, "stopped")) && !file.exists(file.path(out_dir, "STOP"))) {
  man <- fetch_raw_pages(urls, out_dir, delay = delay)
  if (isTRUE(attr(man, "stopped"))) {
    note("stopped by HTTP 429/403; not attempted: ", length(attr(man, "not_attempted")), " pages")
  }
  bad <- man[is.na(man$http_status) | man$http_status != 200L, ]
  for (i in seq_len(nrow(bad))) report(bad$url[i], paste0(bad$url[i], " -> ", bad$error[i]))
  # The page must say the date we expect (within a day or two of today).
  dated <- man[!is.na(man$page_date) & man$page_date != "", ]
  age <- as.numeric(as.Date(format(started, "%Y-%m-%d", tz = "UTC")) - as.Date(dated$page_date))
  stale <- dated$url[age < 0 | age > ifelse(grepl("_weekly", dated$url), 14, 4)]
  for (u in stale) report(u, paste0("page date out of expected range: ", u))
  # A 200 whose body has no page date where one belongs (a country chart or the totals page) is an error, challenge or changed
  # layout, not data: only artists.html and the index legitimately carry none (review, 5 Nov)
  undated <- man$url[man$http_status %in% 200L & (is.na(man$page_date) | man$page_date == "") & grepl("/country/|totals", man$url)]
  for (u in undated) report(u, paste0("200 with no page date (error or changed page?): ", u))
}

# 2b. Track pages (plan Stage 4, 10 Nov 2026: the old daily-scrape.yml that fetched them is retired). One page per track on
# today's global daily chart (200), saved raw like every other page. A track page holds the last 31 days per country plus
# one row per charted week, so a daily capture of the current top 200 loses nothing; parsing them into track_daily /
# track_weekly is a later step (parse_raw.R skips page types with no parser). KWORB_TRACKS caps the count (0 = none).
n_tracks <- suppressWarnings(as.integer(Sys.getenv("KWORB_TRACKS", "200"))); if (is.na(n_tracks)) n_tracks <- 200L
track_urls <- character()
gd <- if (!is.null(man)) man[grepl("/country/global_daily\\.html$", man$url) & man$http_status %in% 200L, ] else NULL
stopped <- file.exists(file.path(out_dir, "STOP"))
if (n_tracks > 0 && !is.null(gd) && nrow(gd) == 1 && !stopped) {
  # an unreadable chart (a 200 challenge page, a changed layout) must not abort before the tar and upload (review, 10 Nov)
  ids <- tryCatch({
    con <- gzfile(file.path(out_dir, gd$file), "rb"); html <- rawToChar(readBin(con, "raw", 5e7)); close(con)
    unique(kworb:::parse_daily_chart(html, "global")$track_id)   # internal parser (not exported)
  }, error = function(e) { note("could not read track ids from the global daily chart: ", conditionMessage(e)); character() })
  ids <- ids[!is.na(ids) & grepl("^[0-9A-Za-z]{22}$", ids)]
  if (length(ids) < 150) note("global daily chart gave only ", length(ids), " valid track ids (expected about 200)")
  track_urls <- if (length(ids)) paste0(base, "track/", head(ids, n_tracks), ".html") else character()
  message("Track pages: ", length(track_urls))
  tm <- if (length(track_urls)) fetch_raw_pages(track_urls, out_dir, delay = delay) else data.frame(url = character(), http_status = integer())
  if (isTRUE(attr(tm, "stopped"))) note("track pages stopped by HTTP 429/403; not attempted: ", length(attr(tm, "not_attempted")))
  tbad <- tm[is.na(tm$http_status) | tm$http_status != 200L, ]
  # a few dead track pages are not a failed day; more than 5 means something is wrong
  if (nrow(tbad)) message("track pages not 200: ", nrow(tbad), " (", paste(head(basename(tbad$url), 5), collapse = ", "), ")")
  if (nrow(tbad) > 5) note(nrow(tbad), " of ", length(track_urls), " track pages failed")
} else if (n_tracks > 0 && !stopped) {
  note("no track pages captured: today's global daily chart was not fetched")
}   # after a STOP the stop note already fails the run

# 3. Bundle: files + manifest in one tar.
manifest <- utils::read.csv(file.path(out_dir, "manifest.csv"), stringsAsFactors = FALSE)
message(sprintf("%d requests, %d ok", nrow(manifest), sum(manifest$http_status %in% 200L)))
if (length(codes) && nrow(manifest) != length(urls) + 1L + length(track_urls)) {
  note("manifest has ", nrow(manifest), " rows, expected ", length(urls) + 1L + length(track_urls))
}
tar_name <- paste0("kworb-raw-", run_ts, ".tar")
man_name <- paste0("kworb-raw-manifest-", run_ts, ".csv")
tar_path <- file.path(root, tar_name)
man_path <- file.path(root, man_name)
invisible(file.copy(file.path(out_dir, "manifest.csv"), man_path, overwrite = TRUE))   # Rscript prints a bare TRUE otherwise
rc <- system2("tar", c("-cf", shQuote(tar_path), "-C", shQuote(out_dir), "."))
if (rc != 0 || !file.exists(tar_path)) note("tar failed")
n_files <- length(list.files(out_dir, pattern = "\\.gz$"))
message(sprintf("Bundle %s: %d page files, %.1f MB", tar_name, n_files,
                file.size(tar_path) / 1e6))

# 4. Upload and verify (always attempted, even after page failures).
gh <- function(args) {
  tmp <- tempfile()
  rc <- system2("gh", args, stdout = tmp, stderr = tmp)
  list(rc = rc, out = readLines(tmp, warn = FALSE))
}
if (skip_upload) {
  message("KWORB_SKIP_UPLOAD set; not uploading")
} else if (file.exists(tar_path)) {
  if (gh(c("release", "view", tag, "-R", repo))$rc != 0) {
    cr <- gh(c("release", "create", tag, "-R", repo, "--title", shQuote(tag),
               "--notes", shQuote("Raw kworb.net page captures (private). One tar plus manifest per run."),
               "--latest=false"))
    if (cr$rc != 0 && gh(c("release", "view", tag, "-R", repo))$rc != 0) {
      note("could not create release ", tag, ": ", paste(cr$out, collapse = " "))
    }
  }
  up <- gh(c("release", "upload", tag, shQuote(tar_path), shQuote(man_path), "-R", repo))
  if (up$rc != 0) note("upload failed: ", paste(up$out, collapse = " "))
  chk <- gh(c("release", "view", tag, "-R", repo, "--json", "assets", "--jq",
              shQuote(".assets[] | [.name, .size] | @tsv")))
  listed <- if (chk$rc == 0) utils::read.delim(text = chk$out, header = FALSE,
                                               col.names = c("name", "size")) else NULL
  if (is.null(listed) || !all(c(tar_name, man_name) %in% listed$name)) {
    note("uploaded assets not found on release ", tag)
  } else if (listed$size[listed$name == tar_name] != file.size(tar_path)) {
    note("uploaded tar size differs from local")
  } else {
    message(sprintf("Uploaded to https://github.com/%s/releases/tag/%s : %s (%.1f MB), %s",
                    repo, tag, tar_name, file.size(tar_path) / 1e6, man_name))
  }
}

if (length(problems)) {
  message("\nCAPTURE FAILED with ", length(problems), " problem(s):")
  for (p in problems) message(" - ", p)
  quit(status = 1)
}
message("Capture OK: ", nrow(manifest), " requests, ", sum(manifest$http_status %in% 200L), " returned 200 (the rest are pages known dead on the site)")
