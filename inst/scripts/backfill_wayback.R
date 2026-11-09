# Stage 3 backfill B1-B4 (docs/plans/kworb-db.md 3.6): every Wayback Machine copy (one per day) of
#   artists.html, country/global_daily.html, country/global_weekly.html, country/global_daily_totals.html
# saved raw, exactly as archived (the `id_` form returns the original bytes, no Wayback toolbar).
#
#   Rscript inst/scripts/backfill_wayback.R [out_dir] [pages]
#   out_dir default data/raw_wayback; pages = comma-separated kworb paths (default the four above).
# Per page: the CDX listing is saved raw first (cdx/<slug>__<UTC>.json.gz), then one request per archived day.
# A copy is saved as <slug>__wb<archive timestamp>.html.gz, where the timestamp is the one the archive actually served
# (it may redirect to a nearby capture); manifest.csv gets one row per request (requested and served timestamps, status,
# bytes, sha256). Resumable: a day already saved (by requested timestamp, in manifest.csv) is never requested again.
# Polite: 3 s between requests, a 429 or 403 writes STOP and ends the run (do not retry today); 20 failures in a row also stop.
# About 1,700 requests, ~1.5-2 h. Run detached on Windows:
#   Start-Process powershell -ArgumentList '-File','<ps1 that runs this from the package root>'
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else "data/raw_wayback"
pages <- if (length(args) >= 2) strsplit(args[2], ",")[[1]] else c("artists.html", "country/global_daily.html", "country/global_weekly.html", "country/global_daily_totals.html")
if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".", quiet = TRUE) else library(kworb)
dir.create(file.path(out_dir, "cdx"), recursive = TRUE, showWarnings = FALSE)
if (file.exists(file.path(out_dir, "STOP"))) stop("STOP marker present in ", out_dir, ": ", paste(readLines(file.path(out_dir, "STOP")), collapse = " "))
ua <- raw_user_agent(); DELAY <- 3
mf <- file.path(out_dir, "manifest.csv")
done <- if (file.exists(mf)) { m <- utils::read.csv(mf, colClasses = "character"); m$requested_ts[m$http_status %in% "200"] } else character()
get <- function(url) { r <- tryCatch(httr::GET(url, httr::user_agent(ua), httr::timeout(90)), error = function(e) e)
  if (inherits(r, "error")) return(list(status = NA_integer_, body = raw(0), url = url, error = conditionMessage(r)))
  list(status = as.integer(httr::status_code(r)), body = httr::content(r, "raw"), url = r$url, error = NA_character_) }
save_gz <- function(body, path) { con <- gzfile(path, "wb"); writeBin(body, con); close(con) }
stop_if_blocked <- function(st, url) if (!is.na(st) && st %in% c(429L, 403L)) {
  writeLines(sprintf("%s HTTP %d for %s. Stopped; do not retry today.", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), st, url), file.path(out_dir, "STOP"))
  stop("HTTP ", st, " from ", url, ": stopped (marker ", file.path(out_dir, "STOP"), ")") }
first <- TRUE; fails <- 0L
for (pg in pages) {
  slug <- raw_slug(paste0("https://kworb.net/spotify/", pg))
  cdx_url <- paste0("https://web.archive.org/cdx/search/cdx?url=kworb.net/spotify/", pg, "&output=json&fl=timestamp,statuscode,length,digest&filter=statuscode:200&collapse=timestamp:8")
  if (!first) Sys.sleep(DELAY); first <- FALSE
  r <- get(cdx_url); stop_if_blocked(r$status, cdx_url)
  if (is.na(r$status) || r$status != 200L) stop("CDX failed for ", pg, ": ", r$status, " ", r$error)
  save_gz(r$body, file.path(out_dir, "cdx", sprintf("%s__%s.json.gz", slug, format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))))
  j <- jsonlite::fromJSON(rawToChar(r$body)); ts <- if (length(j) && nrow(j) > 1) j[-1, 1] else character()
  todo <- setdiff(ts, done)
  cat(format(Sys.time(), "%H:%M:%S"), pg, "| archived days:", length(ts), "| already saved:", length(ts) - length(todo), "| to fetch:", length(todo), "\n")
  for (i in seq_along(todo)) { t <- todo[i]; Sys.sleep(DELAY)
    u <- sprintf("https://web.archive.org/web/%sid_/https://kworb.net/spotify/%s", t, pg); r <- get(u)
    served <- sub("^.*/web/([0-9]{14})[a-z_]*/.*$", "\\1", r$url); if (!grepl("^[0-9]{14}$", served)) served <- t
    ok <- !is.na(r$status) && r$status == 200L && length(r$body) > 0
    file <- if (ok) sprintf("%s__wb%s.html.gz", slug, served) else ""
    if (ok) save_gz(r$body, file.path(out_dir, file))
    row <- data.frame(page = pg, slug = slug, requested_ts = t, served_ts = served, url = u, final_url = r$url, http_status = r$status, bytes = length(r$body),
                      sha256 = if (ok) digest::digest(r$body, algo = "sha256", serialize = FALSE) else "", file = file, fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                      error = if (ok) "" else ifelse(is.na(r$error), paste("HTTP", r$status), r$error), stringsAsFactors = FALSE)
    utils::write.table(row, mf, sep = ",", row.names = FALSE, col.names = !file.exists(mf), append = file.exists(mf), qmethod = "double")
    stop_if_blocked(r$status, u)
    fails <- if (ok) 0L else fails + 1L; if (fails >= 20) stop("20 failed requests in a row; stopping (last: ", u, " ", row$error, ")")
    if (i %% 50 == 0 || i == length(todo)) cat(format(Sys.time(), "%H:%M:%S"), pg, i, "of", length(todo), "| last served", served, "\n") } }
m <- utils::read.csv(mf, colClasses = "character")
ok <- m$http_status %in% "200"   # NA status (transport error) is a failure, not NA in the count
cat(format(Sys.time(), "%H:%M:%S"), "done | requests in manifest:", nrow(m), "| saved:", sum(ok), "| failed (retried on the next run):", sum(!ok), "| saved by page:
"); print(table(m$page[ok]))
