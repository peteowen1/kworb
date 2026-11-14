# Raw capture: one request per page, body saved exactly as received.
#
# Stage 1 of docs/plans/kworb-db.md. Nothing here parses a page except the date
# in its heading; parsing is a later, re-runnable step over the saved files.

.kworb_base <- "https://kworb.net/spotify/"

#' Descriptive user agent for raw capture
#' @return Character string.
#' @keywords internal
raw_user_agent <- function() {
  "kworb-raw-capture/0.1 (private research archive; https://github.com/peteowen1/kworb)"
}

#' Page slug for a kworb URL
#'
#' Drops the host, the `/spotify/` prefix and the extension, and turns `/` into
#' `-`: `.../country/us_daily.html` becomes `country-us_daily`.
#'
#' @param url Page URL.
#' @return Character slug.
#' @keywords internal
raw_slug <- function(url) {
  s <- sub("^https?://[^/]+/", "", url)
  s <- sub("^spotify/", "", s)
  s <- sub("\\.[A-Za-z0-9]+$", "", s)
  s <- gsub("[^A-Za-z0-9_.-]+", "-", s)
  if (!nzchar(s)) "index" else s
}

#' File name for a raw capture
#'
#' @param url Page URL.
#' @param fetched_at POSIXct request time.
#' @return `<slug>__<UTC yyyymmddTHHMMSSZ>.html.gz`.
#' @keywords internal
raw_filename <- function(url, fetched_at) {
  ts <- format(as.POSIXct(fetched_at, tz = "UTC"), "%Y%m%dT%H%M%SZ", tz = "UTC")
  paste0(raw_slug(url), "__", ts, ".html.gz")
}

#' Parse the date a kworb page says it describes
#'
#' Looks in the page heading (`<span class="pagetitle">`) for `yyyy/mm/dd`; the
#' Totals pages carry it in "Covers charts from A to B" (B is returned).
#' Pages with no date (the artists list) give `NA`.
#'
#' @param html Page text.
#' @return A `Date`, possibly `NA`.
#' @keywords internal
parse_page_date <- function(html) {
  none <- as.Date(NA)
  if (length(html) != 1 || is.na(html)) return(none)
  pat <- "[0-9]{4}/[0-9]{2}/[0-9]{2}"
  head <- regmatches(html, regexpr('class="pagetitle">(<strong>)?[^<]*', html))   # pages to 2019 or so wrap it in <strong>
  if (length(head) == 1) {
    m <- regmatches(head, regexpr(pat, head))
    if (length(m) == 1) return(as.Date(m, format = "%Y/%m/%d"))
  }
  cov <- regmatches(html, regexpr(paste0("Covers charts from ", pat, " to ", pat), html))
  if (length(cov) == 1) {
    m <- regmatches(cov, gregexpr(pat, cov))[[1]]
    return(as.Date(m[2], format = "%Y/%m/%d"))
  }
  none
}

#' One GET, returning status and body bytes (internal; mocked in tests)
#'
#' @param url Page URL.
#' @param user_agent User agent string.
#' @return List with `status` (integer, `NA` on a transport error), `body`
#'   (raw vector) and `error` (message or `NA`).
#' @keywords internal
http_get_raw <- function(url, user_agent) {
  res <- tryCatch(
    httr::GET(url, httr::user_agent(user_agent), httr::timeout(60)),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(status = NA_integer_, body = raw(0), error = conditionMessage(res)))
  }
  list(status = as.integer(httr::status_code(res)), body = httr::content(res, "raw"),
       error = NA_character_)
}

#' Fetch one page and save the response body untouched
#'
#' Makes exactly one request (no retry). On HTTP 200 the body is written
#' byte-for-byte, gzipped, to `out_dir`. A 429 or 403 writes a `STOP` marker
#' file into `out_dir`, records the manifest row to `manifest.csv`, and then
#' signals an error of class `kworb_stop`; the caller must not retry.
#'
#' @param url Full page URL.
#' @param out_dir Directory for the saved file, the manifest and the stop marker.
#' @param user_agent Descriptive user agent.
#' @param http_get Function `(url, user_agent)` returning
#'   `list(status, body, error)`; replaced in tests.
#' @param now Function returning the current time; replaced in tests.
#' @return A one-row tibble: `url, slug, file, http_status, bytes, sha256,
#'   fetched_at, page_date, error`.
#' @export
fetch_raw <- function(url, out_dir, user_agent = raw_user_agent(),
                      http_get = http_get_raw, now = Sys.time) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fetched_at <- as.POSIXct(now(), tz = "UTC")
  res <- http_get(url, user_agent)
  status <- res$status
  body <- res$body
  ok <- !is.na(status) && status == 200L && length(body) > 0

  file <- NA_character_
  sha <- NA_character_
  page_date <- as.Date(NA)
  if (ok) {
    file <- raw_filename(url, fetched_at)
    con <- gzfile(file.path(out_dir, file), "wb")
    writeBin(body, con)
    close(con)
    sha <- digest::digest(body, algo = "sha256", serialize = FALSE)
    # saving never depends on parsing: a body the locale can't read gives no date, not a crash that loses the day's capture
    page_date <- tryCatch(parse_page_date(iconv(rawToChar(body[body != as.raw(0)]), "UTF-8", "UTF-8", sub = "byte")),
                          error = function(e) NA_character_)
  }
  err <- res$error
  if (!ok && is.na(err)) {
    err <- if (!is.na(status) && status == 200L) "empty body" else paste("HTTP", status)
  }

  row <- tibble::tibble(
    url = url,
    slug = raw_slug(url),
    file = file,
    http_status = status,
    bytes = length(body),
    sha256 = sha,
    fetched_at = format(fetched_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    page_date = as.character(page_date),
    error = if (ok) NA_character_ else err
  )

  append_manifest(row, out_dir)

  if (!is.na(status) && status %in% c(429L, 403L)) {
    writeLines(
      sprintf("%s HTTP %d for %s. Stopped; do not retry now.", row$fetched_at, status, url),
      file.path(out_dir, "STOP")
    )
    cli_stop(
      sprintf("HTTP %d from %s; stopping the capture (marker: %s)", status, url,
              file.path(out_dir, "STOP")),
      class = "kworb_stop", manifest_row = row
    )
  }
  row
}

#' Signal a classed error
#' @keywords internal
cli_stop <- function(msg, class, ...) {
  stop(structure(class = c(class, "error", "condition"),
                 list(message = msg, call = NULL, ...)))
}

#' Append a manifest row to `manifest.csv`
#' @keywords internal
append_manifest <- function(row, out_dir) {
  path <- file.path(out_dir, "manifest.csv")
  utils::write.table(as.data.frame(row), path, sep = ",", row.names = FALSE,
                     col.names = !file.exists(path), append = file.exists(path),
                     qmethod = "double", na = "")
  invisible(path)
}

#' Fetch a list of pages politely
#'
#' Calls [fetch_raw()] for each URL with `delay` seconds between requests
#' (not before the first), appending one manifest row per request to
#' `out_dir/manifest.csv` as it goes. A 429 or 403 ends the run at once.
#'
#' @param urls Character vector of page URLs.
#' @param out_dir Output directory.
#' @param delay Seconds between requests.
#' @param ... Passed to [fetch_raw()] (`user_agent`, `http_get`, `now`).
#' @param sleep Function used to wait; replaced in tests.
#' @return The manifest as a tibble. Has attribute `stopped` (`TRUE` if a
#'   429/403 ended the run) and `not_attempted` (URLs never requested).
#' @export
fetch_raw_pages <- function(urls, out_dir, delay = 3, ..., sleep = Sys.sleep) {
  rows <- list()
  stopped <- FALSE
  done <- 0L
  for (i in seq_along(urls)) {
    if (i > 1) sleep(delay)
    row <- tryCatch(
      fetch_raw(urls[i], out_dir, ...),
      kworb_stop = function(e) {
        stopped <<- TRUE
        e$manifest_row
      }
    )
    done <- i
    rows[[i]] <- row
    if (stopped) break
  }
  out <- dplyr::bind_rows(rows)
  attr(out, "stopped") <- stopped
  attr(out, "not_attempted") <- urls[seq_along(urls) > done]
  out
}

#' Country codes on kworb's own index page
#'
#' Reads the codes from the daily-chart links on `index_spotify.html` (the
#' site lists 77, including `global`), instead of the fixed list in
#' [get_country_codes()].
#'
#' @param html Text of the index page.
#' @return Character vector of codes in page order.
#' @export
parse_index_country_codes <- function(html) {
  m <- regmatches(html, gregexpr('href="country/[a-z0-9]+_daily\\.html"', html))[[1]]
  unique(sub('href="country/([a-z0-9]+)_daily\\.html"', "\\1", m))
}

#' Weekday slot of an artist in the songs-page rotation
#'
#' A fixed function of the id (h = (31 h + code) mod 7 over its characters), so an artist keeps its capture day while
#' the artists list around it changes. capture_raw.R fetches the artists whose slot equals the capture date's day
#' number mod 7.
#' @param ids Character vector of Spotify artist ids.
#' @return Integer vector, 0 to 6.
#' @keywords internal
kworb_weekday_of <- function(ids) {
  vapply(ids, function(id) { h <- 0L; for (cp in utf8ToInt(id)) h <- (31L * h + cp) %% 7L; h }, integer(1), USE.NAMES = FALSE)
}
