fake_get <- function(status = 200L, body = "<html></html>") {
  force(status); force(body)
  function(url, user_agent) {
    list(status = status, body = charToRaw(body), error = NA_character_)
  }
}
t0 <- as.POSIXct("2026-11-05 03:15:00", tz = "UTC")
chart_html <- paste0('<html><span class="pagetitle">Spotify Daily Chart - Global - 2026/11/03 | ',
                     '<a href="x">Totals</a></span></html>')

test_that("slug and file name follow page and UTC time", {
  expect_equal(raw_slug("https://kworb.net/spotify/country/us_daily.html"), "country-us_daily")
  expect_equal(raw_slug("https://kworb.net/spotify/artists.html"), "artists")
  expect_equal(raw_filename("https://kworb.net/spotify/country/us_daily.html", t0),
               "country-us_daily__20261105T031500Z.html.gz")
})

test_that("body is saved byte-for-byte and the manifest row is right", {
  d <- withr::local_tempdir()
  body <- paste0(chart_html, "é \r\n tail")
  row <- fetch_raw("https://kworb.net/spotify/country/global_daily.html", d,
                   http_get = fake_get(200L, body), now = function() t0)
  f <- file.path(d, row$file)
  expect_true(file.exists(f))
  con <- gzfile(f, "rb"); got <- readBin(con, "raw", 1e6); close(con)
  expect_identical(got, charToRaw(body))
  expect_equal(row$sha256, digest::digest(charToRaw(body), "sha256", serialize = FALSE))
  expect_equal(row$bytes, length(charToRaw(body)))
  expect_equal(row$http_status, 200L)
  expect_equal(row$page_date, "2026-11-03")
  expect_equal(row$fetched_at, "2026-11-05T03:15:00Z")
  m <- utils::read.csv(file.path(d, "manifest.csv"))
  expect_equal(nrow(m), 1)
  expect_equal(m$url, row$url)
  expect_false(file.exists(file.path(d, "STOP")))
})

test_that("page date parses from heading, Covers line, and is NA when absent", {
  expect_equal(parse_page_date(chart_html), as.Date("2026-11-03"))
  expect_equal(parse_page_date('<span class="pagetitle"></span>Covers charts from 2014/08/10 to 2026/11/03.<br>'),
               as.Date("2026-11-03"))
  expect_true(is.na(parse_page_date('<span class="pagetitle">Spotify most streamed artists</span>')))
})

test_that("429 and 403 write a STOP marker, record the row and signal kworb_stop", {
  for (s in c(429L, 403L)) {
    d <- withr::local_tempdir()
    expect_error(fetch_raw("https://kworb.net/spotify/x.html", d, http_get = fake_get(s, "no")),
                 class = "kworb_stop")
    expect_true(file.exists(file.path(d, "STOP")))
    m <- utils::read.csv(file.path(d, "manifest.csv"))
    expect_equal(m$http_status, s)
    expect_length(list.files(d, pattern = "\\.gz$"), 0)
  }
})

test_that("fetch_raw_pages sleeps between requests only and stops at 429 with no retry", {
  d <- withr::local_tempdir()
  calls <- character(); sleeps <- 0
  getter <- function(url, user_agent) {
    calls <<- c(calls, url)
    st <- if (length(calls) == 3) 429L else 200L
    list(status = st, body = charToRaw(chart_html), error = NA_character_)
  }
  urls <- paste0("https://kworb.net/spotify/country/", c("a", "b", "c", "d", "e"), "_daily.html")
  m <- fetch_raw_pages(urls, d, delay = 3, http_get = getter, sleep = function(s) sleeps <<- sleeps + s)
  expect_length(calls, 3)           # c was requested once; d, e never
  expect_equal(sleeps, 6)           # between 1-2 and 2-3 only
  expect_true(attr(m, "stopped"))
  expect_equal(attr(m, "not_attempted"), urls[4:5])
  expect_equal(nrow(utils::read.csv(file.path(d, "manifest.csv"))), 3)
  expect_true(file.exists(file.path(d, "STOP")))
})

test_that("non-200 other than 429/403 is recorded, nothing saved, run continues", {
  d <- withr::local_tempdir()
  getter <- function(url, user_agent) {
    if (grepl("bad", url)) list(status = 500L, body = raw(0), error = NA_character_)
    else list(status = 200L, body = charToRaw(chart_html), error = NA_character_)
  }
  m <- fetch_raw_pages(paste0("https://kworb.net/spotify/", c("bad", "ok"), ".html"), d,
                       delay = 0, http_get = getter, sleep = function(s) NULL)
  expect_false(attr(m, "stopped"))
  expect_equal(m$http_status, c(500L, 200L))
  expect_true(is.na(m$file[1]))
  expect_match(m$error[1], "HTTP 500")
})

test_that("country codes come from the index page links", {
  html <- '<a href="country/global_daily.html">x</a><a href="country/us_daily.html">x</a>
           <a href="country/us_weekly.html">x</a><a href="country/ad_daily.html">x</a>
           <a href="country/us_daily.html">dup</a>'
  expect_equal(parse_index_country_codes(html), c("global", "us", "ad"))
})
