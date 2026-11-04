# Tests for data release functions: the file-name rules the loaders use to tell chart files from
# track files (is_chart_file / is_track_file in R/data_releases.R), and list_release_files()'s
# behaviour when the release cannot be listed.

test_that("chart files are recognised by _daily_ or _weekly_", {
  expect_true(all(is_chart_file(c(
    "global_daily_2025-12-29.csv", "us_daily_2025-12-29.csv",
    "global_weekly_2025-12-29.csv", "gb_weekly_2025-01-01.csv"
  ))))
  expect_false(any(is_chart_file(c(
    "53iuhJlwXhSER5J2IYYv1W.csv", "0bYg9bo50gSsH3LtXe2SQn.csv", "7qiZfU4dY1lWllzX7mPBI3.csv"
  ))))
})

test_that("track files are a 22-character Spotify id + .csv", {
  expect_true(all(is_track_file(c(
    "53iuhJlwXhSER5J2IYYv1W.csv", "0bYg9bo50gSsH3LtXe2SQn.csv", "7qiZfU4dY1lWllzX7mPBI3.csv"
  ))))
  expect_false(any(is_track_file(c(
    "global_daily_2025-12-29.csv", "us_weekly_2025-01-01.csv", "last_scrape.json",
    "short.csv",                                  # too short
    "waytoolongtobeavalidspotifyid123.csv",       # too long
    "53iuhJlwXhSER5J2IYYv1!.csv",                 # invalid character
    "53iuhJlwXhSER5J2IYYv1W.json"                 # wrong extension
  ))))
})

test_that("a release listing splits cleanly into chart and track files", {
  files <- c(
    "global_daily_2025-12-29.csv", "53iuhJlwXhSER5J2IYYv1W.csv", "0bYg9bo50gSsH3LtXe2SQn.csv",
    "us_weekly_2025-12-22.csv", "7qiZfU4dY1lWllzX7mPBI3.csv", "last_scrape.json"
  )
  expect_identical(files[is_chart_file(files)], c("global_daily_2025-12-29.csv", "us_weekly_2025-12-22.csv"))
  expect_identical(files[is_track_file(files)],
                   c("53iuhJlwXhSER5J2IYYv1W.csv", "0bYg9bo50gSsH3LtXe2SQn.csv", "7qiZfU4dY1lWllzX7mPBI3.csv"))
})

test_that("list_release_files returns an empty tibble when the release cannot be listed", {
  # no GitHub call: pb_list is replaced by one that fails the way an unreachable release does
  local_mocked_bindings(pb_list = function(...) stop("Cannot access release data"), .package = "piggyback")
  expect_message(result <- list_release_files("nonexistent/repo", "nonexistent-tag"), "Could not list files")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
