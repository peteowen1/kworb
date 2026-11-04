# Tests for data release functions

# Test file pattern matching logic used in load functions
# These patterns distinguish chart files from track files

test_that("chart file pattern matches daily/weekly files", {
  # Chart files contain _daily_ or _weekly_
  chart_pattern <- "_daily_|_weekly_"

  expect_true(grepl(chart_pattern, "global_daily_2025-12-29.csv"))
  expect_true(grepl(chart_pattern, "us_daily_2025-12-29.csv"))
  expect_true(grepl(chart_pattern, "global_weekly_2025-12-29.csv"))
  expect_true(grepl(chart_pattern, "gb_weekly_2025-01-01.csv"))
})

test_that("chart file pattern excludes track files", {
  chart_pattern <- "_daily_|_weekly_"

  # Track IDs (22 char alphanumeric) should NOT match chart pattern
  expect_false(grepl(chart_pattern, "53iuhJlwXhSER5J2IYYv1W.csv"))
  expect_false(grepl(chart_pattern, "0bYg9bo50gSsH3LtXe2SQn.csv"))
  expect_false(grepl(chart_pattern, "7qiZfU4dY1lWllzX7mPBI3.csv"))
})

test_that("track file pattern matches Spotify IDs", {
  # Track files are exactly 22 alphanumeric characters + .csv
  track_pattern <- "^[A-Za-z0-9]{22}\\.csv$"

  expect_true(grepl(track_pattern, "53iuhJlwXhSER5J2IYYv1W.csv"))
  expect_true(grepl(track_pattern, "0bYg9bo50gSsH3LtXe2SQn.csv"))
  expect_true(grepl(track_pattern, "7qiZfU4dY1lWllzX7mPBI3.csv"))
})

test_that("track file pattern excludes chart files", {
  track_pattern <- "^[A-Za-z0-9]{22}\\.csv$"

  expect_false(grepl(track_pattern, "global_daily_2025-12-29.csv"))
  expect_false(grepl(track_pattern, "us_weekly_2025-01-01.csv"))
  expect_false(grepl(track_pattern, "last_scrape.json"))
})

test_that("track file pattern excludes invalid IDs", {
  track_pattern <- "^[A-Za-z0-9]{22}\\.csv$"

  # Too short
  expect_false(grepl(track_pattern, "short.csv"))
  # Too long
  expect_false(grepl(track_pattern, "waytoolongtobeavalidspotifyid123.csv"))
  # Invalid characters
  expect_false(grepl(track_pattern, "53iuhJlwXhSER5J2IYYv1!.csv"))
  # Wrong extension
  expect_false(grepl(track_pattern, "53iuhJlwXhSER5J2IYYv1W.json"))
})

test_that("combined patterns correctly separate chart from track files", {
  chart_pattern <- "_daily_|_weekly_"
  track_pattern <- "^[A-Za-z0-9]{22}\\.csv$"

  # Sample filenames that would be in a release
  test_files <- c(
    "global_daily_2025-12-29.csv",
    "53iuhJlwXhSER5J2IYYv1W.csv",
    "0bYg9bo50gSsH3LtXe2SQn.csv",
    "us_weekly_2025-12-22.csv",
    "7qiZfU4dY1lWllzX7mPBI3.csv",
    "last_scrape.json"
  )

  chart_files <- test_files[grepl(chart_pattern, test_files)]
  track_files <- test_files[
    grepl(track_pattern, test_files) & !grepl(chart_pattern, test_files)
  ]

  expect_equal(length(chart_files), 2)
  expect_equal(length(track_files), 3)

  expect_true("global_daily_2025-12-29.csv" %in% chart_files)
  expect_true("us_weekly_2025-12-22.csv" %in% chart_files)

  expect_true("53iuhJlwXhSER5J2IYYv1W.csv" %in% track_files)
  expect_true("0bYg9bo50gSsH3LtXe2SQn.csv" %in% track_files)
  expect_true("7qiZfU4dY1lWllzX7mPBI3.csv" %in% track_files)
})

test_that("list_release_files returns tibble", {
  # This tests the return type without actually calling GitHub
  # We test that an error returns an empty tibble
  result <- kworb::list_release_files("nonexistent/repo", "nonexistent-tag")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
