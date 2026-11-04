# Tests for utility functions

test_that("parse_streams handles standard formats", {
  expect_equal(kworb:::parse_streams("128,896,137"), 128896137)
  expect_equal(kworb:::parse_streams("1,234"), 1234)
  expect_equal(kworb:::parse_streams("5675034"), 5675034)
  expect_equal(kworb:::parse_streams("+979,081"), 979081)
})

test_that("parse_streams handles edge cases", {
  expect_true(is.na(kworb:::parse_streams(NA)))
  expect_true(is.na(kworb:::parse_streams("")))
  expect_true(is.na(kworb:::parse_streams("-")))
})

test_that("parse_position_streams parses position and streams", {
  result <- kworb:::parse_position_streams("1 (128,896,137)")
  expect_equal(result$position, 1L)
  expect_equal(result$streams, 128896137)

  result2 <- kworb:::parse_position_streams("42 (5,675,034)")
  expect_equal(result2$position, 42L)
  expect_equal(result2$streams, 5675034)
})

test_that("parse_position_streams handles streams only", {
  result <- kworb:::parse_position_streams("128,896,137")
  expect_true(is.na(result$position))
  expect_equal(result$streams, 128896137)
})

test_that("parse_position_streams handles position only", {
  result <- kworb:::parse_position_streams("42")
  expect_equal(result$position, 42L)
  expect_true(is.na(result$streams))
})

test_that("parse_position_streams handles edge cases", {
  result <- kworb:::parse_position_streams(NA)
  expect_true(is.na(result$position))
  expect_true(is.na(result$streams))

  result2 <- kworb:::parse_position_streams("")
  expect_true(is.na(result2$position))
  expect_true(is.na(result2$streams))

  result3 <- kworb:::parse_position_streams("-")
  expect_true(is.na(result3$position))
  expect_true(is.na(result3$streams))
})

test_that("validate_spotify_id validates correctly", {
  # Valid 22-char alphanumeric IDs

expect_true(kworb:::validate_spotify_id("53iuhJlwXhSER5J2IYYv1W"))
  expect_true(kworb:::validate_spotify_id("0bYg9bo50gSsH3LtXe2SQn"))

  # Invalid IDs
  expect_false(kworb:::validate_spotify_id("tooshort"))
  expect_false(kworb:::validate_spotify_id("waytoolongtobeavalidspotifyid123"))
  expect_false(kworb:::validate_spotify_id(NA))
  expect_false(kworb:::validate_spotify_id(""))
  expect_false(kworb:::validate_spotify_id("53iuhJlwXhSER5J2IYYv1!"))  # Invalid char
})

test_that("extract_spotify_id extracts from hrefs", {
  expect_equal(
    kworb:::extract_spotify_id("../track/53iuhJlwXhSER5J2IYYv1W.html"),
    "53iuhJlwXhSER5J2IYYv1W"
  )
  expect_equal(
    kworb:::extract_spotify_id("/spotify/track/0bYg9bo50gSsH3LtXe2SQn.html"),
    "0bYg9bo50gSsH3LtXe2SQn"
  )
  expect_true(is.na(kworb:::extract_spotify_id(NA)))
  expect_true(is.na(kworb:::extract_spotify_id("")))
})

test_that("build_kworb_url constructs correct URLs", {
  expect_equal(
    kworb:::build_kworb_url("chart", "global", "daily"),
    "https://kworb.net/spotify/country/global_daily.html"
  )
  expect_equal(
    kworb:::build_kworb_url("chart", "us", "weekly"),
    "https://kworb.net/spotify/country/us_weekly.html"
  )
  expect_equal(
    kworb:::build_kworb_url("track", "53iuhJlwXhSER5J2IYYv1W"),
    "https://kworb.net/spotify/track/53iuhJlwXhSER5J2IYYv1W.html"
  )
})

test_that("parse_position_change handles various formats", {
  result <- kworb:::parse_position_change("NEW")
  expect_equal(result$type, "new")

  result2 <- kworb:::parse_position_change("RE")
  expect_equal(result2$type, "re")

  result3 <- kworb:::parse_position_change("+46")
  expect_equal(result3$type, "up")
  expect_equal(result3$value, 46L)

  result4 <- kworb:::parse_position_change("-2")
  expect_equal(result4$type, "down")
  expect_equal(result4$value, 2L)

  result5 <- kworb:::parse_position_change("=")
  expect_equal(result5$type, "same")
  expect_equal(result5$value, 0L)
})
