# Tests for data storage functions

test_that("merge_data combines and deduplicates", {
  existing <- tibble::tibble(
    date = as.Date(c("2025-01-01", "2025-01-02")),
    country = c("US", "US"),
    streams = c(1000, 2000),
    track_id = c("abc", "abc")
  )

  new <- tibble::tibble(
    date = as.Date(c("2025-01-02", "2025-01-03")),  # 01-02 is duplicate
    country = c("US", "US"),
    streams = c(2000, 3000),
    track_id = c("abc", "abc")
  )

  result <- kworb::merge_data(existing, new, c("date", "country", "track_id"))

  expect_equal(nrow(result), 3)  # 3 unique date/country/track combinations
  expect_equal(sort(as.character(result$date)),
               c("2025-01-01", "2025-01-02", "2025-01-03"))
})

test_that("merge_data handles empty data frames", {
  existing <- tibble::tibble(
    date = as.Date("2025-01-01"),
    streams = 1000
  )

  empty <- tibble::tibble(
    date = as.Date(character()),
    streams = numeric()
  )

  # Empty new data returns existing
  result1 <- kworb::merge_data(existing, empty, c("date"))
  expect_equal(nrow(result1), 1)

  # Empty existing returns new
  result2 <- kworb::merge_data(empty, existing, c("date"))
  expect_equal(nrow(result2), 1)
})

test_that("aggregate_global_streams sums by date", {
  history <- tibble::tibble(
    date = as.Date(c("2025-01-01", "2025-01-01", "2025-01-02", "2025-01-02")),
    country = c("US", "GB", "US", "GB"),
    streams = c(1000, 500, 2000, 600),
    track_id = rep("abc", 4)
  )

  result <- kworb::aggregate_global_streams(history)

  expect_equal(nrow(result), 2)  # 2 unique dates
  expect_equal(result$streams[result$date == as.Date("2025-01-01")], 1500)
  expect_equal(result$streams[result$date == as.Date("2025-01-02")], 2600)
})

test_that("aggregate_global_streams handles NA streams", {
  history <- tibble::tibble(
    date = as.Date(c("2025-01-01", "2025-01-01")),
    country = c("US", "GB"),
    streams = c(1000, NA),
    track_id = rep("abc", 2)
  )

  result <- kworb::aggregate_global_streams(history)

  expect_equal(nrow(result), 1)
  expect_equal(result$streams, 1000)  # NA is ignored in sum
})

test_that("list_stored_tracks returns empty for missing directory", {
  result <- kworb::list_stored_tracks("/nonexistent/path")
  expect_equal(length(result), 0)
  expect_type(result, "character")
})

test_that("get_storage_summary returns correct structure", {
  result <- kworb::get_storage_summary("/nonexistent/path")

  expect_type(result, "list")
  expect_true("chart_days" %in% names(result))
  expect_true("track_count" %in% names(result))
  expect_true("chart_path" %in% names(result))
  expect_true("tracks_path" %in% names(result))
})

test_that("load_chart_data returns empty tibble for missing directory", {
  expect_warning(
    result <- kworb::load_chart_data("/nonexistent/path"),
    "Directory does not exist"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("load_track_history returns empty tibble for missing track", {
  expect_warning(
    result <- kworb::load_track_history("nonexistent_track", "/nonexistent/path"),
    "Directory does not exist"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# Tests using temp directory for file I/O
test_that("save and load chart data roundtrip works", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  chart_path <- file.path(temp_dir, "charts")

  # Create test data
  test_data <- tibble::tibble(
    pos = 1:3,
    artist = c("Artist A", "Artist B", "Artist C"),
    title = c("Song 1", "Song 2", "Song 3"),
    streams_daily = c(1000000, 500000, 250000),
    scrape_date = as.Date("2025-12-29"),
    country = "global"
  )

  # Save
  saved_path <- kworb::save_chart_data(test_data, path = chart_path)
  expect_true(file.exists(saved_path))

  # Load
  loaded <- kworb::load_chart_data(path = chart_path)
  expect_equal(nrow(loaded), 3)
  expect_equal(loaded$artist, test_data$artist)
})

test_that("save and load track history roundtrip works", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  tracks_path <- file.path(temp_dir, "tracks")

  # Create test data
  test_data <- tibble::tibble(
    date = as.Date(c("2025-01-01", "2025-01-02")),
    country = c("US", "US"),
    streams = c(1000, 2000),
    track_id = "53iuhJlwXhSER5J2IYYv1W"
  )

  # Save
  kworb::save_track_history(test_data, path = tracks_path)

  # Load
  loaded <- kworb::load_track_history("53iuhJlwXhSER5J2IYYv1W", path = tracks_path)
  expect_equal(nrow(loaded), 2)
  expect_equal(loaded$streams, test_data$streams)
})

test_that("save_track_history appends and deduplicates", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  tracks_path <- file.path(temp_dir, "tracks")
  track_id <- "53iuhJlwXhSER5J2IYYv1W"

  # First save
  data1 <- tibble::tibble(
    date = as.Date(c("2025-01-01", "2025-01-02")),
    country = c("US", "US"),
    streams = c(1000, 2000),
    track_id = track_id
  )
  kworb::save_track_history(data1, path = tracks_path)

  # Second save with overlap
  data2 <- tibble::tibble(
    date = as.Date(c("2025-01-02", "2025-01-03")),  # 01-02 overlaps
    country = c("US", "US"),
    streams = c(2000, 3000),
    track_id = track_id
  )
  kworb::save_track_history(data2, path = tracks_path)

  # Load and verify deduplication
  loaded <- kworb::load_track_history(track_id, path = tracks_path)
  expect_equal(nrow(loaded), 3)  # 3 unique dates, not 4
})
