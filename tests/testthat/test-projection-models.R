# Tests for projection model functions

test_that("create_temporal_features returns correct structure", {
  dates <- as.Date(c("2025-12-25", "2025-01-01", "2025-07-04"))
  result <- kworb::create_temporal_features(dates)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)

  # Check all expected columns exist
  expected_cols <- c("date", "day_of_week", "day_of_month", "day_of_year",
                     "week_of_year", "month", "year", "is_weekend",
                     "is_month_start", "is_month_end")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("create_temporal_features calculates correctly", {
  # Christmas 2025 is a Thursday
  result <- kworb::create_temporal_features(as.Date("2025-12-25"))

  expect_equal(result$month, 12)
  expect_equal(result$day_of_month, 25)
  expect_equal(result$year, 2025)
  expect_equal(result$day_of_year, 359)  # Dec 25 is day 359
  expect_equal(result$day_of_week, 4)    # Thursday = 4 (week starts Monday)
  expect_false(result$is_weekend)
  expect_false(result$is_month_start)
})

test_that("create_temporal_features handles weekends", {
  # Saturday and Sunday
  sat <- kworb::create_temporal_features(as.Date("2025-12-27"))  # Saturday
  sun <- kworb::create_temporal_features(as.Date("2025-12-28"))  # Sunday
  mon <- kworb::create_temporal_features(as.Date("2025-12-29"))  # Monday

  expect_true(sat$is_weekend)
  expect_true(sun$is_weekend)
  expect_false(mon$is_weekend)
})

test_that("create_temporal_features handles month boundaries", {
  start <- kworb::create_temporal_features(as.Date("2025-01-01"))
  end <- kworb::create_temporal_features(as.Date("2025-01-31"))

  expect_true(start$is_month_start)
  expect_false(start$is_month_end)

  expect_false(end$is_month_start)
  expect_true(end$is_month_end)
})

test_that("calculate_rolling_stats adds rolling columns", {
  # Create test data
  test_data <- tibble::tibble(
    date = seq(as.Date("2025-01-01"), as.Date("2025-02-15"), by = "day"),
    streams = rep(1000, 46)  # Constant for easy verification
  )

  result <- kworb::calculate_rolling_stats(test_data)

  expect_true("rolling_7day" %in% names(result))
  expect_true("rolling_14day" %in% names(result))
  expect_true("rolling_30day" %in% names(result))

  # With constant values, rolling mean should equal the value after window is full
  expect_equal(result$rolling_7day[7], 1000)
  expect_equal(result$rolling_14day[14], 1000)
  expect_equal(result$rolling_30day[30], 1000)

  # First few values should be NA (incomplete window)
  expect_true(is.na(result$rolling_7day[1]))
  expect_true(is.na(result$rolling_30day[29]))
})

test_that("calculate_rolling_stats handles custom windows", {
  test_data <- tibble::tibble(
    date = seq(as.Date("2025-01-01"), as.Date("2025-01-15"), by = "day"),
    streams = 1:15 * 100
  )

  result <- kworb::calculate_rolling_stats(test_data, windows = c(3, 5))

  expect_true("rolling_3day" %in% names(result))
  expect_true("rolling_5day" %in% names(result))
  expect_false("rolling_7day" %in% names(result))
})

test_that("add_yoy_features adds seasonal flags", {
  # Create test data spanning multiple seasons
  test_data <- tibble::tibble(
    date = as.Date(c("2025-12-25", "2025-10-31", "2025-07-04", "2025-02-14")),
    streams = c(1000000, 500000, 300000, 400000)
  )

  result <- kworb::add_yoy_features(test_data)

  expect_true("is_christmas_season" %in% names(result))
  expect_true("is_halloween_season" %in% names(result))
  expect_true("is_summer" %in% names(result))
  expect_true("is_valentines" %in% names(result))

  # Note: add_yoy_features sorts by date, so order is: Feb, Jul, Oct, Dec
  # Check flags by filtering to specific dates
  expect_true(result$is_christmas_season[result$date == as.Date("2025-12-25")])
  expect_true(result$is_halloween_season[result$date == as.Date("2025-10-31")])
  expect_true(result$is_summer[result$date == as.Date("2025-07-04")])
  expect_true(result$is_valentines[result$date == as.Date("2025-02-14")])
})

test_that("add_yoy_features calculates year-over-year ratio", {
  # Create data with same day across two years
  test_data <- tibble::tibble(
    date = as.Date(c("2024-12-25", "2025-12-25")),
    streams = c(1000000, 1200000)
  )

  result <- kworb::add_yoy_features(test_data)

  expect_true("yoy_streams" %in% names(result))
  expect_true("yoy_ratio" %in% names(result))

  # 2025 should have YoY data from 2024
  expect_equal(result$yoy_streams[2], 1000000)
  expect_equal(result$yoy_ratio[2], 1.2)  # 1200000 / 1000000

  # 2024 has no prior year data
  expect_true(is.na(result$yoy_streams[1]))
})

test_that("analyze_dow_patterns returns correct structure", {
  # Create test data with enough days
  test_data <- tibble::tibble(
    date = seq(as.Date("2025-01-01"), as.Date("2025-01-31"), by = "day"),
    streams = rep(1000, 31)
  )

  result <- kworb::analyze_dow_patterns(test_data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 7)  # 7 days of week
  expect_true("day_of_week" %in% names(result))
  expect_true("avg_streams" %in% names(result))
  expect_true("multiplier" %in% names(result))
  expect_true("day_name" %in% names(result))
})

test_that("evaluate_model calculates correct metrics", {
  actual <- c(100, 200, 300, 400, 500)
  predicted <- c(110, 190, 310, 390, 510)

  result <- kworb::evaluate_model(actual, predicted)

  expect_true("mae" %in% names(result))
  expect_true("rmse" %in% names(result))
  expect_true("mape" %in% names(result))
  expect_true("n" %in% names(result))

  # MAE should be 10 (average of |10|, |10|, |10|, |10|, |10|)
  expect_equal(result$mae, 10)
  expect_equal(result$n, 5)
})
