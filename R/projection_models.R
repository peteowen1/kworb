# Stream projection and forecasting models

#' Calculate rolling statistics for stream history
#'
#' Adds rolling mean columns for different window sizes to track history.
#'
#' @param history tibble with date and streams columns (from aggregate_global_streams)
#' @param windows Vector of window sizes in days (default: c(7, 14, 30))
#' @return tibble with additional rolling_Xday columns
#' @export
#' @examples
#' \dontrun{
#' history <- load_track_history("53iuhJlwXhSER5J2IYYv1W")
#' global <- aggregate_global_streams(history)
#' with_rolling <- calculate_rolling_stats(global)
#' }
calculate_rolling_stats <- function(history, windows = c(7, 14, 30)) {
# Ensure sorted by date
history <- dplyr::arrange(history, date)

# Use slider if available, otherwise base R
use_slider <- requireNamespace("slider", quietly = TRUE)

for (window in windows) {
  col_name <- sprintf("rolling_%dday", window)

  if (use_slider) {
    history[[col_name]] <- slider::slide_dbl(
      history$streams,
      mean,
      .before = window - 1,
      .complete = TRUE
    )
  } else {
    # Base R fallback - simple rolling mean
    n <- nrow(history)
    rolling <- rep(NA_real_, n)
    for (i in window:n) {
      rolling[i] <- mean(history$streams[(i - window + 1):i])
    }
    history[[col_name]] <- rolling
  }
}

history
}

#' Create temporal features from dates
#'
#' Generates calendar-based features useful for stream prediction.
#'
#' @param dates Date vector
#' @return tibble with temporal feature columns
#' @export
#' @examples
#' \dontrun{
#' dates <- seq(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
#' features <- create_temporal_features(dates)
#' }
create_temporal_features <- function(dates) {
tibble::tibble(
  date = dates,
  day_of_week = lubridate::wday(dates, week_start = 1),  # Monday = 1
  day_of_month = lubridate::mday(dates),
  day_of_year = lubridate::yday(dates),
  week_of_year = lubridate::isoweek(dates),
  month = lubridate::month(dates),
  year = lubridate::year(dates),
  is_weekend = day_of_week %in% c(6, 7),
  is_month_start = day_of_month <= 3,
  is_month_end = day_of_month >= 28
)
}

#' Prepare features for modeling
#'
#' Combines rolling statistics and temporal features for a track's history.
#'
#' @param history tibble with date and streams columns
#' @return tibble with all features needed for modeling
#' @export
prepare_model_features <- function(history) {
# Get global streams if per-country data
if ("country" %in% names(history)) {
  history <- aggregate_global_streams(history)
}

# Ensure sorted
history <- dplyr::arrange(history, date)

# Add rolling stats
history <- calculate_rolling_stats(history)

# Add temporal features
temporal <- create_temporal_features(history$date)
history <- dplyr::left_join(history, temporal, by = "date")

# Add lag features
history <- history |>
  dplyr::mutate(
    lag_1 = dplyr::lag(streams, 1),
    lag_7 = dplyr::lag(streams, 7),
    lag_14 = dplyr::lag(streams, 14),
    pct_change_1 = (streams - lag_1) / lag_1,
    pct_change_7 = (streams - lag_7) / lag_7
  )

# Add momentum (ratio of short to long term average)
history <- history |>
  dplyr::mutate(
    momentum_7_30 = rolling_7day / rolling_30day
  )

history
}

#' Build stream projection model
#'
#' Fits a linear regression model to predict daily streams.
#'
#' @param history tibble with stream history (raw or with features)
#' @param method Model type: "lm" (default), or "gam" (requires mgcv)
#' @return Fitted model object
#' @export
#' @examples
#' \dontrun{
#' history <- load_track_history("53iuhJlwXhSER5J2IYYv1W")
#' model <- build_projection_model(history)
#' }
build_projection_model <- function(history, method = "lm") {
method <- match.arg(method, c("lm", "gam"))

# Prepare features if not already done
if (!"rolling_7day" %in% names(history)) {
  history <- prepare_model_features(history)
}

# Remove rows with NAs (from rolling calculations)
model_data <- tidyr::drop_na(history)

if (nrow(model_data) < 30) {
  warning("Less than 30 complete observations for modeling")
}

if (method == "lm") {
  # Linear model with key features
  model <- stats::lm(
    streams ~ rolling_7day + rolling_14day + rolling_30day +
      factor(day_of_week) + momentum_7_30 + pct_change_7,
    data = model_data
  )
} else if (method == "gam") {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' required for GAM models")
  }

  model <- mgcv::gam(
    streams ~ s(rolling_7day) + s(rolling_30day) +
      s(day_of_week, bs = "cc", k = 7) +
      s(day_of_year, bs = "cc", k = 12),
    data = model_data
  )
}

# Store metadata
attr(model, "track_id") <- unique(history$track_id)[1]
attr(model, "last_date") <- max(history$date)
attr(model, "method") <- method

model
}

#' Predict future streams
#'
#' Generates stream predictions for future dates.
#'
#' @param model Fitted model from build_projection_model()
#' @param history Original history data (needed for feature calculation)
#' @param days_ahead Number of days to forecast (default: 30)
#' @return tibble with date, predicted_streams, and confidence intervals
#' @export
#' @examples
#' \dontrun{
#' history <- load_track_history("53iuhJlwXhSER5J2IYYv1W")
#' model <- build_projection_model(history)
#' forecast <- predict_streams(model, history, days_ahead = 14)
#' }
predict_streams <- function(model, history, days_ahead = 30) {
# Prepare history if needed
if (!"rolling_7day" %in% names(history)) {
  history <- prepare_model_features(history)
}

# Get last known values
last_row <- history[nrow(history), ]
last_date <- max(history$date)

# Generate future dates
future_dates <- seq(last_date + 1, last_date + days_ahead, by = "day")

# Create future features
future_features <- create_temporal_features(future_dates)

# For rolling stats and lags, we need to use last known values
# and potentially update as we predict forward
future_features$rolling_7day <- last_row$rolling_7day
future_features$rolling_14day <- last_row$rolling_14day
future_features$rolling_30day <- last_row$rolling_30day
future_features$momentum_7_30 <- last_row$momentum_7_30
future_features$pct_change_7 <- last_row$pct_change_7

# Make predictions
method <- attr(model, "method")

if (method == "lm") {
  pred <- stats::predict(model, newdata = future_features,
                         interval = "prediction", level = 0.95)
  tibble::tibble(
    date = future_dates,
    predicted_streams = pred[, "fit"],
    lower_ci = pred[, "lwr"],
    upper_ci = pred[, "upr"]
  )
} else {
  # GAM prediction
  pred <- stats::predict(model, newdata = future_features, se.fit = TRUE)
  tibble::tibble(
    date = future_dates,
    predicted_streams = pred$fit,
    lower_ci = pred$fit - 1.96 * pred$se.fit,
    upper_ci = pred$fit + 1.96 * pred$se.fit
  )
}
}

#' Evaluate model performance
#'
#' Calculates error metrics for model predictions vs actual values.
#'
#' @param actual Numeric vector of actual stream values
#' @param predicted Numeric vector of predicted stream values
#' @return Named list with MAE, RMSE, MAPE metrics
#' @export
evaluate_model <- function(actual, predicted) {
# Remove NAs
valid <- !is.na(actual) & !is.na(predicted)
actual <- actual[valid]
predicted <- predicted[valid]

errors <- actual - predicted

list(
  mae = mean(abs(errors)),
  rmse = sqrt(mean(errors^2)),
  mape = mean(abs(errors / actual)) * 100,
  n = length(errors)
)
}

#' Analyze day of week patterns
#'
#' Calculates average stream multipliers by day of week.
#'
#' @param history tibble with date and streams columns
#' @return tibble with day_of_week, avg_streams, multiplier
#' @export
#' @examples
#' \dontrun{
#' history <- load_track_history("53iuhJlwXhSER5J2IYYv1W")
#' dow_patterns <- analyze_dow_patterns(history)
#' }
analyze_dow_patterns <- function(history) {
# Aggregate to global if per-country
if ("country" %in% names(history)) {
  history <- aggregate_global_streams(history)
}

# Add day of week
history$day_of_week <- lubridate::wday(history$date, week_start = 1)

# Calculate overall mean
overall_mean <- mean(history$streams, na.rm = TRUE)

# Calculate by day of week
dow_summary <- history |>
  dplyr::group_by(day_of_week) |>
  dplyr::summarise(
    avg_streams = mean(streams, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    multiplier = avg_streams / overall_mean,
    day_name = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[day_of_week]
  )

dow_summary
}

#' Analyze monthly patterns
#'
#' Calculates average stream patterns by day of month.
#'
#' @param history tibble with date and streams columns
#' @return tibble with day_of_month, avg_streams, multiplier
#' @export
analyze_monthly_patterns <- function(history) {
if ("country" %in% names(history)) {
  history <- aggregate_global_streams(history)
}

history$day_of_month <- lubridate::mday(history$date)
overall_mean <- mean(history$streams, na.rm = TRUE)

history |>
  dplyr::group_by(day_of_month) |>
  dplyr::summarise(
    avg_streams = mean(streams, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    multiplier = avg_streams / overall_mean
  )
}
