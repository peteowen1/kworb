# Stream Projection Analysis
# Demonstrates forecasting Spotify track streams using historical patterns
#
# This script shows how to:
# - Load track streaming data
# - Engineer features (rolling averages, temporal, year-over-year)
# - Explore patterns (day of week, seasonal, YoY for Christmas songs)
# - Build projection models (LM and GAM)
# - Generate and evaluate forecasts

library(kworb)
library(dplyr)

# =============================================================================
# 1. SETUP & DATA LOADING
# =============================================================================

# Configuration
REPO <- "peteowen1/kworb"
RELEASE_TAG <- "data"

# Try to load from GitHub Releases first, fall back to scraping
message("Attempting to load data from GitHub Releases...")

release_files <- tryCatch({
  list_release_files(REPO, RELEASE_TAG)
}, error = function(e) {
  message("Could not access releases: ", e$message)
  tibble::tibble()
})

if (nrow(release_files) > 0) {
  message(sprintf("Found %d files in release", nrow(release_files)))

  # Load charts and tracks from release
  charts <- load_charts_from_release(REPO, RELEASE_TAG)
  tracks <- load_tracks_from_release(REPO, "all", RELEASE_TAG)

  message(sprintf("Loaded %d chart rows, %d track history rows",
                  nrow(charts), nrow(tracks)))

  # Use the first track with sufficient history for analysis
  if (nrow(tracks) > 0) {
    track_counts <- tracks |>
      group_by(track_id) |>
      summarise(n_days = n_distinct(date), .groups = "drop") |>
      arrange(desc(n_days))

    # Pick track with most history
    track_id <- track_counts$track_id[1]
    history_raw <- tracks |> filter(track_id == !!track_id)
    message(sprintf("Using track %s with %d days of history", track_id, nrow(history_raw)))
  } else {
    message("No track data in release, will scrape fresh data")
    release_files <- tibble::tibble()  # Fall through to scraping
  }
}

# If no release data, scrape a sample track
if (nrow(release_files) == 0) {
  # Example: "All I Want for Christmas Is You" by Mariah Carey
  # Track ID: 0bYg9bo50gSsH3LtXe2SQn (a classic seasonal song)
  track_id <- "0bYg9bo50gSsH3LtXe2SQn"

  message(sprintf("Scraping track history for %s...", track_id))
  history_raw <- scrape_track_history(track_id, view = "daily")

  message(sprintf("Loaded %d rows of daily per-country data", nrow(history_raw)))
}

# Show data structure
head(history_raw)

# Aggregate to global streams (if per-country data)
if ("country" %in% names(history_raw)) {
  history <- aggregate_global_streams(history_raw)
  message(sprintf("Aggregated to %d days of global data", nrow(history)))
} else {
  history <- history_raw
  message(sprintf("Using %d days of data", nrow(history)))
}

# =============================================================================
# 2. FEATURE ENGINEERING
# =============================================================================

# Add rolling statistics (7, 14, 30 day averages)
history <- calculate_rolling_stats(history)

# Add temporal features (day of week, month, etc.)
temporal_features <- create_temporal_features(history$date)
history <- left_join(history, temporal_features, by = "date")

# Add year-over-year features (same day last year, seasonal flags)
history <- add_yoy_features(history)

# Preview features
message("\nFeature summary:")
glimpse(history)

# =============================================================================
# 3. EXPLORE PATTERNS
# =============================================================================

# --- Day of Week Patterns ---
dow_patterns <- analyze_dow_patterns(history)
message("\nDay of Week Patterns:")
print(dow_patterns)

# Interpretation: multiplier > 1 means more streams than average
# Fridays typically spike due to new music releases

# --- Monthly Patterns ---
monthly_patterns <- analyze_monthly_patterns(history)
message("\nDay of Month Patterns (sample):")
print(head(monthly_patterns, 10))

# --- Seasonal Patterns ---
seasonal_patterns <- analyze_seasonal_patterns(history, smooth = TRUE)
message("\nSeasonal Patterns (sample - Dec/Jan peak for Christmas songs):")
# Show December and January (days 335-366 and 1-31)
print(seasonal_patterns |>
        filter(day_of_year >= 335 | day_of_year <= 31) |>
        arrange(desc(multiplier)))

# --- Year-over-Year Analysis ---
message("\nYear-over-Year Summary:")
yoy_summary <- history |>
  filter(!is.na(yoy_ratio)) |>
  summarise(
    avg_yoy_ratio = mean(yoy_ratio, na.rm = TRUE),
    median_yoy_ratio = median(yoy_ratio, na.rm = TRUE),
    min_yoy = min(yoy_ratio, na.rm = TRUE),
    max_yoy = max(yoy_ratio, na.rm = TRUE),
    n_days_with_yoy = n()
  )
print(yoy_summary)

# Christmas season analysis (if it's a Christmas song)
if (any(history$is_christmas_season, na.rm = TRUE)) {
  message("\nChristmas Season Performance:")
  christmas_summary <- history |>
    group_by(is_christmas_season) |>
    summarise(
      avg_streams = mean(streams, na.rm = TRUE),
      median_streams = median(streams, na.rm = TRUE),
      n_days = n(),
      .groups = "drop"
    ) |>
    mutate(
      season_label = if_else(is_christmas_season, "Christmas Season", "Regular"),
      multiplier = avg_streams / min(avg_streams)
    )
  print(christmas_summary)
}

# =============================================================================
# 4. BUILD PROJECTION MODELS
# =============================================================================

# Prepare full feature set
model_data <- prepare_model_features(history)

# Filter to complete cases only
model_data_complete <- model_data |>
  filter(!is.na(rolling_30day) & !is.na(streams))

message(sprintf("\nModel data: %d complete observations", nrow(model_data_complete)))

# --- Linear Model ---
message("\nBuilding Linear Model...")
lm_model <- build_projection_model(model_data_complete, method = "lm")
lm_summary <- summary(lm_model)
message(sprintf("LM R-squared: %.3f", lm_summary$r.squared))

# Show coefficients
message("\nTop predictors:")
coefs <- broom::tidy(lm_model) |>
  filter(p.value < 0.05) |>
  arrange(desc(abs(estimate)))
print(head(coefs, 10))

# --- GAM Model (if mgcv available) ---
if (requireNamespace("mgcv", quietly = TRUE)) {
  message("\nBuilding GAM Model...")
  gam_model <- build_projection_model(model_data_complete, method = "gam")
  gam_summary <- summary(gam_model)
  message(sprintf("GAM Deviance explained: %.1f%%", gam_summary$dev.expl * 100))
}

# =============================================================================
# 5. GENERATE FORECASTS
# =============================================================================

# Generate 30-day forecast using LM
message("\nGenerating 30-day forecast...")
forecast_lm <- predict_streams(lm_model, model_data_complete, days_ahead = 30)

message("\nForecast summary:")
print(forecast_lm)

# Calculate expected totals
message(sprintf("\nExpected streams next 7 days: %s",
                format(sum(forecast_lm$predicted_streams[1:7]), big.mark = ",")))
message(sprintf("Expected streams next 14 days: %s",
                format(sum(forecast_lm$predicted_streams[1:14]), big.mark = ",")))
message(sprintf("Expected streams next 30 days: %s",
                format(sum(forecast_lm$predicted_streams), big.mark = ",")))

# =============================================================================
# 6. MODEL EVALUATION
# =============================================================================

# Holdout evaluation - use last 14 days as test set
if (nrow(model_data_complete) > 44) {
  train_data <- head(model_data_complete, -14)
  test_data <- tail(model_data_complete, 14)

  # Refit on training data
  train_model <- build_projection_model(train_data, method = "lm")

  # Predict on holdout
  test_features <- test_data |>
    select(date, day_of_week, rolling_7day, rolling_14day, rolling_30day,
           momentum_7_30, pct_change_7)

  test_pred <- predict(train_model, newdata = test_features)

  # Evaluate
  metrics <- evaluate_model(test_data$streams, test_pred)

  message("\nHoldout Evaluation (last 14 days):")
  message(sprintf("  MAE:  %s streams", format(round(metrics$mae), big.mark = ",")))
  message(sprintf("  RMSE: %s streams", format(round(metrics$rmse), big.mark = ",")))
  message(sprintf("  MAPE: %.1f%%", metrics$mape))
}

# =============================================================================
# 7. VISUALIZATION (if ggplot2 available)
# =============================================================================

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # --- Plot 1: Historical streams with rolling averages ---
  p1 <- ggplot(history, aes(x = date)) +
    geom_line(aes(y = streams), alpha = 0.3, color = "gray50") +
    geom_line(aes(y = rolling_7day), color = "blue", linewidth = 0.8) +
    geom_line(aes(y = rolling_30day), color = "red", linewidth = 0.8) +
    labs(
      title = "Daily Streams with Rolling Averages",
      subtitle = "Blue = 7-day, Red = 30-day",
      x = "Date", y = "Streams"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()

  print(p1)

  # --- Plot 2: Day of Week patterns ---
  p2 <- ggplot(dow_patterns, aes(x = day_name, y = multiplier)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = "Day of Week Streaming Patterns",
      subtitle = "Multiplier relative to overall average (1.0 = average)",
      x = "Day of Week", y = "Multiplier"
    ) +
    scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
    theme_minimal()

  print(p2)

  # --- Plot 3: Seasonal pattern (day of year) ---
  if ("multiplier_smooth" %in% names(seasonal_patterns)) {
    p3 <- ggplot(seasonal_patterns, aes(x = day_of_year, y = multiplier_smooth)) +
      geom_line(color = "darkgreen", linewidth = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      labs(
        title = "Seasonal Streaming Pattern",
        subtitle = "Smoothed multiplier by day of year",
        x = "Day of Year", y = "Multiplier"
      ) +
      theme_minimal()

    print(p3)
  }

  # --- Plot 4: Forecast with confidence intervals ---
  p4 <- ggplot(forecast_lm, aes(x = date)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.5) +
    geom_line(aes(y = predicted_streams), color = "blue", linewidth = 1) +
    labs(
      title = "30-Day Stream Forecast",
      subtitle = "With 95% prediction intervals",
      x = "Date", y = "Predicted Streams"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()

  print(p4)

  # --- Plot 5: Year-over-year comparison (if data available) ---
  yoy_data <- history |>
    filter(!is.na(yoy_ratio)) |>
    filter(is_christmas_season)

  if (nrow(yoy_data) > 10) {
    p5 <- ggplot(yoy_data, aes(x = date, y = yoy_ratio)) +
      geom_line(color = "purple", linewidth = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      labs(
        title = "Year-over-Year Ratio (Christmas Season)",
        subtitle = ">1 means growing vs last year, <1 means declining",
        x = "Date", y = "YoY Ratio"
      ) +
      theme_minimal()

    print(p5)
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("ANALYSIS COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("Track ID: %s", track_id))
message(sprintf("Data range: %s to %s", min(history$date), max(history$date)))
message(sprintf("Total days: %d", nrow(history)))
message(sprintf("Model R-squared: %.3f", lm_summary$r.squared))
message(paste(rep("=", 60), collapse = ""))
