#!/usr/bin/env Rscript
# Daily scrape script for GitHub Actions
# This script is run automatically by the daily-scrape.yml workflow

library(kworb)

# Configuration
DATA_PATH <- "data"
CHARTS_PATH <- file.path(DATA_PATH, "charts")
TRACKS_PATH <- file.path(DATA_PATH, "tracks")

# Create directories if needed
dir.create(CHARTS_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(TRACKS_PATH, recursive = TRUE, showWarnings = FALSE)

# Timestamp for logging
message(sprintf("Starting daily scrape at %s", Sys.time()))

# 1. Scrape daily chart
message("\n=== Scraping Global Daily Chart ===")
chart_data <- tryCatch({
  scrape_chart(country = "global", frequency = "daily")
}, error = function(e) {
  stop(sprintf("Failed to scrape chart: %s", e$message))
})

message(sprintf("Found %d tracks on chart", nrow(chart_data)))

# Save chart data
save_chart_data(chart_data, path = CHARTS_PATH)

# 2. Get all track IDs from chart
track_ids <- unique(chart_data$track_id)
track_ids <- track_ids[!is.na(track_ids)]
message(sprintf("\n=== Scraping History for %d Tracks ===", length(track_ids)))

# Track progress
successful <- 0
failed <- 0
failed_ids <- character()

# Scrape each track with rate limiting
for (i in seq_along(track_ids)) {
  track_id <- track_ids[i]

  # Progress message every 25 tracks
  if (i %% 25 == 0 || i == 1) {
    message(sprintf("[%d/%d] Processing tracks... (%.1f%% complete)",
                    i, length(track_ids), 100 * i / length(track_ids)))
  }

  result <- tryCatch({
    # Check if we already have recent data for this track
    existing <- load_track_history(track_id, path = TRACKS_PATH)

    # Scrape new data
    history <- scrape_track_history(track_id, view = "daily")

    if (nrow(history) > 0) {
      # Save (will merge with existing)
      save_track_history(history, track_id, path = TRACKS_PATH)
      successful <<- successful + 1
    }

    TRUE
  }, error = function(e) {
    failed <<- failed + 1
    failed_ids <<- c(failed_ids, track_id)
    FALSE
  })

  # Extra pause every 50 tracks to be polite
  if (i %% 50 == 0) {
    Sys.sleep(3)
  }
}

# Summary
message("\n=== Scrape Complete ===")
message(sprintf("Successful: %d tracks", successful))
message(sprintf("Failed: %d tracks", failed))

if (length(failed_ids) > 0) {
  message(sprintf("Failed track IDs: %s", paste(head(failed_ids, 10), collapse = ", ")))
  if (length(failed_ids) > 10) {
    message(sprintf("  ... and %d more", length(failed_ids) - 10))
  }
}

# Save summary
summary <- list(
  scrape_date = Sys.Date(),
  scrape_time = Sys.time(),
  chart_tracks = nrow(chart_data),
  successful_tracks = successful,
  failed_tracks = failed
)

summary_file <- file.path(DATA_PATH, "last_scrape.json")
jsonlite::write_json(summary, summary_file, auto_unbox = TRUE, pretty = TRUE)

message(sprintf("\nFinished at %s", Sys.time()))
