# Data storage functions for local persistence

#' Save chart data to CSV
#'
#' Saves chart data to a dated CSV file. Creates the directory if needed.
#'
#' @param data tibble from `scrape_chart()`
#' @param path Directory path for storage (default "data/charts")
#' @param date Date for filename (default: today or from data$scrape_date)
#' @return Invisible path to saved file
#' @export
#' @examples
#' \dontrun{
#' chart <- scrape_chart()
#' save_chart_data(chart)
#' }
save_chart_data <- function(data, path = "data/charts", date = NULL) {
# Create directory if needed
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

# Determine date for filename
if (is.null(date)) {
  if ("scrape_date" %in% names(data)) {
    date <- data$scrape_date[1]
  } else {
    date <- Sys.Date()
  }
}

# Determine country if present
country <- "global"
if ("country" %in% names(data)) {
  country <- unique(data$country)[1]
}

# Build filename
filename <- sprintf("%s_daily_%s.csv", country, format(date, "%Y-%m-%d"))
filepath <- file.path(path, filename)

# Save
readr::write_csv(data, filepath)
message(sprintf("Saved chart data to %s", filepath))

invisible(filepath)
}

#' Load historical chart data
#'
#' Loads all chart CSV files from a directory, optionally filtering by date.
#'
#' @param path Directory path containing chart CSVs
#' @param start_date Optional start date filter (inclusive)
#' @param end_date Optional end date filter (inclusive)
#' @param country Optional country code filter
#' @return tibble of combined chart data
#' @export
#' @examples
#' \dontrun{
#' # Load all chart data
#' all_charts <- load_chart_data()
#'
#' # Load last 7 days
#' recent <- load_chart_data(start_date = Sys.Date() - 7)
#' }
load_chart_data <- function(path = "data/charts", start_date = NULL,
                           end_date = NULL, country = NULL) {
if (!dir.exists(path)) {
  warning(sprintf("Directory does not exist: %s", path))
  return(tibble::tibble())
}

# Find all CSV files
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

if (length(files) == 0) {
  return(tibble::tibble())
}

# Filter by country if specified
if (!is.null(country)) {
  pattern <- sprintf("^%s_", country)
  files <- files[grepl(pattern, basename(files))]
}

# Filter by date if specified
if (!is.null(start_date) || !is.null(end_date)) {
  # Extract dates from filenames
  file_dates <- regmatches(basename(files),
                           regexec("(\\d{4}-\\d{2}-\\d{2})\\.csv$", basename(files)))
  file_dates <- purrr::map_chr(file_dates, ~ if(length(.x) >= 2) .x[2] else NA_character_)
  file_dates <- as.Date(file_dates)

  keep <- rep(TRUE, length(files))
  if (!is.null(start_date)) {
    keep <- keep & (file_dates >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    keep <- keep & (file_dates <= as.Date(end_date))
  }
  files <- files[keep]
}

if (length(files) == 0) {
  return(tibble::tibble())
}

# Read and combine
data <- purrr::map(files, ~ readr::read_csv(.x, show_col_types = FALSE))
dplyr::bind_rows(data)
}

#' Save track history data
#'
#' Saves or appends track history data. Handles deduplication automatically.
#'
#' @param data tibble from `scrape_track_history()`
#' @param track_id Spotify track ID (used for filename if not in data)
#' @param path Directory path for storage (default "data/tracks")
#' @return Invisible path to saved file
#' @export
#' @examples
#' \dontrun{
#' history <- scrape_track_history("53iuhJlwXhSER5J2IYYv1W")
#' save_track_history(history)
#' }
save_track_history <- function(data, track_id = NULL, path = "data/tracks") {
# Create directory if needed
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

# Get track_id from data if not provided
if (is.null(track_id)) {
  if ("track_id" %in% names(data)) {
    track_id <- unique(data$track_id)[1]
  } else {
    stop("track_id must be provided or present in data")
  }
}

# Build filepath
filepath <- file.path(path, sprintf("%s.csv", track_id))

# Load existing data if present
if (file.exists(filepath)) {
  existing <- readr::read_csv(filepath, show_col_types = FALSE)
  data <- merge_data(existing, data, c("date", "country", "track_id"))
}

# Save
readr::write_csv(data, filepath)

invisible(filepath)
}

#' Load track history data
#'
#' Loads track history from CSV file(s).
#'
#' @param track_id Spotify track ID, or "all" to load all tracks
#' @param path Directory path containing track CSVs
#' @return tibble of track history
#' @export
#' @examples
#' \dontrun{
#' # Load single track
#' history <- load_track_history("53iuhJlwXhSER5J2IYYv1W")
#'
#' # Load all tracks
#' all_history <- load_track_history("all")
#' }
load_track_history <- function(track_id, path = "data/tracks") {
if (!dir.exists(path)) {
  warning(sprintf("Directory does not exist: %s", path))
  return(tibble::tibble())
}

if (track_id == "all") {
  # Load all track files
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    return(tibble::tibble())
  }
  data <- purrr::map(files, ~ readr::read_csv(.x, show_col_types = FALSE))
  return(dplyr::bind_rows(data))
}

# Load single track
filepath <- file.path(path, sprintf("%s.csv", track_id))

if (!file.exists(filepath)) {
  return(tibble::tibble(
    date = as.Date(character()),
    country = character(),
    streams = numeric(),
    track_id = character()
  ))
}

readr::read_csv(filepath, show_col_types = FALSE)
}

#' Merge new data with existing, handling deduplication
#'
#' Combines two data frames and removes duplicates based on key columns.
#'
#' @param existing Existing tibble
#' @param new New tibble to merge
#' @param key_cols Character vector of columns that define unique rows
#' @return Merged tibble without duplicates
#' @export
#' @examples
#' \dontrun{
#' merged <- merge_data(existing_history, new_history, c("date", "country", "track_id"))
#' }
merge_data <- function(existing, new, key_cols) {
if (nrow(existing) == 0) {
  return(new)
}
if (nrow(new) == 0) {
  return(existing)
}

# Ensure key columns exist in both
missing_in_existing <- setdiff(key_cols, names(existing))
missing_in_new <- setdiff(key_cols, names(new))

if (length(missing_in_existing) > 0 || length(missing_in_new) > 0) {
  warning("Key columns missing in one or both data frames")
  return(dplyr::bind_rows(existing, new))
}

# Combine and deduplicate
combined <- dplyr::bind_rows(existing, new)
dplyr::distinct(combined, dplyr::across(dplyr::all_of(key_cols)), .keep_all = TRUE)
}

#' List available track IDs in storage
#'
#' Returns track IDs for which we have stored history.
#'
#' @param path Directory path containing track CSVs
#' @return Character vector of track IDs
#' @export
list_stored_tracks <- function(path = "data/tracks") {
if (!dir.exists(path)) {
  return(character())
}

files <- list.files(path, pattern = "\\.csv$")
gsub("\\.csv$", "", files)
}

#' Get storage summary
#'
#' Returns summary statistics about stored data.
#'
#' @param base_path Base data directory
#' @return List with storage statistics
#' @export
get_storage_summary <- function(base_path = "data") {
chart_path <- file.path(base_path, "charts")
tracks_path <- file.path(base_path, "tracks")

chart_files <- if (dir.exists(chart_path)) {
  list.files(chart_path, pattern = "\\.csv$")
} else {
  character()
}

track_files <- if (dir.exists(tracks_path)) {
  list.files(tracks_path, pattern = "\\.csv$")
} else {
  character()
}

list(
  chart_days = length(chart_files),
  track_count = length(track_files),
  chart_path = chart_path,
  tracks_path = tracks_path
)
}
