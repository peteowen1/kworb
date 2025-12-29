# Track history scraping functions

#' Scrape track streaming history from kworb.net
#'
#' Scrapes the historical streaming data for a specific track, returning
#' per-country daily or weekly stream counts.
#'
#' @param track_id Spotify track ID (22 character alphanumeric string)
#' @param view Data view: "daily" or "weekly" (default "daily")
#' @return A tibble with columns:
#'   - `date`: Date of the data point
#'   - `country`: Country code
#'   - `streams`: Stream count for that country/date
#'   - `track_id`: Spotify track ID
#' @export
#' @examples
#' \dontrun{
#' # Scrape daily history for a track
#' history <- scrape_track_history("53iuhJlwXhSER5J2IYYv1W")
#'
#' # Scrape weekly history
#' weekly <- scrape_track_history("53iuhJlwXhSER5J2IYYv1W", view = "weekly")
#' }
scrape_track_history <- function(track_id, view = "daily") {
# Validate inputs
view <- match.arg(view, c("daily", "weekly"))

if (!validate_spotify_id(track_id)) {
  stop("Invalid Spotify track ID: ", track_id)
}

# Build URL
url <- build_kworb_url("track", track_id)

# Fetch page
html <- polite_read_html(url)
if (is.null(html)) {
  stop(sprintf("Failed to fetch track history for %s", track_id))
}

# Parse the history table
history_data <- parse_track_history(html, view)

# Add track ID
history_data$track_id <- track_id

history_data
}

#' Parse track history HTML
#'
#' Internal function to extract streaming history from track page HTML.
#' The page structure has daily/weekly classes on span elements inside cells,
#' not on the rows themselves.
#'
#' @param html rvest html_document
#' @param view "daily" or "weekly"
#' @return tibble of streaming history
#' @keywords internal
parse_track_history <- function(html, view = "daily") {
# The page has TWO tables:
# - Table 1: Weekly data
# - Table 2: Daily data
all_tables <- rvest::html_elements(html, "table")

if (length(all_tables) == 0) {
  stop("Could not find any tables in HTML")
}

# Select the appropriate table based on view
if (view == "daily" && length(all_tables) >= 2) {
  table <- all_tables[[2]]  # Daily data is in second table
} else {
  table <- all_tables[[1]]  # Weekly data (or fallback)
}

# Get all rows
all_rows <- rvest::html_elements(table, "tr")
if (length(all_rows) < 2) {
  return(tibble::tibble(
    date = as.Date(character()),
    country = character(),
    streams = numeric()
  ))
}

# Get header row to find country columns
header_cells <- rvest::html_elements(all_rows[[1]], "th, td")
header_text <- purrr::map_chr(header_cells, ~ trimws(rvest::html_text(.x)))

# Country codes are in header (skip first column which is "Date")
# Header format: "Date", "Global", "US", "DE", "GB", ...
country_codes <- header_text[-1]
# Keep entries that look like country codes (2-6 chars, letters only)
# "Global" is also valid
country_codes <- country_codes[nchar(country_codes) >= 2 & nchar(country_codes) <= 6]
country_codes <- country_codes[grepl("^[A-Za-z]+$", country_codes)]

if (length(country_codes) == 0) {
  warning("Could not parse country codes from header")
  return(tibble::tibble(
    date = as.Date(character()),
    country = character(),
    streams = numeric()
  ))
}

# Parse data rows (skip header, and skip Total/Peak rows)
# Rows are: [1]=header, [2]=Total, [3]=Peak, [4+]=dates
data_rows <- all_rows[-(1:3)]  # Skip header, Total, and Peak rows
results <- list()

for (row in data_rows) {
  cells <- rvest::html_elements(row, "td")
  if (length(cells) < 2) next

  # Get all cell text
  cell_text <- purrr::map_chr(cells, ~ trimws(rvest::html_text(.x)))

  # First cell is the date (format: YYYY/MM/DD)
  date_str <- cell_text[1]
  date <- tryCatch({
    as.Date(date_str, format = "%Y/%m/%d")
  }, error = function(e) NA)

  if (is.na(date)) next  # Skip non-date rows

  # Parse each country column
  # Cell format is "position (streams)" like "1 (128,896,137)"
  for (i in seq_along(country_codes)) {
    cell_idx <- i + 1
    if (cell_idx > length(cell_text)) break

    country <- country_codes[i]
    cell_value <- cell_text[cell_idx]

    if (cell_value == "" || cell_value == "-") next

    # Parse the cell - format is "position (streams)"
    parsed <- parse_position_streams(cell_value)

    if (!is.na(parsed$streams) && parsed$streams > 0) {
      results[[length(results) + 1]] <- tibble::tibble(
        date = date,
        country = country,
        streams = parsed$streams
      )
    }
  }
}

if (length(results) == 0) {
  return(tibble::tibble(
    date = as.Date(character()),
    country = character(),
    streams = numeric()
  ))
}

dplyr::bind_rows(results)
}

#' Get track metadata
#'
#' Fetches metadata for a track including title, artist, and total streams.
#'
#' @param track_id Spotify track ID
#' @return A tibble with one row containing:
#'   - `track_id`: Spotify track ID
#'   - `title`: Track title
#'   - `artist`: Artist name(s)
#'   - `artist_id`: Primary artist's Spotify ID
#'   - `total_streams`: Total cumulative streams
#' @export
#' @examples
#' \dontrun{
#' metadata <- get_track_metadata("53iuhJlwXhSER5J2IYYv1W")
#' }
get_track_metadata <- function(track_id) {
if (!validate_spotify_id(track_id)) {
  stop("Invalid Spotify track ID: ", track_id)
}

url <- build_kworb_url("track", track_id)
html <- polite_read_html(url)

if (is.null(html)) {
  stop(sprintf("Failed to fetch track metadata for %s", track_id))
}

# Extract title - usually in an h1 or similar element
title <- tryCatch({
  title_elem <- rvest::html_element(html, "h1, .title, title")
  if (!is.na(title_elem)) {
    trimws(rvest::html_text(title_elem))
  } else {
    NA_character_
  }
}, error = function(e) NA_character_)

# Extract artist info from links
artist_links <- rvest::html_elements(html, "a[href*='/artist/']")
artist_name <- NA_character_
artist_id <- NA_character_

if (length(artist_links) > 0) {
  artist_name <- trimws(rvest::html_text(artist_links[[1]]))
  artist_id <- extract_spotify_id(rvest::html_attr(artist_links[[1]], "href"))
}

# Extract total streams - look for large numbers in the page
# This is often in a summary section
total_streams <- tryCatch({
  # Look for text containing "Total:" or similar
  text <- rvest::html_text(html)
  matches <- regmatches(text, gregexpr("[\\d,]+", text))[[1]]
  # Find largest number (likely total streams)
  nums <- purrr::map_dbl(matches, ~ parse_streams(.x))
  max(nums, na.rm = TRUE)
}, error = function(e) NA_real_)

tibble::tibble(
  track_id = track_id,
  title = title,
  artist = artist_name,
  artist_id = artist_id,
  total_streams = total_streams
)
}

#' Scrape history for multiple tracks
#'
#' Batch scrapes streaming history for multiple tracks with rate limiting
#' and progress reporting.
#'
#' @param track_ids Character vector of Spotify track IDs
#' @param view "daily" or "weekly"
#' @param progress Show progress messages (default TRUE)
#' @param delay Seconds between requests (default 1)
#' @return tibble with all track histories combined
#' @export
#' @examples
#' \dontrun{
#' # Get chart and scrape all tracks
#' chart <- scrape_chart()
#' histories <- scrape_all_tracks(chart$track_id[1:10])
#' }
scrape_all_tracks <- function(track_ids, view = "daily", progress = TRUE, delay = 1) {
track_ids <- unique(track_ids)
n_tracks <- length(track_ids)
results <- list()
failed <- character()

for (i in seq_along(track_ids)) {
  track_id <- track_ids[i]

  if (progress && (i %% 10 == 0 || i == 1 || i == n_tracks)) {
    message(sprintf("[%d/%d] Scraping track %s...", i, n_tracks, track_id))
  }

  tryCatch({
    history <- scrape_track_history(track_id, view = view)
    results[[track_id]] <- history

    # Extra delay every 50 requests
    if (i %% 50 == 0) {
      Sys.sleep(5)
    }
  }, error = function(e) {
    warning(sprintf("Failed to scrape track %s: %s", track_id, e$message))
    failed <<- c(failed, track_id)
  })
}

if (length(failed) > 0 && progress) {
  message(sprintf("Failed to scrape %d tracks", length(failed)))
}

dplyr::bind_rows(results)
}

#' Debug track page HTML structure
#'
#' Fetches a track page and returns information about its HTML structure
#' to help debug parsing issues.
#'
#' @param track_id Spotify track ID
#' @return List with debug information about the page structure
#' @export
debug_track_page <- function(track_id) {
url <- build_kworb_url("track", track_id)
html <- polite_read_html(url, delay = 0)

if (is.null(html)) {
  return(list(error = "Failed to fetch page"))
}

# Find ALL tables on the page
all_tables <- rvest::html_elements(html, "table")

# Look for elements with daily/weekly classes
daily_elements <- rvest::html_elements(html, ".daily")
weekly_elements <- rvest::html_elements(html, ".weekly")

# Get the first table
table <- all_tables[[1]]
all_rows <- rvest::html_elements(table, "tr")

# Check row classes
row_classes <- purrr::map_chr(all_rows, ~ {
  cls <- rvest::html_attr(.x, "class")
  if (is.na(cls)) "none" else cls
})

# Get header info
header_cells <- rvest::html_elements(all_rows[[1]], "th, td")
header_text <- purrr::map_chr(header_cells, ~ trimws(rvest::html_text(.x)))

# Get first few data rows
sample_rows <- list()
for (i in 2:min(5, length(all_rows))) {
  row <- all_rows[[i]]
  cells <- rvest::html_elements(row, "td")
  cell_text <- purrr::map_chr(cells, ~ trimws(rvest::html_text(.x)))
  sample_rows[[i-1]] <- list(
    text = cell_text,
    n_cells = length(cells),
    row_class = row_classes[i]
  )
}

# Info about each table
table_info <- list()
for (t in seq_along(all_tables)) {
  tbl <- all_tables[[t]]
  tbl_rows <- rvest::html_elements(tbl, "tr")
  tbl_class <- rvest::html_attr(tbl, "class")

  # Get first data row date (if any)
  first_date <- NA
  if (length(tbl_rows) >= 4) {
    cells <- rvest::html_elements(tbl_rows[[4]], "td")
    if (length(cells) > 0) {
      first_date <- trimws(rvest::html_text(cells[[1]]))
    }
  }

  table_info[[t]] <- list(
    table_class = if(is.na(tbl_class)) "none" else tbl_class,
    n_rows = length(tbl_rows),
    first_date = first_date
  )
}

list(
  n_tables = length(all_tables),
  n_daily_elements = length(daily_elements),
  n_weekly_elements = length(weekly_elements),
  table_info = table_info,
  row_classes = unique(row_classes),
  header = header_text,
  sample_rows = sample_rows
)
}

#' Aggregate track streams to global totals
#'
#' Sums per-country streams to get global daily totals.
#'
#' @param history tibble from `scrape_track_history()` with per-country data
#' @return tibble with columns: date, streams, track_id
#' @export
#' @examples
#' \dontrun{
#' history <- scrape_track_history("53iuhJlwXhSER5J2IYYv1W")
#' global <- aggregate_global_streams(history)
#' }
aggregate_global_streams <- function(history) {
history |>
  dplyr::group_by(date, track_id) |>
  dplyr::summarise(
    streams = sum(streams, na.rm = TRUE),
    .groups = "drop"
  )
}
