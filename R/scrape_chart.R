# Chart scraping functions

#' Scrape Spotify daily chart from kworb.net
#'
#' Scrapes the global or country-specific daily chart and returns a tibble
#' with track information and streaming data.
#'
#' @param country Country code (default "global"). Use `get_country_codes()` for options.
#' @param frequency Chart frequency: "daily" or "weekly" (default "daily")
#' @return A tibble with columns:
#'   - `pos`: Current chart position
#'   - `pos_change`: Position change indicator (e.g., "NEW", "+5", "-2")
#'   - `artist`: Artist name(s)
#'   - `artist_id`: Spotify artist ID (first artist if multiple)
#'   - `title`: Track title
#'   - `track_id`: Spotify track ID
#'   - `days`: Days on chart
#'   - `peak`: Peak position
#'   - `streams_daily`: Daily streams
#'   - `streams_change`: Change in daily streams
#'   - `streams_7day`: 7-day total streams
#'   - `streams_7day_change`: Change in 7-day streams
#'   - `streams_total`: Total cumulative streams
#'   - `scrape_date`: Date of scrape
#' @export
#' @examples
#' \dontrun{
#' # Scrape global daily chart
#' chart <- scrape_chart()
#'
#' # Scrape US weekly chart
#' us_chart <- scrape_chart(country = "us", frequency = "weekly")
#' }
scrape_chart <- function(country = "global", frequency = "daily") {
# Validate inputs
frequency <- match.arg(frequency, c("daily", "weekly"))

# Build URL
url <- build_kworb_url("chart", country, frequency)
message(sprintf("Scraping %s %s chart...", country, frequency))

# Fetch page
html <- polite_read_html(url)
if (is.null(html)) {
  stop(sprintf("Failed to fetch chart for %s", country))
}

# Parse the table
chart_data <- parse_chart_table(html)

# Add metadata
chart_data$scrape_date <- Sys.Date()

chart_data
}

#' Parse chart HTML table
#'
#' Internal function to extract data from the chart table HTML.
#'
#' @param html rvest html_document
#' @return tibble of chart data
#' @keywords internal
parse_chart_table <- function(html) {
# Find the main table
table <- rvest::html_element(html, "table")
if (is.na(table)) {
  stop("Could not find chart table in HTML")
}

# Get all rows (skip header)
rows <- rvest::html_elements(table, "tbody tr")
if (length(rows) == 0) {
  # Try without tbody
  rows <- rvest::html_elements(table, "tr")[-1]  # Skip header row
}

if (length(rows) == 0) {
  warning("No data rows found in chart table")
  return(tibble::tibble())
}

# Parse each row
parsed_rows <- purrr::map(rows, parse_chart_row)

# Combine into tibble
dplyr::bind_rows(parsed_rows)
}

#' Parse a single chart row
#'
#' @param row rvest html_element for a table row
#' @return tibble with one row of data
#' @keywords internal
parse_chart_row <- function(row) {
cells <- rvest::html_elements(row, "td")

if (length(cells) < 7) {
  return(NULL)
}

# Extract cell text
cell_text <- purrr::map_chr(cells, ~ trimws(rvest::html_text(.x)))

# Extract links from the artist/title cell (usually cell 3)
links <- rvest::html_elements(cells[[3]], "a")

# Find track and artist links
track_id <- NA_character_
artist_id <- NA_character_
artist_name <- NA_character_
track_title <- NA_character_

for (link in links) {
  href <- rvest::html_attr(link, "href")
  text <- trimws(rvest::html_text(link))

  if (grepl("/track/", href)) {
    track_id <- extract_spotify_id(href)
    track_title <- text
  } else if (grepl("/artist/", href)) {
    if (is.na(artist_id)) {
      artist_id <- extract_spotify_id(href)
    }
    if (is.na(artist_name)) {
      artist_name <- text
    } else {
      artist_name <- paste(artist_name, text, sep = ", ")
    }
  }
}

# If title not found in link, get from full cell text
if (is.na(track_title) || track_title == "") {
  track_title <- cell_text[3]
}

tibble::tibble(
  pos = as.integer(cell_text[1]),
  pos_change = cell_text[2],
  artist = artist_name,
  artist_id = artist_id,
  title = track_title,
  track_id = track_id,
  days = as.integer(gsub("[^0-9]", "", cell_text[4])),
  peak = as.integer(gsub("[^0-9]", "", cell_text[5])),
  streams_daily = parse_streams(cell_text[7]),
  streams_change = parse_streams(cell_text[8]),
  streams_7day = parse_streams(cell_text[9]),
  streams_7day_change = parse_streams(cell_text[10]),
  streams_total = parse_streams(cell_text[11])
)
}

#' Scrape multiple country charts
#'
#' Scrapes charts for multiple countries with rate limiting.
#'
#' @param countries Character vector of country codes
#' @param frequency "daily" or "weekly"
#' @param progress Show progress messages (default TRUE)
#' @return tibble with additional `country` column
#' @export
#' @examples
#' \dontrun{
#' # Scrape charts for multiple countries
#' charts <- scrape_charts(c("global", "us", "gb", "de"))
#' }
scrape_charts <- function(countries, frequency = "daily", progress = TRUE) {
results <- list()

for (i in seq_along(countries)) {
  country <- countries[i]

  if (progress) {
    message(sprintf("[%d/%d] Scraping %s...", i, length(countries), country))
  }

  tryCatch({
    chart <- scrape_chart(country = country, frequency = frequency)
    chart$country <- country
    results[[country]] <- chart
  }, error = function(e) {
    warning(sprintf("Failed to scrape %s: %s", country, e$message))
  })
}

dplyr::bind_rows(results)
}

#' Get available country codes
#'
#' Returns a character vector of known country codes that can be used
#' with `scrape_chart()`.
#'
#' @return Character vector of country codes
#' @export
#' @examples
#' codes <- get_country_codes()
#' head(codes)
get_country_codes <- function() {
# Common country codes available on kworb
c(
  "global",
  "ar", "au", "at", "be", "bo", "br", "bg", "ca", "cl", "co", "cr",
  "cy", "cz", "dk", "do", "ec", "eg", "sv", "ee", "fi", "fr", "de",
  "gr", "gt", "hn", "hk", "hu", "is", "in", "id", "ie", "il", "it",
  "jp", "lv", "lt", "lu", "my", "mx", "ma", "nl", "nz", "ni", "no",
  "pa", "py", "pe", "ph", "pl", "pt", "ro", "ru", "sa", "sg", "sk",
  "za", "kr", "es", "se", "ch", "tw", "th", "tr", "ua", "ae", "gb",
  "us", "uy", "ve", "vn"
)
}
