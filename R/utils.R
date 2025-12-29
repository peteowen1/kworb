# Utility functions for kworb package

#' Parse stream numbers from kworb format
#'
#' Converts formatted stream strings like "5,675,034" or "+979,081" to numeric.
#'
#' @param x Character string with formatted number
#' @return Numeric value, or NA if parsing fails
#' @keywords internal
parse_streams <- function(x) {
if (is.na(x) || x == "" || x == "-") {
  return(NA_real_)
}
# Remove commas and any leading +/- sign for absolute value
cleaned <- gsub(",", "", x)
cleaned <- gsub("^\\+", "", cleaned)
as.numeric(cleaned)
}

#' Parse position/streams cell format
#'
#' Parses cells like "1 (128,896,137)" into position and streams components.
#'
#' @param x Character string in format "position (streams)"
#' @return Named list with `position` (integer) and `streams` (numeric)
#' @keywords internal
parse_position_streams <- function(x) {
if (is.na(x) || x == "" || x == "-") {
  return(list(position = NA_integer_, streams = NA_real_))
}

# Try to extract position (number before parenthesis) and streams (number in parenthesis)
# Format: "1 (128,896,137)" or just "128,896,137"

position <- NA_integer_
streams <- NA_real_

# Check if there's a parenthetical part
if (grepl("\\(", x)) {
  # Extract number before parenthesis (position)
  pos_part <- sub("\\s*\\(.*", "", x)
  pos_part <- trimws(pos_part)
  if (grepl("^[0-9]+$", pos_part)) {
    position <- as.integer(pos_part)
  }

  # Extract number inside parenthesis (streams)
  streams_match <- regmatches(x, regexec("\\(([^)]+)\\)", x))[[1]]
  if (length(streams_match) >= 2) {
    streams <- parse_streams(streams_match[2])
  }
} else {
  # No parenthesis - might just be a number (position or streams)
  clean <- trimws(x)
  if (grepl("^[0-9,]+$", clean)) {
    # If it has commas, likely streams; otherwise position
    if (grepl(",", clean)) {
      streams <- parse_streams(clean)
    } else {
      position <- as.integer(clean)
    }
  }
}

list(position = position, streams = streams)
}

#' Build kworb URL
#'
#' Constructs the full URL for kworb.net pages.
#'
#' @param type One of "chart", "track", or "artist"
#' @param id Spotify ID (for track/artist) or country code (for chart)
#' @param frequency "daily" or "weekly" (only used for charts)
#' @return Full URL string
#' @keywords internal
build_kworb_url <- function(type, id = "global", frequency = "daily") {
base <- "https://kworb.net/spotify/"

switch(type,
  "chart" = paste0(base, "country/", id, "_", frequency, ".html"),
  "track" = paste0(base, "track/", id, ".html"),
  "artist" = paste0(base, "artist/", id, ".html"),
  stop("Unknown type: ", type)
)
}

#' Polite HTTP request with rate limiting
#'
#' Fetches a URL with a delay to be respectful of the server.
#'
#' @param url URL to fetch
#' @param delay Seconds to wait before request (default: 1)
#' @param max_retries Maximum retry attempts on failure
#' @return rvest html_document or NULL on failure
#' @keywords internal
polite_read_html <- function(url, delay = 1, max_retries = 3) {
# Rate limiting delay
Sys.sleep(delay)

for (attempt in seq_len(max_retries)) {
  result <- tryCatch({
    response <- httr::GET(
      url,
      httr::user_agent(.kworb_user_agent),
      httr::timeout(30)
    )

    if (httr::status_code(response) == 200) {
      rvest::read_html(response)
    } else {
      warning(sprintf("HTTP %d for %s", httr::status_code(response), url))
      NULL
    }
  }, error = function(e) {
    if (attempt < max_retries) {
      message(sprintf("Attempt %d failed: %s. Retrying...", attempt, e$message))
      Sys.sleep(2^attempt)  # Exponential backoff
    }
    NULL
  })

  if (!is.null(result)) {
    return(result)
  }
}

warning(sprintf("All %d attempts failed for %s", max_retries, url))
NULL
}

#' Extract Spotify ID from href
#'
#' Parses hrefs like "../track/53iuhJlwXhSER5J2IYYv1W.html" to extract the ID.
#'
#' @param href Link string from kworb page
#' @return Spotify ID string, or NA if parsing fails
#' @keywords internal
extract_spotify_id <- function(href) {
if (is.na(href) || href == "") {
  return(NA_character_)
}
# Match the ID before .html
match <- regmatches(href, regexec("/([A-Za-z0-9]+)\\.html$", href))[[1]]
if (length(match) == 2) {
  match[2]
} else {
  NA_character_
}
}

#' Parse position change indicator
#'
#' Parses position change strings like "NEW", "RE", "+46", "-2", "=".
#'
#' @param x Character string with position change
#' @return Named list with `type` ("new", "re", "up", "down", "same") and `value` (integer)
#' @keywords internal
parse_position_change <- function(x) {
if (is.na(x) || x == "") {
  return(list(type = NA_character_, value = NA_integer_))
}

x <- trimws(x)

if (toupper(x) == "NEW") {
  list(type = "new", value = NA_integer_)
} else if (toupper(x) == "RE") {
  list(type = "re", value = NA_integer_)
} else if (x == "=" || x == "-") {
  list(type = "same", value = 0L)
} else if (grepl("^\\+", x)) {
  list(type = "up", value = as.integer(gsub("\\+", "", x)))
} else if (grepl("^-?\\d+$", x)) {
  val <- as.integer(x)
  if (val < 0) {
    list(type = "down", value = abs(val))
  } else {
    list(type = "up", value = val)
  }
} else {
  list(type = NA_character_, value = NA_integer_)
}
}

#' Validate Spotify ID format
#'
#' Checks if a string is a valid Spotify ID (22 alphanumeric characters).
#'
#' @param id Character string to validate
#' @return Logical TRUE if valid, FALSE otherwise
#' @keywords internal
validate_spotify_id <- function(id) {
if (is.na(id) || !is.character(id)) {
  return(FALSE)
}
grepl("^[A-Za-z0-9]{22}$", id)
}
