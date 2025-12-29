# GitHub Releases integration using piggyback

#' Download data from GitHub Release
#'
#' Downloads all data files from the specified release tag.
#'
#' @param repo Repository in "owner/repo" format
#' @param dest_dir Destination directory for downloaded files
#' @param tag Release tag (default: "data")
#' @return Invisible path to destination directory
#' @export
#' @examples
#' \dontrun{
#' download_release_data("peteowen1/kworb", "data")
#' }
download_release_data <- function(repo, dest_dir = "data", tag = "data") {
  # Create directory if needed
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
}

# Check if release exists
releases <- tryCatch({
  piggyback::pb_list(repo = repo, tag = tag)
}, error = function(e) {
  message("No existing release found: ", e$message)
  return(NULL)
})

if (is.null(releases) || nrow(releases) == 0) {
  message("No files to download")
  return(invisible(dest_dir))
}

message(sprintf("Downloading %d files from release '%s'...", nrow(releases), tag))

piggyback::pb_download(
  repo = repo,
  tag = tag,
  dest = dest_dir,
  overwrite = TRUE
)

message("Download complete")
invisible(dest_dir)
}

#' Upload data to GitHub Release
#'
#' Uploads data files to a GitHub Release. Creates the release if it doesn't exist.
#'
#' @param repo Repository in "owner/repo" format
#' @param files Character vector of file paths to upload
#' @param tag Release tag (default: "data")
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' upload_release_data("peteowen1/kworb", "data/charts/global_daily_2024-01-01.csv")
#' }
upload_release_data <- function(repo, files, tag = "data") {
# Filter to existing files
files <- files[file.exists(files)]

if (length(files) == 0) {
  message("No files to upload")
  return(invisible(NULL))
}

message(sprintf("Uploading %d files to release '%s'...", length(files), tag))

# Create release if it doesn't exist
release_created <- FALSE
tryCatch({
  piggyback::pb_release_create(repo = repo, tag = tag)
  message(sprintf("Created new release '%s'", tag))
  release_created <- TRUE
}, error = function(e) {
  # Release likely already exists
  if (!grepl("already exists", e$message, ignore.case = TRUE)) {
    message("Note: ", e$message)
  }
})

# Wait for GitHub to propagate new release
if (release_created) {
  message("Waiting for release to propagate...")
  Sys.sleep(5)
}

# Upload files
piggyback::pb_upload(
  file = files,
  repo = repo,
  tag = tag,
  overwrite = TRUE
)

message("Upload complete")
invisible(NULL)
}

#' Upload entire data directory to GitHub Release
#'
#' Uploads all CSV files from a directory to a GitHub Release.
#'
#' @param repo Repository in "owner/repo" format
#' @param data_dir Directory containing data files
#' @param tag Release tag (default: "data")
#' @param pattern File pattern to match (default: "\\.csv$")
#' @return Invisible NULL
#' @export
upload_data_directory <- function(repo, data_dir = "data", tag = "data",
                                  pattern = "\\.csv$") {
# Find all matching files recursively
files <- list.files(data_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)

if (length(files) == 0) {
  message("No files found matching pattern")
  return(invisible(NULL))
}

upload_release_data(repo, files, tag)
}

#' List files in GitHub Release
#'
#' Returns information about files stored in a release.
#'
#' @param repo Repository in "owner/repo" format
#' @param tag Release tag (default: "data")
#' @return tibble with file information
#' @export
list_release_files <- function(repo, tag = "data") {
tryCatch({
  piggyback::pb_list(repo = repo, tag = tag)
}, error = function(e) {
  message("Could not list files: ", e$message)
  tibble::tibble()
})
}

#' Load all chart data from GitHub Release
#'
#' Downloads and loads all chart CSV files from a release into a single tibble.
#'
#' @param repo Repository in "owner/repo" format
#' @param tag Release tag (default: "data")
#' @return tibble with all chart data combined
#' @export
#' @examples
#' \dontrun{
#' charts <- load_charts_from_release("peteowen1/kworb")
#' }
load_charts_from_release <- function(repo, tag = "data") {
  # List files in release
  files <- list_release_files(repo, tag)

  if (nrow(files) == 0) {
    message("No files found in release")
    return(tibble::tibble())
  }

  # Filter to chart files (contain _daily_ or _weekly_ in filename)
  # Piggyback strips directory structure, so we match on basename patterns
  chart_files <- files[grepl("_daily_|_weekly_", files$file_name), ]

  if (nrow(chart_files) == 0) {
    message("No chart files found")
    return(tibble::tibble())
  }

  message(sprintf("Loading %d chart files...", nrow(chart_files)))

  # Download to temp directory and read
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  piggyback::pb_download(
    file = chart_files$file_name,
    repo = repo,
    tag = tag,
    dest = temp_dir
  )

  # Read all CSVs
  csv_files <- list.files(temp_dir, pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)

  data_list <- lapply(csv_files, readr::read_csv, show_col_types = FALSE)
  dplyr::bind_rows(data_list)
}

#' Load track history from GitHub Release
#'
#' Downloads and loads track history CSV files from a release.
#'
#' @param repo Repository in "owner/repo" format
#' @param track_ids Character vector of track IDs to load, or "all" for all tracks
#' @param tag Release tag (default: "data")
#' @return tibble with track history data
#' @export
#' @examples
#' \dontrun{
#' # Load specific tracks
#' history <- load_tracks_from_release("peteowen1/kworb",
#'                                      c("53iuhJlwXhSER5J2IYYv1W"))
#'
#' # Load all tracks
#' all_history <- load_tracks_from_release("peteowen1/kworb", "all")
#' }
load_tracks_from_release <- function(repo, track_ids = "all", tag = "data") {
  # List files in release
  files <- list_release_files(repo, tag)

  if (nrow(files) == 0) {
    message("No files found in release")
    return(tibble::tibble())
  }

  # Filter to track files
  # Track files are Spotify track IDs (22 char alphanumeric) + .csv
  # Exclude chart files (which contain _daily_ or _weekly_)
  track_files <- files[
    grepl("^[A-Za-z0-9]{22}\\.csv$", files$file_name) &
    !grepl("_daily_|_weekly_", files$file_name)
  , ]

  if (nrow(track_files) == 0) {
    message("No track files found")
    return(tibble::tibble())
  }

  # Filter to specific tracks if requested
  if (!identical(track_ids, "all")) {
    pattern <- paste0("^(", paste(track_ids, collapse = "|"), ")\\.csv$")
    track_files <- track_files[grepl(pattern, track_files$file_name), ]
  }

  if (nrow(track_files) == 0) {
    message("No matching track files found")
    return(tibble::tibble())
  }

  message(sprintf("Loading %d track files...", nrow(track_files)))

  # Download to temp directory and read
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  piggyback::pb_download(
    file = track_files$file_name,
    repo = repo,
    tag = tag,
    dest = temp_dir
  )

  # Read all CSVs
  csv_files <- list.files(temp_dir, pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)

  data_list <- lapply(csv_files, readr::read_csv, show_col_types = FALSE)
  dplyr::bind_rows(data_list)
}

#' Load all data from GitHub Release
#'
#' Downloads and loads all data (charts and tracks) from a release.
#'
#' @param repo Repository in "owner/repo" format
#' @param tag Release tag (default: "data")
#' @return List with `charts` and `tracks` tibbles
#' @export
#' @examples
#' \dontrun{
#' data <- load_all_from_release("peteowen1/kworb")
#' data$charts
#' data$tracks
#' }
load_all_from_release <- function(repo, tag = "data") {
  list(
    charts = load_charts_from_release(repo, tag),
    tracks = load_tracks_from_release(repo, "all", tag)
  )
}

#' Delete files from GitHub Release
#'
#' Removes specified files from a release.
#'
#' @param repo Repository in "owner/repo" format
#' @param files Character vector of filenames to delete
#' @param tag Release tag (default: "data")
#' @return Invisible NULL
#' @export
delete_release_files <- function(repo, files, tag = "data") {
for (file in files) {
  tryCatch({
    piggyback::pb_delete(file = file, repo = repo, tag = tag)
    message(sprintf("Deleted: %s", file))
  }, error = function(e) {
    warning(sprintf("Could not delete %s: %s", file, e$message))
  })
}
invisible(NULL)
}
