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
tryCatch({
  piggyback::pb_release_create(repo = repo, tag = tag)
  message(sprintf("Created new release '%s'", tag))
}, error = function(e) {
  # Release likely already exists
  if (!grepl("already exists", e$message, ignore.case = TRUE)) {
    message("Note: ", e$message)
  }
})

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
