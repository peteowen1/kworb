# GitHub Releases integration for data storage

#' Download data from latest GitHub Release
#'
#' Downloads the data archive from the most recent GitHub Release
#' and extracts it to the specified directory.
#'
#' @param repo Repository in "owner/repo" format
#' @param dest_dir Destination directory for extracted data
#' @param token GitHub token (default: uses GITHUB_TOKEN env var)
#' @return Invisible path to extracted data directory
#' @export
#' @examples
#' \dontrun{
#' download_latest_data("peteo/kworb", "data")
#' }
download_latest_data <- function(repo, dest_dir = "data", token = NULL) {
# Get token from environment if not provided
if (is.null(token)) {
  token <- Sys.getenv("GITHUB_TOKEN", "")
  if (token == "") {
    token <- Sys.getenv("GH_TOKEN", "")
  }
}

# Get latest release info
release <- get_latest_release(repo, token)

if (is.null(release)) {
  message("No releases found")
  return(invisible(NULL))
}

message(sprintf("Found release: %s", release$tag_name))

# Find the data asset
data_asset <- NULL
for (asset in release$assets) {
  if (grepl("data.*\\.(tar\\.gz|zip)$", asset$name, ignore.case = TRUE)) {
    data_asset <- asset
    break
  }
}

if (is.null(data_asset)) {
  message("No data asset found in release")
  return(invisible(NULL))
}

# Download the asset
temp_file <- tempfile(fileext = ifelse(grepl("\\.zip$", data_asset$name), ".zip", ".tar.gz"))

message(sprintf("Downloading %s...", data_asset$name))

download_result <- tryCatch({
  if (token != "") {
    httr::GET(
      data_asset$browser_download_url,
      httr::add_headers(Authorization = paste("token", token)),
      httr::write_disk(temp_file, overwrite = TRUE)
    )
  } else {
    httr::GET(
      data_asset$browser_download_url,
      httr::write_disk(temp_file, overwrite = TRUE)
    )
  }
}, error = function(e) {
  stop(sprintf("Failed to download: %s", e$message))
})

if (httr::status_code(download_result) != 200) {
  stop(sprintf("Download failed with status %d", httr::status_code(download_result)))
}

# Extract
message(sprintf("Extracting to %s...", dest_dir))
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
}

if (grepl("\\.zip$", data_asset$name)) {
  utils::unzip(temp_file, exdir = dest_dir)
} else {
  utils::untar(temp_file, exdir = dest_dir)
}

# Cleanup
unlink(temp_file)

message("Download complete")
invisible(dest_dir)
}

#' Upload data as a GitHub Release
#'
#' Creates a new GitHub Release and uploads the data directory as an archive.
#' Uses the `gh` CLI tool which must be installed and authenticated.
#'
#' @param repo Repository in "owner/repo" format
#' @param data_dir Directory containing data to upload
#' @param tag Release tag (default: "data-YYYY-MM-DD")
#' @param title Release title (default: "Data update YYYY-MM-DD")
#' @return Invisible release URL
#' @export
#' @examples
#' \dontrun{
#' upload_data_release("peteo/kworb", "data")
#' }
upload_data_release <- function(repo, data_dir = "data",
                               tag = NULL, title = NULL) {
# Check gh CLI is available
if (Sys.which("gh") == "") {
  stop("GitHub CLI (gh) not found. Install from https://cli.github.com/")
}

# Set defaults
today <- format(Sys.Date(), "%Y-%m-%d")
if (is.null(tag)) {
  tag <- paste0("data-", today)
}
if (is.null(title)) {
  title <- paste0("Data update ", today)
}

# Create archive
archive_name <- paste0("kworb-data-", today, ".tar.gz")
archive_path <- file.path(tempdir(), archive_name)

message(sprintf("Creating archive %s...", archive_name))

# Create tar.gz archive
old_wd <- getwd()
setwd(dirname(data_dir))
on.exit(setwd(old_wd), add = TRUE)

utils::tar(archive_path, files = basename(data_dir), compression = "gzip")
setwd(old_wd)

# Check archive was created
if (!file.exists(archive_path)) {
  stop("Failed to create archive")
}

message(sprintf("Archive size: %.1f MB", file.size(archive_path) / 1024 / 1024))

# Create release using gh CLI
message(sprintf("Creating release %s...", tag))

# Build gh command
cmd <- sprintf(
  'gh release create "%s" "%s" --repo "%s" --title "%s" --notes "Automated data update"',
  tag, archive_path, repo, title
)

result <- system(cmd, intern = TRUE)

# Cleanup
unlink(archive_path)

if (length(result) > 0) {
  message(sprintf("Release created: %s", result[1]))
  invisible(result[1])
} else {
  message("Release created")
  invisible(NULL)
}
}

#' List available data releases
#'
#' Returns information about all data releases for a repository.
#'
#' @param repo Repository in "owner/repo" format
#' @param token GitHub token (optional)
#' @return tibble with release information
#' @export
#' @examples
#' \dontrun{
#' releases <- list_data_releases("peteo/kworb")
#' }
list_data_releases <- function(repo, token = NULL) {
# Get token from environment if not provided
if (is.null(token)) {
  token <- Sys.getenv("GITHUB_TOKEN", "")
  if (token == "") {
    token <- Sys.getenv("GH_TOKEN", "")
  }
}

# Build API URL
api_url <- sprintf("https://api.github.com/repos/%s/releases", repo)

# Make request
response <- tryCatch({
  if (token != "") {
    httr::GET(api_url, httr::add_headers(Authorization = paste("token", token)))
  } else {
    httr::GET(api_url)
  }
}, error = function(e) {
  warning(sprintf("Failed to fetch releases: %s", e$message))
  return(NULL)
})

if (is.null(response) || httr::status_code(response) != 200) {
  return(tibble::tibble())
}

releases <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

if (length(releases) == 0) {
  return(tibble::tibble())
}

# Filter to data releases
data_releases <- releases[grepl("^data-", releases$tag_name), ]

if (nrow(data_releases) == 0) {
  return(tibble::tibble())
}

tibble::tibble(
  tag = data_releases$tag_name,
  name = data_releases$name,
  published_at = as.POSIXct(data_releases$published_at),
  url = data_releases$html_url
)
}

#' Get latest release info
#'
#' Internal function to get the latest release details.
#'
#' @param repo Repository in "owner/repo" format
#' @param token GitHub token (optional)
#' @return List with release details or NULL
#' @keywords internal
get_latest_release <- function(repo, token = NULL) {
api_url <- sprintf("https://api.github.com/repos/%s/releases/latest", repo)

response <- tryCatch({
  if (!is.null(token) && token != "") {
    httr::GET(api_url, httr::add_headers(Authorization = paste("token", token)))
  } else {
    httr::GET(api_url)
  }
}, error = function(e) {
  return(NULL)
})

if (is.null(response) || httr::status_code(response) != 200) {
  return(NULL)
}

jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
}

#' Delete old data releases
#'
#' Removes old data releases, keeping only the most recent N releases.
#' Requires gh CLI to be installed and authenticated.
#'
#' @param repo Repository in "owner/repo" format
#' @param keep Number of releases to keep (default: 7)
#' @return Invisible number of releases deleted
#' @export
delete_old_releases <- function(repo, keep = 7) {
if (Sys.which("gh") == "") {
  stop("GitHub CLI (gh) not found")
}

releases <- list_data_releases(repo)

if (nrow(releases) <= keep) {
  message(sprintf("Only %d releases found, nothing to delete", nrow(releases)))
  return(invisible(0))
}

# Sort by date and identify old releases
releases <- releases[order(releases$published_at, decreasing = TRUE), ]
to_delete <- releases$tag[(keep + 1):nrow(releases)]

deleted <- 0
for (tag in to_delete) {
  message(sprintf("Deleting release %s...", tag))
  cmd <- sprintf('gh release delete "%s" --repo "%s" --yes', tag, repo)
  result <- system(cmd)
  if (result == 0) {
    deleted <- deleted + 1
  }
}

message(sprintf("Deleted %d old releases", deleted))
invisible(deleted)
}
