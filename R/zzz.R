# Package startup functions

.onLoad <- function(libname, pkgname) {
# Set default package options
op <- options()
op.kworb <- list(
  kworb.data_path = "data",
  kworb.rate_limit = 1,
  kworb.user_agent = "kworb-r-package/0.1.0"
)
toset <- !(names(op.kworb) %in% names(op))
if (any(toset)) options(op.kworb[toset])

invisible()
}

.onAttach <- function(libname, pkgname) {
packageStartupMessage(
  "kworb: Spotify streaming data from kworb.net\n",
  "Please be respectful of kworb.net's bandwidth when scraping."
)
}
