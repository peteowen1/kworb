#' Summary of the parsed kworb database
#'
#' One row per table and capture month of a local database folder written by `inst/scripts/parse_raw.R` (or the
#' assets of mediadata release `kworb-db`, downloaded into the same layout): rows, captures, the dates the rows
#' describe, and how many columns are 100% empty (should be only the columns blank on the page by design).
#'
#' @param db_dir Folder holding `<table>/month=YYYY-MM/part.parquet`.
#' @return Data frame: table, month, rows, captures, first_date, last_date, empty_columns.
#' @export
kworb_db_summary <- function(db_dir = "data/db") {
  if (!requireNamespace("arrow", quietly = TRUE)) stop("kworb_db_summary() needs the arrow package")
  parts <- list.files(db_dir, "^part\\.parquet$", recursive = TRUE, full.names = TRUE)
  if (!length(parts)) stop("no tables under ", db_dir)
  do.call(rbind, lapply(parts, function(p) {
    x <- as.data.frame(arrow::read_parquet(p)); tbl <- basename(dirname(dirname(p)))
    dcol <- intersect(c("chart_date", "snapshot_date"), names(x))[1]
    empty <- names(x)[vapply(x, function(v) all(is.na(v) | (is.character(v) & !nzchar(v))), logical(1))]
    data.frame(table = tbl, month = sub("^month=", "", basename(dirname(p))), rows = nrow(x), captures = length(unique(x$captured_at)),
               first_date = min(x[[dcol]]), last_date = max(x[[dcol]]), empty_columns = paste(empty, collapse = ";"), stringsAsFactors = FALSE)
  }))
}
