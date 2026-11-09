# Publish the parsed tables (inst/scripts/parse_raw.R output) to the PRIVATE peteowen1/mediadata release `kworb-db`
# (docs/plans/kworb-db.md 3.2: tables from a third party's pages stay private, as the raw captures do).
#
#   Rscript inst/scripts/publish_db.R [db_dir] [months] [asset_prefix]
#   db_dir default data/db; months = comma-separated YYYY-MM to publish (default: every month in db_dir).
# One asset per table per capture month: <table>__YYYY-MM.parquet (GitHub rewrites "=" in asset names). The daily job publishes only the current month,
# replacing that month's assets (a closed month is never touched again). Each upload is checked by listing the release
# afterwards: an asset missing or with the wrong size fails the run. Needs `gh` with write access (GH_TOKEN).
args <- commandArgs(trailingOnly = TRUE)
db_dir <- if (length(args) >= 1) args[1] else "data/db"
months <- if (length(args) >= 2 && nzchar(args[2]) && args[2] != "all") strsplit(args[2], ",")[[1]] else NULL   # "all" (or empty) = every month; PowerShell drops an empty argument
# arg 3: asset prefix after the table name, e.g. "wayback__" for the tables parsed from Wayback copies (data/db_wayback), so
# the daily job, which rewrites <table>__<month>.parquet from live captures only, never overwrites them
prefix <- if (length(args) >= 3) args[3] else ""
repo <- Sys.getenv("MEDIADATA_REPO", "peteowen1/mediadata"); tag <- "kworb-db"
gh <- function(a) { out <- suppressWarnings(system2("gh", a, stdout = TRUE, stderr = TRUE)); list(rc = attr(out, "status") %||% 0L, out = out) }
`%||%` <- function(x, y) if (is.null(x)) y else x
parts <- list.files(db_dir, "^part\\.parquet$", recursive = TRUE, full.names = TRUE)
info <- data.frame(path = parts, table = basename(dirname(dirname(parts))), month = sub("^month=", "", basename(dirname(parts))), stringsAsFactors = FALSE)
if (!is.null(months)) info <- info[info$month %in% months, ]
if (!nrow(info)) stop("nothing to publish in ", db_dir, if (!is.null(months)) paste(" for", paste(months, collapse = ",")))
info$asset <- sprintf("%s__%s%s.parquet", info$table, prefix, info$month)   # no "=": GitHub turns it into "."
if (gh(c("release", "view", tag, "-R", repo))$rc != 0) {
  cr <- gh(c("release", "create", tag, "-R", repo, "--title", tag, "--notes", shQuote("kworb database tables (kworb repo, inst/scripts/parse_raw.R): one parquet per table per capture month.")))
  if (cr$rc != 0) stop("could not create release ", tag, ": ", paste(cr$out, collapse = " ")) }
tmp <- tempfile("kworb-db-"); dir.create(tmp)
for (i in seq_len(nrow(info))) {
  f <- file.path(tmp, info$asset[i]); file.copy(info$path[i], f, overwrite = TRUE)
  up <- gh(c("release", "upload", tag, shQuote(f), "-R", repo, "--clobber"))
  if (up$rc != 0) stop("upload failed for ", info$asset[i], ": ", paste(up$out, collapse = " "))
  cat("uploaded", info$asset[i], round(file.size(f) / 1e6, 2), "MB\n") }
ls <- gh(c("release", "view", tag, "-R", repo, "--json", "assets", "--jq", shQuote(".assets[] | .name + \" \" + (.size|tostring)")))
got <- do.call(rbind, lapply(strsplit(ls$out, " "), function(z) data.frame(asset = z[1], size = as.numeric(z[2]), stringsAsFactors = FALSE)))
chk <- merge(data.frame(asset = info$asset, want = file.size(info$path)), got, by = "asset", all.x = TRUE)
bad <- chk[is.na(chk$size) | chk$size != chk$want, ]
if (nrow(bad)) { print(bad); stop(nrow(bad), " asset(s) missing or the wrong size on ", repo, " ", tag) }
cat("published", nrow(info), "assets to", repo, tag, "| verified sizes\n")
