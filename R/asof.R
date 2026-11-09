#' Artist snapshots from the local kworb database
#'
#' Reads every `artist_snapshot` part from one or more database folders (the daily `data/db` and the Wayback
#' `data/db_wayback`, or the `kworb-db` release assets downloaded into the same layout) into one table with the
#' columns [kworb_asof()] needs. Rows parsed before the `era` column existed (the live table at parser_version 2)
#' get `era = "all"` when the row has a `daily` figure, else `"chart"`; that is the page's own distinction (only the
#' all-streams layout from April 2023 has a Daily column). `source` is `"live"` when the table does not carry one.
#'
#' @param db_dirs Folders holding `artist_snapshot/month=YYYY-MM/part.parquet`.
#' @return Data frame: artist_id, streams, daily, era, captured_at (POSIXct, UTC), snapshot_date, source,
#'   page_truncated, streams_resolution, raw_file.
#' @export
kworb_artist_snapshots <- function(db_dirs = c("data/db", "data/db_wayback")) {
  if (!requireNamespace("arrow", quietly = TRUE)) stop("kworb_artist_snapshots() needs the arrow package")
  parts <- unlist(lapply(file.path(db_dirs, "artist_snapshot"), list.files, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE))
  if (!length(parts)) stop("no artist_snapshot parts under ", paste(db_dirs, collapse = ", "))
  keep <- c("artist_id", "streams", "daily", "era", "captured_at", "snapshot_date", "source", "page_truncated", "streams_resolution", "raw_file")
  out <- do.call(rbind, lapply(parts, function(p) {
    x <- as.data.frame(arrow::read_parquet(p))
    for (cc in setdiff(keep, names(x))) x[[cc]] <- NA
    if (all(is.na(x$source))) x$source <- "live"
    x$era <- ifelse(is.na(x$era), ifelse(is.na(x$daily), "chart", "all"), x$era)
    x$page_truncated <- x$page_truncated %in% TRUE
    x$captured_at <- as.POSIXct(as.character(x$captured_at), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    x[, keep] }))
  if (anyNA(out$captured_at)) stop(sum(is.na(out$captured_at)), " rows have a captured_at that is not YYYY-MM-DDTHH:MM:SSZ")
  out
}

#' An artist's Spotify streams as of a date, with no look-ahead
#'
#' The lookup the awards scripts call (plan `docs/plans/kworb-db.md`, section 3.4): for each artist id and day, the
#' all-time stream total on the kworb artists page as it stood that day, and how that number was obtained.
#'
#' A snapshot counts as "seen by" a day when its capture time (UTC) falls on or before that day. With `strict = TRUE`
#' only such snapshots are used, so nothing captured after the day can reach the answer. Rules:
#' * The latest seen snapshot that lists the artist gives the value (`exact` when captured that day, else
#'   `last_before`), provided it is at most `max_gap_days` old.
#' * A complete snapshot (`page_truncated` FALSE) newer than that, which does not list the artist, means the artist
#'   was below the page's cut-off (top 10,000 in the chart era, top 3,000 since): `not_listed`, value NA.
#'   A cut-off (truncated) archive copy that does not list the artist says nothing and is skipped.
#' * Nothing within `max_gap_days`: `missing`, value NA, never a stale number.
#' * `strict = FALSE` (reporting only): when a later snapshot also lists the artist in the same era, the value is
#'   interpolated on a straight line between the two (`interpolated`).
#' * `what = "window_streams"`: the value on `date` minus the value on `date - window_days`, both found by the rules
#'   above; `missing` when either end has no value or the two ends are in different eras (the chart-era and
#'   all-streams totals are on different scales).
#'
#' @param ids Spotify artist ids (22 characters).
#' @param date Day(s) to answer for: one, or one per id.
#' @param snapshots Table from [kworb_artist_snapshots()].
#' @param what `"artist_total"` or `"window_streams"`.
#' @param window_days Length of the window for `"window_streams"`.
#' @param strict Use only snapshots seen by `date` (TRUE, the default; FALSE for reporting).
#' @param max_gap_days Oldest snapshot, in days before `date`, that may answer.
#' @return Data frame, one row per id, in input order: id, as_of_date, value, daily (the snapshot's Daily, all-streams
#'   era only), method, snap_captured_at, snap_after_captured_at (interpolation only), gap_days, source, era,
#'   resolution; for `"window_streams"` also start_value, start_method, start_captured_at.
#' @export
kworb_asof <- function(ids, date, snapshots, what = c("artist_total", "window_streams"), window_days = 365,
                       strict = TRUE, max_gap_days = 45) {
  what <- match.arg(what)
  date <- as.Date(date)
  if (length(date) == 1L) date <- rep(date, length(ids))
  if (length(date) != length(ids)) stop("date must be length 1 or the same length as ids")
  need <- c("artist_id", "streams", "daily", "era", "captured_at", "source", "page_truncated", "streams_resolution")
  miss <- setdiff(need, names(snapshots)); if (length(miss)) stop("snapshots lacks: ", paste(miss, collapse = ", "))
  s <- snapshots[order(snapshots$captured_at), need]
  cn <- as.numeric(s$captured_at)
  trunc_any <- tapply(s$page_truncated, cn, any)                         # one pass: is each capture a cut-off copy?
  caps <- as.numeric(names(trunc_any)); complete <- !as.vector(trunc_any)
  cap_day <- as.Date(as.POSIXct(caps, origin = "1970-01-01", tz = "UTC"), tz = "UTC")
  by_id <- split(seq_len(nrow(s)), s$artist_id)                          # rows of each artist, in capture order
  one <- function(id, d) {
    na <- list(value = NA_real_, daily = NA_real_, method = "missing", snap = NA_real_, snap_after = NA_real_,
               gap = NA_real_, source = NA_character_, era = NA_character_, res = NA_real_)
    if (is.na(id) || is.na(d)) return(na)
    seen <- cap_day <= d & cap_day >= d - max_gap_days
    rows <- by_id[[id]]; rows <- rows[cn[rows] %in% caps[seen]]
    last_listed <- if (length(rows)) cn[rows[length(rows)]] else -Inf
    gone <- caps[seen & complete & caps > last_listed]                   # newer complete copies without the artist
    if (length(gone)) { g <- max(gone); gd <- cap_day[match(g, caps)]
      return(modifyList(na, list(method = "not_listed", snap = g, gap = as.numeric(d - gd)))) }
    if (!length(rows)) return(na)
    i <- rows[length(rows)]; kd <- as.Date(s$captured_at[i], tz = "UTC")
    out <- list(value = s$streams[i], daily = s$daily[i], method = if (kd == d) "exact" else "last_before", snap = cn[i],
                snap_after = NA_real_, gap = as.numeric(d - kd), source = s$source[i], era = s$era[i], res = s$streams_resolution[i])
    if (!strict && out$method == "last_before") {
      later <- by_id[[id]]; later <- later[as.Date(s$captured_at[later], tz = "UTC") > d & as.Date(s$captured_at[later], tz = "UTC") <= d + max_gap_days & s$era[later] == out$era]
      if (length(later)) { j <- later[1]; jd <- as.Date(s$captured_at[j], tz = "UTC")
        w <- as.numeric(d - kd) / as.numeric(jd - kd)
        out$value <- out$value + w * (s$streams[j] - out$value); out$method <- "interpolated"; out$snap_after <- cn[j] } }
    out }
  r <- lapply(seq_along(ids), function(i) one(ids[i], date[i]))
  res <- data.frame(id = ids, as_of_date = date, value = vapply(r, `[[`, 0, "value"), daily = vapply(r, `[[`, 0, "daily"),
                    method = vapply(r, `[[`, "", "method"),
                    snap_captured_at = as.POSIXct(vapply(r, `[[`, 0, "snap"), origin = "1970-01-01", tz = "UTC"),
                    snap_after_captured_at = as.POSIXct(vapply(r, `[[`, 0, "snap_after"), origin = "1970-01-01", tz = "UTC"),
                    gap_days = vapply(r, `[[`, 0, "gap"), source = vapply(r, `[[`, "", "source"), era = vapply(r, `[[`, "", "era"),
                    resolution = vapply(r, `[[`, 0, "res"), stringsAsFactors = FALSE)
  if (what == "artist_total") return(res)
  st <- kworb_asof(ids, date - window_days, snapshots, "artist_total", strict = strict, max_gap_days = max_gap_days)
  ok <- !is.na(res$value) & !is.na(st$value) & res$era == st$era
  res$start_value <- st$value; res$start_method <- st$method; res$start_captured_at <- st$snap_captured_at
  res$value <- ifelse(ok, res$value - st$value, NA_real_)
  res$method <- ifelse(ok, res$method, "missing")
  res
}
