# Parsers for the raw captures (Stage 2 of docs/plans/kworb-db.md).
#
# Each parser is a pure function of one saved page: the same file always gives the same rows, so a parser fix
# means re-parsing, never re-fetching. Every column the page shows is kept, in raw form next to the parsed one
# where parsing could lose something (position change, the "(xN)" peak count). Rows carry both clocks: the day
# the number describes (chart_date / snapshot_date) and when we saw it (captured_at).

#' Parser version written on every parsed row
#' @return Integer.
#' @keywords internal
kworb_parser_version <- function() 6L   # 6: track pages (track_chart / track_page_summary). 5: global weekly totals pages (chart_totals, frequency weekly; weeks / top10_weeks). 4: last row of a page with no </table> dropped, dates inside <strong>, weekly charts; 2 (9 Nov review): raw cells for top10_days, artists' as_lead / solo / as_feature; 3: page layouts back to 2016 (layout, era, page_pos, streams_raw, source)

#' Number from a kworb cell
#'
#' Drops thousands separators and a leading "+"; "", "=", "NEW", "RE" and anything else non-numeric give NA.
#' @param x Character vector.
#' @return Numeric vector.
#' @keywords internal
kworb_num <- function(x) {
  x <- gsub(paste0("[,", intToUtf8(160L), " ]"), "", trimws(x))   # thousands separators, incl. a non-breaking space
  x <- sub("^\\+", "", x)
  out <- suppressWarnings(as.numeric(x))
  out[!grepl("^-?[0-9]+(\\.[0-9]+)?$", x)] <- NA_real_
  out
}

# the table's header texts, its cells as a character matrix (one row per <tr>), and the <tr> nodes. Vectorised: one XPath
# query per table, not one per cell (the per-row version took minutes for a 12,000-row totals page).
kworb_table <- function(doc, drop_last = FALSE) {
  tb <- rvest::html_element(doc, "table")
  if (inherits(tb, "xml_missing")) return(NULL)
  # header = the first row with <th> cells (in <thead> today; a plain first <tr> on pages before 2023); data rows = every
  # <tr> with <td> cells, inside <tbody> or not. Each data row is numbered (data-kw-row) so cells can be matched to it.
  hdr <- trimws(xml2::xml_text(xml2::xml_find_all(tb, "(.//tr[th])[1]/th")))
  rows <- xml2::xml_find_all(tb, ".//tr[td]")
  ncol <- length(hdr)
  nc <- xml2::xml_find_num(rows, "count(./td)")
  # a Wayback copy can be cut off mid-row (artists__wb20190716: row 7,246 is '<a href="artist/"></a>' and no streams cell):
  # a short LAST row is dropped and the page marked truncated; a short row anywhere else is a layout change and stops
  truncated <- length(rows) > 0 && ((nc[length(nc)] < ncol && all(nc[-length(nc)] == ncol)) || drop_last)
  if (truncated) { rows <- rows[-length(rows)]; nc <- nc[-length(nc)] }
  if (length(rows) && any(nc != ncol)) stop("a row has ", nc[nc != ncol][1], " cells, not ", ncol, " (the header's count)")
  xml2::xml_set_attr(rows, "data-kw-row", seq_along(rows))
  td <- xml2::xml_find_all(rows, "./td")
  m <- matrix(trimws(xml2::xml_text(td)), ncol = max(ncol, 1L), byrow = TRUE)
  list(header = hdr, cells = m, rows = rows, truncated = truncated)
}

# read a page; some Wayback copies were stored cut off (four totals pages of 2019-21 end at exactly 1 MiB, mid-tag, and
# libxml2 then fails with "Memory allocation failed"). On a failure the text is cut back to its last complete </tr> and the
# table closed, and the document is marked truncated: the rows before the cut are real, the rest of the page is missing.
kworb_read_html <- function(html) {
  d <- tryCatch(rvest::read_html(html), error = function(e) NULL)
  # a copy with no closing </table> was cut somewhere (at a row boundary, or mid-cell: artists__wb20211023 ends '<td>4,952,',
  # which would read as 4,952 streams): its last row cannot be trusted, so kworb_table() drops it (review 9 Nov)
  if (!is.null(d)) return(list(doc = d, cut = FALSE, incomplete = !grepl("</table>", html, fixed = TRUE)))
  ends <- gregexpr("</tr>", html, fixed = TRUE)[[1]]
  if (ends[1] < 0) stop("page does not parse and has no complete table row")
  last <- ends[length(ends)] + 4L
  list(doc = rvest::read_html(paste0(substr(html, 1, last), "</tbody></table></body></html>")), cut = TRUE, incomplete = FALSE)   # already cut at a complete row
}

# pick the layout whose header matches; stop naming every known layout when none does
kworb_layout <- function(got, layouts, page) {
  for (nm in names(layouts)) if (identical(got, layouts[[nm]])) return(nm)
  stop(page, ": header changed. Got [", paste(got, collapse = "|"), "]; known: ", paste(vapply(layouts, paste, "", collapse = "|"), collapse = " / "))
}

# the "Artist and Title" cell (column `col`) of every row: lead artists before the track link, the track, featured
# artists after it ("(w/ ...)"), and the cell's text
kworb_title_cells <- function(rows, col) {
  n <- length(rows); empty <- rep("", n)
  if (!n) return(data.frame(track_id = character(), title = character(), lead_artist_ids = character(), lead_artist_names = character(),
                            feat_artist_ids = character(), feat_artist_names = character(), cell_text = character(), stringsAsFactors = FALSE))
  cell <- xml2::xml_find_all(rows, sprintf("./td[%d]", col))
  a <- xml2::xml_find_all(cell, ".//a")
  row <- as.integer(xml2::xml_attr(xml2::xml_find_first(a, "ancestor::tr[1]"), "data-kw-row"))   # set by kworb_table()
  href <- xml2::xml_attr(a, "href"); txt <- xml2::xml_text(a)
  kind <- ifelse(grepl("(^|/)track/", href), "track", ifelse(grepl("(^|/)artist/", href), "artist", "other"))
  id <- sub("^(.*/)?(artist|track)/([A-Za-z0-9]+)(_songs)?\\.html$", "\\3", href)
  ti <- tapply(seq_along(a)[kind == "track"], row[kind == "track"], min)        # first track link per row
  tpos <- rep(Inf, n); tpos[as.integer(names(ti))] <- ti
  is_lead <- kind == "artist" & seq_along(a) < tpos[row]; is_feat <- kind == "artist" & seq_along(a) > tpos[row]
  agg <- function(v, keep) { o <- empty; s <- tapply(v[keep], row[keep], paste, collapse = ";"); o[as.integer(names(s))] <- s; o }
  tr_id <- rep(NA_character_, n); tr_t <- rep(NA_character_, n); k <- as.integer(names(ti)); tr_id[k] <- id[ti]; tr_t[k] <- txt[ti]
  data.frame(track_id = tr_id, title = tr_t, lead_artist_ids = agg(id, is_lead), lead_artist_names = agg(txt, is_lead),
             feat_artist_ids = agg(id, is_feat), feat_artist_names = agg(txt, is_feat), cell_text = trimws(xml2::xml_text(cell)), stringsAsFactors = FALSE)
}

#' Parse a saved daily chart page (country or global)
#'
#' @param html Page text.
#' @param country Country code from the file name (`global`, `us`, ...).
#' @return Data frame, one row per chart position: every column the page shows, plus `chart_date` from the page heading.
#' @keywords internal
parse_daily_chart <- function(html, country) {
  rd <- kworb_read_html(html); t <- kworb_table(rd$doc, drop_last = rd$incomplete); if (!is.null(t)) t$truncated <- t$truncated || rd$cut
  if (is.null(t)) stop("daily chart ", country, ": no table")
  lay <- kworb_layout(t$header, list(d11 = c("Pos", "P+", "Artist and Title", "Days", "Pk", "(x?)", "Streams", "Streams+", "7Day", "7Day+", "Total")), paste("daily chart", country))
  x <- t$cells
  out <- cbind(data.frame(pos = as.integer(x[, 1]), pos_change_raw = x[, 2], stringsAsFactors = FALSE), kworb_title_cells(t$rows, 3),
               data.frame(days = kworb_num(x[, 4]), peak_pos = kworb_num(x[, 5]), peak_count_raw = x[, 6],
                          peak_count = kworb_num(sub("^\\(x([0-9]+)\\)$", "\\1", x[, 6])), streams = kworb_num(x[, 7]),
                          streams_change_raw = x[, 8], streams_change = kworb_num(x[, 8]), streams_7day = kworb_num(x[, 9]),
                          streams_7day_change_raw = x[, 10], streams_7day_change = kworb_num(x[, 10]), streams_total = kworb_num(x[, 11]),
                          stringsAsFactors = FALSE))
  pc <- out$pos_change_raw
  out$pos_change_type <- ifelse(pc == "NEW", "new", ifelse(pc == "RE", "re-entry", ifelse(pc == "=", "same", ifelse(grepl("^[+-][0-9]+$", pc), "move", "other"))))
  out$pos_change <- ifelse(out$pos_change_type == "same", 0, kworb_num(pc))
  out$country <- country; out$frequency <- "daily"; out$chart_date <- parse_page_date(html); out$layout <- lay; out$page_truncated <- t$truncated
  out
}

#' Parse a saved weekly chart page (country/<code>_weekly.html)
#'
#' Same row shape as the daily chart, with weeks on the chart (`weeks`) in place of days and no 7-day columns; rows go to
#' `chart_entry` with `frequency = "weekly"`. `chart_date` is the date in the page heading.
#' @param html Page text.
#' @param country Country code from the file name.
#' @return Data frame, one row per chart position.
#' @keywords internal
parse_weekly_chart <- function(html, country) {
  rd <- kworb_read_html(html); t <- kworb_table(rd$doc, drop_last = rd$incomplete); if (!is.null(t)) t$truncated <- t$truncated || rd$cut
  if (is.null(t)) stop("weekly chart ", country, ": no table")
  lay <- kworb_layout(t$header, list(w9 = c("Pos", "P+", "Artist and Title", "Wks", "Pk", "(x?)", "Streams", "Streams+", "Total")), paste("weekly chart", country))
  x <- t$cells
  out <- cbind(data.frame(pos = as.integer(x[, 1]), pos_change_raw = x[, 2], stringsAsFactors = FALSE), kworb_title_cells(t$rows, 3),
               data.frame(weeks = kworb_num(x[, 4]), peak_pos = kworb_num(x[, 5]), peak_count_raw = x[, 6],
                          peak_count = kworb_num(sub("^\\(x([0-9]+)\\)$", "\\1", x[, 6])), streams = kworb_num(x[, 7]),
                          streams_change_raw = x[, 8], streams_change = kworb_num(x[, 8]), streams_total = kworb_num(x[, 9]), stringsAsFactors = FALSE))
  pc <- out$pos_change_raw
  out$pos_change_type <- ifelse(pc == "NEW", "new", ifelse(pc == "RE", "re-entry", ifelse(pc == "=", "same", ifelse(grepl("^[+-][0-9]+$", pc), "move", "other"))))
  out$pos_change <- ifelse(out$pos_change_type == "same", 0, kworb_num(pc))
  out$country <- country; out$frequency <- "weekly"; out$chart_date <- parse_page_date(html); out$layout <- lay; out$page_truncated <- t$truncated
  out
}

#' Parse a saved "Totals" page (global daily or weekly chart totals)
#'
#' The daily page counts days on the chart (`Days`), the weekly page weeks (`Wks`); the rest of the layout is the same.
#' Daily rows fill `days` / `top10_days`, weekly rows `weeks` / `top10_weeks`; the other pair is NA.
#' @param html Page text.
#' @param country Country code from the file name.
#' @param frequency `"daily"` or `"weekly"`.
#' @return Data frame, one row per track: chart-time totals as the page states them, with `covers_from` / `covers_to`.
#' @keywords internal
parse_chart_totals <- function(html, country, frequency = "daily") {
  stopifnot(frequency %in% c("daily", "weekly"))
  rd <- kworb_read_html(html); t <- kworb_table(rd$doc, drop_last = rd$incomplete); if (!is.null(t)) t$truncated <- t$truncated || rd$cut
  if (is.null(t)) stop("totals ", country, ": no table")
  T7 <- c("Artist and Title", if (frequency == "daily") "Days" else "Wks", "T10", "Pk", "(x?)", "PkStreams", "Total")
  lay <- kworb_layout(t$header, list(t7 = T7, t8_pos = c("Pos", T7)), paste(frequency, "totals", country))   # t8_pos: pages before 2023 lead with Pos
  page_pos <- if (lay == "t8_pos") as.integer(t$cells[, 1]) else NA_integer_
  x <- if (lay == "t8_pos") t$cells[, -1, drop = FALSE] else t$cells
  dly <- frequency == "daily"; NAn <- rep(NA_real_, nrow(x)); NAc <- rep(NA_character_, nrow(x))
  out <- cbind(kworb_title_cells(t$rows, if (lay == "t8_pos") 2 else 1),
               data.frame(days = if (dly) kworb_num(x[, 2]) else NAn, top10_days_raw = if (dly) x[, 3] else NAc, top10_days = if (dly) kworb_num(x[, 3]) else NAn,
                          weeks = if (dly) NAn else kworb_num(x[, 2]), top10_weeks_raw = if (dly) NAc else x[, 3], top10_weeks = if (dly) NAn else kworb_num(x[, 3]),
                          peak_pos = kworb_num(x[, 4]), peak_count_raw = x[, 5],
                          peak_count = kworb_num(sub("^\\(x([0-9]+)\\)$", "\\1", x[, 5])), peak_streams = kworb_num(x[, 6]), total_streams = kworb_num(x[, 7]),
                          stringsAsFactors = FALSE))
  cov <- regmatches(html, regexpr("Covers charts from [0-9]{4}/[0-9]{2}/[0-9]{2} to [0-9]{4}/[0-9]{2}/[0-9]{2}", html))
  d <- if (length(cov)) regmatches(cov, gregexpr("[0-9]{4}/[0-9]{2}/[0-9]{2}", cov))[[1]] else c(NA, NA)
  out$covers_from <- as.Date(d[1], format = "%Y/%m/%d"); out$covers_to <- as.Date(d[2], format = "%Y/%m/%d")
  out$rank <- seq_len(nrow(out)); out$page_pos <- page_pos; out$country <- country; out$frequency <- frequency; out$snapshot_date <- out$covers_to; out$layout <- lay; out$page_truncated <- t$truncated
  out
}

#' Parse a saved artists page (artists.html)
#'
#' The page shows streams in millions to one decimal place and daily streams in millions to three; both are kept as
#' shown (`*_m`) and in raw units with their resolution, so nothing pretends to more precision than the page has.
#' The page carries no date: `snapshot_date` is the capture's UTC date and the caller must treat it as such.
#' @param html Page text.
#' @return Data frame, one row per artist.
#' @keywords internal
parse_artists <- function(html) {
  rd <- kworb_read_html(html); t <- kworb_table(rd$doc, drop_last = rd$incomplete); if (!is.null(t)) t$truncated <- t$truncated || rd$cut
  if (is.null(t)) stop("artists: no table")
  lay <- kworb_layout(t$header, list(
    a6 = c("Artist", "Streams", "Daily", "As lead", "Solo", "As feature"),      # from May 2023: all-time streams, millions
    a5 = c("Artist", "Streams", "Daily", "As lead", "As feature"),              # Apr-May 2023: no Solo column yet
    a3_chart = c("Pos", "Artist", "Total Streams")), "artists")                 # to Mar 2023: chart-time totals, exact integers
  x <- t$cells; a <- xml2::xml_find_first(t$rows, ".//td//a")
  id <- sub("^(.*/)?artist/([A-Za-z0-9]+)(_songs)?\\.html$", "\\2", xml2::xml_attr(a, "href")); nm <- xml2::xml_text(a)
  NAc <- rep(NA_character_, nrow(x)); NAn <- rep(NA_real_, nrow(x))
  if (lay == "a3_chart") {
    out <- data.frame(artist_id = id, artist_name = nm, streams_m = kworb_num(x[, 3]) / 1e6, daily_m = NAn, as_lead_m = NAn, solo_m = NAn, as_feature_m = NAn,
                      as_lead_raw = NAc, solo_raw = NAc, as_feature_raw = NAc, streams_raw = x[, 3], page_pos = as.integer(x[, 1]), stringsAsFactors = FALSE)
  } else {
    sc <- if (lay == "a6") x[, 5] else NAc; fc <- if (lay == "a6") x[, 6] else x[, 5]
    out <- data.frame(artist_id = id, artist_name = nm, streams_m = kworb_num(x[, 2]), daily_m = kworb_num(x[, 3]), as_lead_m = kworb_num(x[, 4]), solo_m = kworb_num(sc),
                      as_feature_m = kworb_num(fc), as_lead_raw = x[, 4], solo_raw = sc, as_feature_raw = fc,   # blank on the page = NA here (0 not confirmed); the cell is kept
                      streams_raw = x[, 2], page_pos = NA_integer_, stringsAsFactors = FALSE)
  }
  for (v in c("streams", "daily", "as_lead", "solo", "as_feature")) out[[v]] <- round(out[[paste0(v, "_m")]] * 1e6)
  out$streams_resolution <- if (lay == "a3_chart") 1 else 1e5; out$daily_resolution <- if (lay == "a3_chart") NA_real_ else 1e3
  out$era <- if (lay == "a3_chart") "chart" else "all"   # chart = streams while on the charts (to Mar 2023); all = every stream: never mix the two
  out$rank <- seq_len(nrow(out)); out$layout <- lay; out$page_truncated <- t$truncated
  out
}

#' Parse a saved track page (track/<id>.html)
#'
#' A track page holds two tables, weekly (every charted week) and daily (the last 31 days), with one column per chart
#' (`Global` plus about 40 countries). Each table starts with a `Total` row and a `Peak` row, followed by one row per chart
#' date. A cell reads `position (streams)`. `--` means not on that chart that day, and those cells are left out, not
#' stored as 0 (plan D7). One output row per filled cell. `row_type` is `date`, `total` (`streams` = the page's total on
#' that chart) or `peak` (`position` = peak position, `streams` = streams at the peak). A cell that does not read as
#' `position (streams)`, `position` or a bare number keeps its text in `cell_raw` and is flagged `cell_odd`.
#' @param html Page text.
#' @param track_id Spotify track id from the file name.
#' @return Data frame, one row per filled cell.
#' @keywords internal
parse_track_page <- function(html, track_id) {
  rd <- kworb_read_html(html); doc <- rd$doc
  title <- regmatches(html, regexec("Title: ([^<\n]*)<br", html))[[1]]
  ah <- xml2::xml_find_all(doc, "//div[@class='subcontainer'][span[@class='pagetitle']]/a[contains(@href, 'artist/')]")
  a_ids <- paste(sub("^(.*/)?artist/([A-Za-z0-9]+)(_songs)?\\.html$", "\\2", xml2::xml_attr(ah, "href")), collapse = ";")
  a_nms <- paste(xml2::xml_text(ah), collapse = ";")
  one <- function(freq) {
    tb <- xml2::xml_find_first(doc, sprintf("//div[@class='%s']/table", freq))
    if (inherits(tb, "xml_missing")) return(NULL)
    hdr <- trimws(xml2::xml_text(xml2::xml_find_all(tb, "(.//tr[th])[1]/th")))
    if (!length(hdr) || hdr[1] != "Date") stop("track page ", freq, ": header changed. Got [", paste(hdr, collapse = "|"), "]")
    if (length(hdr) == 1) return(NULL)   # no chart columns: a track new this week has a weekly table of Date / Total / Peak only (11 Nov)
    rows <- xml2::xml_find_all(tb, ".//tr[td]")
    nc <- xml2::xml_find_num(rows, "count(./td)")
    if (rd$incomplete && length(rows)) { rows <- rows[-length(rows)]; nc <- nc[-length(nc)] }   # a cut copy: the last row cannot be trusted
    if (any(nc != length(hdr))) stop("track page ", freq, ": a row has ", nc[nc != length(hdr)][1], " cells, not ", length(hdr))
    td <- xml2::xml_find_all(rows, "./td")
    txt <- matrix(trimws(xml2::xml_text(td)), ncol = length(hdr), byrow = TRUE)
    p <- matrix(xml2::xml_text(xml2::xml_find_first(td, "./span[@class='p']")), ncol = length(hdr), byrow = TRUE)
    s <- matrix(xml2::xml_text(xml2::xml_find_first(td, "./span[@class='s']")), ncol = length(hdr), byrow = TRUE)
    lab <- txt[, 1]
    rt <- ifelse(lab == "Total", "total", ifelse(lab == "Peak", "peak", ifelse(grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$", lab), "date", "other")))
    if (any(rt == "other")) stop("track page ", freq, ": a row label is not Total, Peak or a date: ", lab[rt == "other"][1])
    out <- list()
    for (j in seq_along(hdr)[-1]) {
      v <- txt[, j]; keep <- nzchar(v) & v != "--"; if (!any(keep)) next
      pj <- p[keep, j]; sj <- s[keep, j]; vj <- v[keep]
      std <- !is.na(pj) & !is.na(sj) & vj == paste0(trimws(pj), " (", trimws(sj), ")")
      bare <- is.na(pj) & is.na(sj) & grepl("^[0-9][0-9,]*$", vj)       # Total row: a plain number
      ponly <- !is.na(pj) & is.na(sj) & vj == trimws(pj)                  # a position with no streams shown
      r <- rt[keep]
      pos <- ifelse(std | ponly, kworb_num(pj), NA_real_)
      st <- ifelse(std, kworb_num(sj), ifelse(bare, kworb_num(vj), NA_real_))
      odd <- !(std | bare | ponly)
      out[[length(out) + 1]] <- data.frame(frequency = freq, row_type = r, country = tolower(hdr[j]),
        chart_date = as.Date(ifelse(r == "date", lab[keep], NA_character_), format = "%Y/%m/%d"),
        position = as.integer(pos), streams = st, cell_raw = ifelse(odd | bare, vj, NA_character_), cell_odd = odd, stringsAsFactors = FALSE)
    }
    if (!length(out)) return(NULL)
    x <- do.call(rbind, out); x$page_truncated <- rd$incomplete || rd$cut; x
  }
  x <- rbind(one("weekly"), one("daily"))
  if (is.null(x)) stop("track page ", track_id, ": no filled cells in either table")
  x$track_id <- track_id; x$title <- if (length(title) == 2) trimws(xml2::xml_text(xml2::read_html(paste0("<p>", title[2], "</p>")))) else NA_character_
  x$artist_ids <- a_ids; x$artist_names <- a_nms; x$layout <- "tp1"
  x[order(x$frequency, x$row_type, x$country, x$chart_date), c("track_id", "title", "artist_ids", "artist_names", setdiff(names(x), c("track_id", "title", "artist_ids", "artist_names")))]
}

#' Parse one saved raw file
#'
#' Picks the parser from the file name (`<slug>__<UTC stamp>.html.gz`) and adds the request columns.
#' @param path Path to a `.html.gz` capture.
#' @return `list(table = <name>, rows = <data frame>)`, or `table = NA` for a page type with no parser (the index).
#' @export
parse_raw_file <- function(path) {
  f <- basename(path); slug <- sub("__.*$", "", f)
  # live captures: <slug>__<yyyymmddTHHMMSSZ>.html.gz; Wayback copies (backfill_wayback.R): <slug>__wb<yyyymmddHHMMSS>.html.gz,
  # where the stamp is when the archive captured the page, i.e. the earliest moment anyone could have read it there
  wb <- grepl("__wb[0-9]{14}\\.html\\.gz$", f)
  captured_at <- if (wb) as.POSIXct(sub("^.*__wb([0-9]{14})\\.html\\.gz$", "\\1", f), format = "%Y%m%d%H%M%S", tz = "UTC") else
    as.POSIXct(sub("^.*__([0-9]{8}T[0-9]{6}Z)\\.html\\.gz$", "\\1", f), format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  if (is.na(captured_at)) stop("cannot read the capture time from the file name: ", f)
  con <- gzfile(path, "rb"); raw <- readBin(con, "raw", file.info(path)$size * 40 + 1e6); close(con)
  html <- rawToChar(raw); Encoding(html) <- "UTF-8"
  m <- regmatches(slug, regexec("^country-([a-z]+)_daily(_totals)?$", slug))[[1]]
  wk <- regmatches(slug, regexec("^country-([a-z]+)_weekly$", slug))[[1]]
  wt <- regmatches(slug, regexec("^country-([a-z]+)_weekly_totals$", slug))[[1]]
  if (length(wt)) {
    rows <- parse_chart_totals(html, wt[2], "weekly"); table <- "chart_totals"
  } else if (length(wk)) {
    rows <- parse_weekly_chart(html, wk[2]); table <- "chart_entry"
  } else if (length(m)) {
    rows <- if (nzchar(m[3])) parse_chart_totals(html, m[2]) else parse_daily_chart(html, m[2])
    table <- if (nzchar(m[3])) "chart_totals" else "chart_entry"
  } else if (slug == "artists") {
    rows <- parse_artists(html); rows$snapshot_date <- as.Date(captured_at, tz = "UTC"); table <- "artist_snapshot"
  } else if (grepl("^track-[A-Za-z0-9]{22}$", slug)) {
    rows <- parse_track_page(html, sub("^track-", "", slug)); table <- "track_chart"
  } else return(list(table = NA_character_, rows = NULL))
  rows$captured_at <- format(captured_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"); rows$source <- if (wb) "wayback" else "live"
  rows$raw_file <- f; rows$parser_version <- kworb_parser_version()
  list(table = table, rows = rows)
}

#' Coverage check on a parsed table
#'
#' Fails when a table is empty, a key is duplicated, or any listed column is populated below its floor. A column that
#' is present but empty is exactly the failure that row counts and schemas do not catch.
#' @param x Data frame.
#' @param key Columns that must be unique together.
#' @param floors Named numeric: column -> minimum share of non-NA, non-empty values.
#' @param name Table name for messages.
#' @return `x`, invisibly; stops on failure.
#' @export
assert_coverage <- function(x, key, floors, name = "table") {
  if (is.null(x) || !nrow(x)) stop(name, ": no rows")
  miss <- setdiff(c(key, names(floors)), names(x)); if (length(miss)) stop(name, ": missing columns ", paste(miss, collapse = ", "))
  if (anyDuplicated(x[, key, drop = FALSE])) stop(name, ": duplicate key (", paste(key, collapse = ", "), ")")
  bad <- character()
  for (v in names(floors)) { z <- x[[v]]; ok <- !is.na(z) & (!is.character(z) | nzchar(z)); s <- mean(ok)
    if (s < floors[[v]]) bad <- c(bad, sprintf("%s %.1f%% < %.1f%%", v, 100 * s, 100 * floors[[v]])) }
  if (length(bad)) stop(name, ": coverage below floor: ", paste(bad, collapse = "; "))
  invisible(x)
}
