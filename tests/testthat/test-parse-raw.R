# Parsers for the Stage 2 tables. Pages are small hand-made copies of the real layouts (kworb.net, 9 Nov 2026):
# same tags, classes and headers, two rows each.
chart_page <- paste0(
  '<html><span class="pagetitle">Spotify Daily Chart - Global - 2026/11/07 | <a href="x">Totals</a></span>',
  '<table class="sortable" id="spotifydaily"><thead><tr><th class="np">Pos</th><th class="np">P+</th><th class="mp text col-title">Artist and Title</th>',
  '<th>Days</th><th>Pk</th><th class="mini text">(x?)</th><th>Streams</th><th>Streams+</th><th>7Day</th><th>7Day+</th><th>Total</th></tr></thead><tbody>',
  '<tr><td class="np">1</td> <td class="np">+1</td> <td class="text mp"><div><a href="../artist/790FomKkXshlbRYZFtlgla.html">KAROL G</a> - ',
  '<a href="../track/3h5T5JypYU7huFiVYhv1dr.html">BbY WOW</a> (w/ <a href="../artist/1H6X7yhnXZg73f9bssaj1Q.html">Judeline</a>, ',
  '<a href="../artist/1XEVu7gdRFfzEFqsPrancH.html">rusowsky</a>)</div></td> <td>88</td> <td>1</td><td class="np mini text">(x49)</td> ',
  '<td>5,075,633</td> <td>+96,820</td> <td class="smaller">33,042,099</td> <td class="smaller">-286,376</td> <td>421,987,118</td></tr>',
  '<tr><td class="np">2</td> <td class="np">NEW</td> <td class="text mp"><div><a href="../artist/45dkTj5sMRSjrmBSBeiHym.html">Tate McRae</a> - ',
  '<a href="../track/2sFepQNyl1WjpROruP8bJ0.html">Housewife</a></div></td> <td>1</td> <td>2</td><td class="np mini text"></td> ',
  '<td>4,567,570</td> <td></td> <td class="smaller">4,567,570</td> <td class="smaller"></td> <td>4,567,570</td></tr></tbody></table></html>')
totals_page <- paste0(
  '<html>Covers charts from 2014/08/10 to 2026/11/07.<br><table class="addpos sortable"><thead><tr> <th class="text">Artist and Title</th> <th>Days</th> ',
  '<th>T10</th> <th>Pk</th> <th class="mini text">(x?)</th> <th>PkStreams</th> <th>Total</th> </tr></thead><tbody>',
  '<tr> <td class="text mp"><div><a href="../artist/1Xyo4u8uXC1ZmMpatF05PJ.html">The Weeknd</a> - <a href="../track/0VjIjW4GlUZAMYd2vXMi3b.html">Blinding Lights</a></div></td> ',
  '<td>2532</td> <td>446</td> <td>1</td><td class="np mini text">(x82)</td> <td>8,453,567</td> <td>5,576,476,177</td></tr>',
  '<tr> <td class="text mp"><div><a href="../artist/77SW9BnxLY8rJ0RciFqkHh.html">The Neighbourhood</a> - <a href="../track/2QjOHCTQ1Jl3zawyYOpxh6.html">Sweater Weather</a></div></td> ',
  '<td>1585</td> <td></td> <td>11</td><td class="np mini text"></td> <td>3,342,205</td> <td>4,378,259,987</td></tr></tbody></table></html>')
artists_page <- paste0(
  '<html><table class="addpos sortable"><thead><tr> <th class="text">Artist</th> <th>Streams</th> <th>Daily</th> <th>As lead</th> <th>Solo</th> <th>As feature</th> </tr></thead><tbody>',
  '<tr> <td class="text"><div><a href="/spotify/artist/3TVXtAsR1Inumwj472S9r4_songs.html">Drake</a></div></td> <td>144,339.9</td> <td>56.900</td> ',
  '<td class="smaller">99,625.8</td> <td class="smaller">56,287.0</td> <td class="smaller">44,714.1</td></tr>',
  '<tr> <td class="text"><div><a href="/spotify/artist/06HL4z0CvFAxyc27GXpf02_songs.html">Taylor Swift</a></div></td> <td>133,199.6</td> <td>40.810</td> ',
  '<td class="smaller">129,223.8</td> <td class="smaller">118,300.9</td> <td class="smaller"></td></tr></tbody></table></html>')

test_that("kworb_num handles separators, signs and tokens", {
  expect_equal(kworb_num(c("5,075,633", "+96,820", "-286,376", "56.900", "", "=", "NEW", "(x49)")), c(5075633, 96820, -286376, 56.9, NA, NA, NA, NA))
})

test_that("daily chart: every column, lead and featured artists, movement types", {
  x <- parse_daily_chart(chart_page, "global")
  expect_equal(nrow(x), 2); expect_equal(x$pos, 1:2); expect_equal(x$chart_date, as.Date(c("2026-11-07", "2026-11-07")))
  expect_equal(x$track_id, c("3h5T5JypYU7huFiVYhv1dr", "2sFepQNyl1WjpROruP8bJ0")); expect_equal(x$title, c("BbY WOW", "Housewife"))
  expect_equal(x$lead_artist_ids[1], "790FomKkXshlbRYZFtlgla")
  expect_equal(x$feat_artist_ids[1], "1H6X7yhnXZg73f9bssaj1Q;1XEVu7gdRFfzEFqsPrancH"); expect_equal(x$feat_artist_names[1], "Judeline;rusowsky")
  expect_equal(x$feat_artist_ids[2], "")
  expect_equal(x$streams, c(5075633, 4567570)); expect_equal(x$streams_change, c(96820, NA)); expect_equal(x$streams_7day_change, c(-286376, NA))
  expect_equal(x$streams_total[1], 421987118); expect_equal(x$peak_count, c(49, NA)); expect_equal(x$peak_count_raw[2], "")
  expect_equal(x$pos_change_type, c("move", "new")); expect_equal(x$pos_change, c(1, NA))
})

test_that("totals page: chart-time totals and the Covers dates", {
  x <- parse_chart_totals(totals_page, "global")
  expect_equal(x$total_streams, c(5576476177, 4378259987)); expect_equal(x$peak_streams[1], 8453567)
  expect_equal(x$top10_days, c(446, NA)); expect_equal(x$peak_count, c(82, NA)); expect_equal(x$rank, 1:2)
  expect_equal(x$covers_from[1], as.Date("2014-08-10")); expect_equal(x$snapshot_date[1], as.Date("2026-11-07"))
})

test_that("artists page: raw units with their resolution, blank cells stay NA", {
  x <- parse_artists(artists_page)
  expect_equal(x$artist_id, c("3TVXtAsR1Inumwj472S9r4", "06HL4z0CvFAxyc27GXpf02"))
  expect_equal(x$streams[1], 144339900000); expect_equal(x$daily[1], 56900000); expect_equal(x$streams_m[1], 144339.9)
  expect_true(is.na(x$as_feature[2])); expect_equal(x$streams_resolution[1], 1e5)
})

test_that("a changed header stops the parser instead of shifting columns", {
  expect_error(parse_daily_chart(sub("<th>7Day</th>", "<th>Week</th>", chart_page), "global"), "header changed")
  expect_error(parse_artists(sub("<th>Daily</th>", "<th>Today</th>", artists_page)), "header changed")
})

test_that("parse_raw_file picks the table from the file name and adds both clocks", {
  d <- withr::local_tempdir(); f <- file.path(d, "country-global_daily__20261109T010942Z.html.gz")
  con <- gzfile(f, "wb"); writeBin(charToRaw(chart_page), con); close(con)
  r <- parse_raw_file(f)
  expect_equal(r$table, "chart_entry"); expect_equal(r$rows$captured_at[1], "2026-11-09T01:09:42Z")
  expect_equal(r$rows$chart_date[1], as.Date("2026-11-07")); expect_equal(r$rows$parser_version[1], kworb_parser_version())
  g <- file.path(d, "index__20261109T010941Z.html.gz"); con <- gzfile(g, "wb"); writeBin(charToRaw("<html></html>"), con); close(con)
  expect_true(is.na(parse_raw_file(g)$table))
})

# older layouts, as the Wayback Machine holds them (artists__wb20190319, country-global_daily_totals__wb2016...)
artists_chart_page <- paste0('<html><table><colgroup><col class="col-pos"></colgroup><tr><th>Pos</th><th class="text">Artist</th><th>Total Streams</th></tr> ',
  '<tr class="d0"><td>1</td><td class="text"><div><a href="artist/3TVXtAsR1Inumwj472S9r4.html">Drake</a></div></td><td>16,396,856,485</td></tr> ',
  '<tr class="d1"><td>2</td><td class="text"><div><a href="artist/6eUKZXaKkcviH0Ku9w2n3V.html">Ed Sheeran</a></div></td><td>10,083,293,261</td></tr> ',
  '<tr class="d0"><td>3</td><td class="text"><div><a href="artist/"></a></div></td></tr></table></html>')   # cut off mid-row, as archived
artists_a5_page <- sub("<th>Solo</th> ", "", sub('<td class="smaller">56,287.0</td> ', "", sub('<td class="smaller">118,300.9</td> ', "", artists_page, fixed = TRUE), fixed = TRUE), fixed = TRUE)
totals_pos_page <- sub('<th class="text">Artist and Title</th>', '<th class="np">Pos</th><th class="text">Artist and Title</th>',
  gsub('<tr> <td class="text mp">', '<tr class="d1"><td class="np">7</td> <td class="text mp">', totals_page, fixed = TRUE), fixed = TRUE)

test_that("artists before Apr 2023: chart-time totals, exact integers, a cut-off last row dropped", {
  x <- parse_artists(artists_chart_page)
  expect_equal(nrow(x), 2); expect_equal(x$artist_id, c("3TVXtAsR1Inumwj472S9r4", "6eUKZXaKkcviH0Ku9w2n3V"))
  expect_equal(x$streams, c(16396856485, 10083293261)); expect_equal(x$streams_resolution[1], 1); expect_equal(x$era[1], "chart")
  expect_true(all(is.na(x$daily))); expect_equal(x$page_pos, 1:2); expect_true(x$page_truncated[1]); expect_equal(x$layout[1], "a3_chart")
})

test_that("artists Apr-May 2023 (no Solo column) and today's layout", {
  x <- parse_artists(artists_a5_page)
  expect_equal(x$layout[1], "a5"); expect_true(all(is.na(x$solo))); expect_equal(x$as_feature[1], 44714100000); expect_equal(x$era[1], "all")
  y <- parse_artists(artists_page); expect_equal(y$layout[1], "a6"); expect_false(y$page_truncated[1])
})

test_that("totals pages before 2023 lead with Pos; the rest of the row is unchanged", {
  x <- parse_chart_totals(totals_pos_page, "global")
  expect_equal(x$layout[1], "t8_pos"); expect_equal(x$page_pos, c(7L, 7L)); expect_equal(x$total_streams, c(5576476177, 4378259987))
  expect_equal(x$title, c("Blinding Lights", "Sweater Weather")); expect_equal(x$top10_days, c(446, NA))
})

# global weekly totals (country/global_weekly_totals.html): the daily totals layout with Wks for Days (Wayback 2016-07 and 2026-10)
wtotals_page <- sub("<th>Days</th>", "<th>Wks</th>", totals_page, fixed = TRUE)
wtotals_pos_page <- sub("<th>Days</th>", "<th>Wks</th>", totals_pos_page, fixed = TRUE)

test_that("weekly totals: weeks, not days; both layouts; a daily header is refused", {
  x <- parse_chart_totals(wtotals_page, "global", "weekly")
  expect_equal(x$frequency[1], "weekly"); expect_equal(x$weeks, c(2532, 1585)); expect_equal(x$top10_weeks, c(446, NA))
  expect_true(all(is.na(x$days))); expect_true(all(is.na(x$top10_days))); expect_equal(x$total_streams, c(5576476177, 4378259987))
  y <- parse_chart_totals(wtotals_pos_page, "global", "weekly"); expect_equal(y$layout[1], "t8_pos"); expect_equal(y$weeks, c(2532, 1585))
  d <- parse_chart_totals(totals_page, "global"); expect_true(all(is.na(d$weeks))); expect_equal(d$days, c(2532, 1585))
  expect_error(parse_chart_totals(totals_page, "global", "weekly"), "header changed")
  f <- file.path(withr::local_tempdir(), "country-global_weekly_totals__wb20161011094006.html.gz")
  con <- gzfile(f, "wb"); writeBin(charToRaw(wtotals_page), con); close(con)
  r <- parse_raw_file(f); expect_equal(r$table, "chart_totals"); expect_equal(r$rows$frequency[1], "weekly"); expect_equal(r$rows$source[1], "wayback")
})

test_that("a page with no closing </table> drops its last row, even when that row has every cell", {
  cut <- sub('<td>10,083,293,261</td></tr> .*$', '<td>10,083,', artists_chart_page)   # cut mid-number, as artists__wb20211023 is
  x <- parse_artists(cut)
  expect_equal(nrow(x), 1); expect_equal(x$streams, 16396856485); expect_true(x$page_truncated[1])
  y <- parse_artists(artists_page); expect_equal(nrow(y), 2); expect_false(y$page_truncated[1])
})

test_that("a short row that is not the last one still stops the parser", {
  bad <- sub('<td class="smaller">99,625.8</td> ', "", artists_page, fixed = TRUE)
  expect_error(parse_artists(bad), "cells, not 6")
})

weekly_page <- paste0('<html><span class="pagetitle"><strong>Spotify Weekly Chart - Global - 2016/09/22</strong></span>',
  '<table class="sortable"><thead><tr><th class="np">Pos</th><th class="np">P+</th><th class="mp text col-title">Artist and Title</th>',
  '<th>Wks</th><th>Pk</th><th class="mini text">(x?)</th><th>Streams</th><th>Streams+</th><th>Total</th></tr></thead><tbody>',
  '<tr><td class="np">1</td> <td class="np">=</td> <td class="text mp"><div><a href="../artist/69GGBxA162lTqCwzJG5jLp.html">The Chainsmokers</a> - ',
  '<a href="../track/7BKLCZ1jbUBVqRi2FVlTVw.html">Closer</a> (w/ <a href="../artist/26VFTg2z8YR0cCuwLzESi2.html">Halsey</a>)</div></td> <td>8</td> <td>1</td>',
  '<td class="np mini text">(x5)</td> <td>40,123,456</td> <td>-1,234,567</td> <td>310,000,000</td></tr></tbody></table></html>')

test_that("weekly chart: weeks on chart, no 7-day columns, frequency weekly", {
  x <- parse_weekly_chart(weekly_page, "global")
  expect_equal(x$frequency, "weekly"); expect_equal(x$chart_date, as.Date("2016-09-22")); expect_equal(x$weeks, 8)
  expect_equal(x$streams, 40123456); expect_equal(x$streams_change, -1234567); expect_equal(x$streams_total, 310000000)
  expect_equal(x$feat_artist_names, "Halsey"); expect_equal(x$pos_change_type, "same"); expect_false("days" %in% names(x))
  d <- withr::local_tempdir(); f <- file.path(d, "country-global_weekly__wb20160929160411.html.gz")
  con <- gzfile(f, "wb"); writeBin(charToRaw(weekly_page), con); close(con); expect_equal(parse_raw_file(f)$table, "chart_entry")
})

test_that("old chart pages put the date inside <strong>", {
  expect_equal(parse_page_date('<span class="pagetitle"><strong>Spotify Daily Chart - Global - 2016/04/15</strong></span>'), as.Date("2016-04-15"))
})

test_that("Wayback file names give the archive's capture time and source", {
  d <- withr::local_tempdir(); f <- file.path(d, "artists__wb20190319180627.html.gz")
  con <- gzfile(f, "wb"); writeBin(charToRaw(artists_chart_page), con); close(con)
  r <- parse_raw_file(f)
  expect_equal(r$table, "artist_snapshot"); expect_equal(r$rows$captured_at[1], "2019-03-19T18:06:27Z"); expect_equal(r$rows$source[1], "wayback")
  expect_equal(r$rows$snapshot_date[1], as.Date("2019-03-19"))
})

test_that("assert_coverage fails on an empty column, a duplicate key and no rows", {
  x <- data.frame(k = 1:3, a = c("x", "y", "z"), b = c(1, NA, NA), stringsAsFactors = FALSE)
  expect_silent(assert_coverage(x, "k", c(a = 1)))
  expect_error(assert_coverage(x, "k", c(b = 0.9)), "coverage below floor")
  expect_error(assert_coverage(transform(x, a = "")[, c("k", "a")], "k", c(a = 0.5)), "coverage below floor")
  expect_error(assert_coverage(rbind(x, x[1, ]), "k", c(a = 1)), "duplicate key")
  expect_error(assert_coverage(x[0, ], "k", c(a = 1)), "no rows")
})

# track page (track/<id>.html, 11 Nov 2026): a weekly and a daily table, Date + one column per chart; Total and Peak rows first
cell <- function(p, s) paste0('<td>\n<span class="p">', p, '</span><span class="b"> (</span><span class="s">', s, '</span><span class="b">)</span>\n</td>')
track_page <- function(weekly = TRUE, odd = FALSE) paste0(
  '<html><body><div class="subcontainer"><span class="pagetitle">Spotify Chart History</span><br><br>\nTitle: Mr. Brightside<br>\n',
  'Artist: <a href="../artist/0C0XlULifJtAgn6ZNCW2eu.html">The Killers</a>\n<br><br>\n',
  '<div class="weekly">\n<table>\n<tr><th>Date</th>\n', if (weekly) '<th>Global</th>\n<th>US</th>\n', '</tr>\n',
  '<tr class="s tot">\n<td>Total</td>\n', if (weekly) '<td>2,000</td>\n<td>700</td>\n', '</tr>\n',
  '<tr class="pr">\n<td>Peak</td>\n', if (weekly) paste0(cell(26, "1,200"), "\n", cell(5, "700"), "\n"), '</tr>\n',
  if (weekly) paste0('<tr>\n<td>2014/10/26</td>\n', cell(191, 800), '\n<td>--</td>\n</tr>\n<tr>\n<td>2014/11/02</td>\n', cell(26, "1,200"), '\n', cell(5, 700), '\n</tr>\n'),
  '</table>\n</div>\n<div class="daily">\n<table>\n<tr><th>Date</th>\n<th>Global</th>\n</tr>\n<tr class="s tot">\n<td>Total</td>\n<td>9,000</td>\n</tr>\n',
  '<tr class="pr">\n<td>Peak</td>\n', cell(22, "300"), '\n</tr>\n<tr>\n<td>2026/11/09</td>\n', if (odd) '<td>?? odd</td>' else cell(25, "213"), '\n</tr>\n</table>\n</div>\n</div></body></html>')

test_that("track page: one row per filled cell, '--' left out, Total and Peak rows typed", {
  x <- parse_track_page(track_page(), "003vvx7Niy0yvhvHt4a68B")
  expect_equal(nrow(x), 3 + 2 + 2 + 3)   # weekly: 3 dated cells (one '--' dropped), 2 Totals, 2 Peaks; daily: 1 + Total + Peak
  w <- x[x$frequency == "weekly" & x$row_type == "date", ]
  expect_equal(w$country, c("global", "global", "us")); expect_equal(w$chart_date, as.Date(c("2014-10-26", "2014-11-02", "2014-11-02")))
  expect_equal(w$position, c(191L, 26L, 5L)); expect_equal(w$streams, c(800, 1200, 700))
  tot <- x[x$row_type == "total" & x$frequency == "weekly", ]; expect_equal(tot$streams, c(2000, 700)); expect_true(all(is.na(tot$chart_date)))
  pk <- x[x$row_type == "peak" & x$country == "global" & x$frequency == "weekly", ]; expect_equal(c(pk$position, pk$streams), c(26, 1200))
  expect_equal(unique(x$title), "Mr. Brightside"); expect_equal(unique(x$artist_ids), "0C0XlULifJtAgn6ZNCW2eu"); expect_equal(unique(x$artist_names), "The Killers")
  expect_false(any(x$cell_odd))
})

test_that("track page: a track new this week (weekly table of Date only) gives daily rows only", {
  x <- parse_track_page(track_page(weekly = FALSE), "003vvx7Niy0yvhvHt4a68B")
  expect_equal(unique(x$frequency), "daily"); expect_equal(nrow(x), 3)
})

test_that("track page: an unreadable cell is kept raw and flagged; a changed header stops", {
  x <- parse_track_page(track_page(odd = TRUE), "003vvx7Niy0yvhvHt4a68B")
  o <- x[x$cell_odd, ]; expect_equal(nrow(o), 1); expect_equal(o$cell_raw, "?? odd"); expect_true(is.na(o$streams))
  expect_error(parse_track_page(sub("<th>Date</th>", "<th>Week</th>", track_page(), fixed = TRUE), "x"), "header changed")
})

test_that("parse_raw_file sends track-<id> files to track_chart", {
  d <- withr::local_tempdir(); f <- file.path(d, "track-003vvx7Niy0yvhvHt4a68B__20261111T013145Z.html.gz")
  con <- gzfile(f, "wb"); writeBin(charToRaw(track_page()), con); close(con)
  r <- parse_raw_file(f)
  expect_equal(r$table, "track_chart"); expect_equal(unique(r$rows$track_id), "003vvx7Niy0yvhvHt4a68B")
  expect_equal(r$rows$captured_at[1], "2026-11-11T01:31:45Z"); expect_equal(r$rows$parser_version[1], 6L)
})
