# kworb_asof() on a synthetic snapshot set with known answers (plan docs/plans/kworb-db.md section 3.7).
snap <- function(day, ids, streams, era = "all", truncated = FALSE, hour = "06:00:00") {
  data.frame(artist_id = ids, streams = streams, daily = if (era == "all") streams / 1000 else NA_real_, era = era,
             captured_at = as.POSIXct(paste(day, hour), tz = "UTC"), source = "wayback", page_truncated = truncated,
             streams_resolution = if (era == "all") 1e5 else 1, stringsAsFactors = FALSE)
}
S <- rbind(
  snap("2023-01-01", c("A", "B", "C"), c(100, 50, 10), era = "chart"),
  snap("2023-05-01", c("A", "B", "C"), c(1000, 500, 100)),
  snap("2023-05-20", c("A"), c(1100), truncated = TRUE),          # cut-off copy: only A survived
  snap("2023-06-01", c("A", "B"), c(1200, 600)),                  # complete: C has dropped below the cut-off
  snap("2024-05-01", c("A", "B"), c(5000, 2000))
)

test_that("exact on the capture day, last_before inside the gap, missing beyond it", {
  r <- kworb_asof(c("A", "A", "A"), as.Date(c("2023-06-01", "2023-06-15", "2023-08-01")), S)
  expect_equal(r$method, c("exact", "last_before", "missing"))
  expect_equal(r$value, c(1200, 1200, NA))
  expect_equal(r$gap_days, c(0, 14, NA))
})

test_that("strict never uses a capture after the day; strict = FALSE interpolates in the same era", {
  r <- kworb_asof("A", as.Date("2023-05-10"), S)
  expect_equal(r$method, "last_before"); expect_equal(r$value, 1000)
  expect_lte(as.numeric(as.Date(r$snap_captured_at)), as.numeric(as.Date("2023-05-10")))
  ri <- kworb_asof("A", as.Date("2023-05-10"), S, strict = FALSE)
  expect_equal(ri$method, "interpolated"); expect_equal(ri$value, 1000 + 9 / 19 * 100)
})

test_that("a complete page without the artist is not_listed; a truncated one is skipped", {
  expect_equal(kworb_asof("C", as.Date("2023-06-10"), S)$method, "not_listed")
  r <- kworb_asof(c("B", "C"), as.Date("2023-05-25"), S)            # 20 May copy is cut off and lacks B and C
  expect_equal(r$method, c("last_before", "last_before")); expect_equal(r$value, c(500, 100))
  expect_equal(kworb_asof("Z", as.Date("2023-06-01"), S)$method, "not_listed")
})

test_that("window_streams needs both ends in one era", {
  w <- kworb_asof(c("A", "A"), as.Date(c("2024-05-01", "2023-05-01")), S, what = "window_streams", window_days = 335)
  # 2024-05-01 minus 335 days = 2023-06-02: A at 1200 (all era) -> 3800; 2023-05-01 minus 335 = 2022-05-31: nothing -> missing
  expect_equal(w$value, c(3800, NA)); expect_equal(w$method, c("exact", "missing"))
  e <- kworb_asof("A", as.Date("2023-05-01"), S, what = "window_streams", window_days = 120, max_gap_days = 45)
  # start 2023-01-01 is chart era, end is all era: different scales, so missing
  expect_equal(e$method, "missing"); expect_true(is.na(e$value))
  expect_equal(e$start_value, 100)
  nl <- kworb_asof("C", as.Date("2023-06-10"), S, what = "window_streams", window_days = 30)
  expect_equal(nl$method, "not_listed"); expect_true(is.na(nl$value))   # below the cut-off at the end stays not_listed
})

test_that("inputs are checked and answers come back in input order", {
  expect_error(kworb_asof("A", as.Date(c("2023-06-01", "2023-06-02")), S), "same length")
  expect_error(kworb_asof("A", as.Date("2023-06-01"), S[, -2]), "snapshots lacks")
  r <- kworb_asof(c("B", NA, "A"), as.Date("2023-06-01"), S)
  expect_equal(r$id, c("B", NA, "A")); expect_equal(r$value, c(600, NA, 1200)); expect_equal(r$method[2], "missing")
})
