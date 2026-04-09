test_that("date_string produces correct YYMMDD_HHMM output", {
  dt <- as.POSIXct("2026-04-07 13:33:00", tz = "UTC")
  expect_equal(date_string(dt), "260407_1333")
})

test_that("date_string respects a custom format string", {
  dt <- as.POSIXct("2026-04-07 00:00:00", tz = "UTC")
  expect_equal(date_string(dt, fmt = "%Y%m%d"), "20260407")
})

test_that("date_string returns a length-1 character vector", {
  result <- date_string(Sys.time())
  expect_type(result, "character")
  expect_length(result, 1L)
})
