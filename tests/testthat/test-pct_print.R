test_that("pct_print computes and formats a percentage", {
  expect_equal(pct_print(25, 100), "25.00%")
  expect_equal(pct_print(1, 3, digits = 1), "33.3%")
})

test_that("pct_print respects prefix and suffix", {
  expect_equal(pct_print(50, 100, prefix = "(", suffix = "%)"), "(50.00%)")
})

# ---------------------------------------------------------------------------

test_that("pct_print2 formats a pre-computed proportion", {
  expect_equal(pct_print2(0.75), "75.00%")
})

test_that("pct_print2 respects conv = 1 for already-scaled percentages", {
  expect_equal(pct_print2(75, conv = 1, digits = 0), "75%")
})

test_that("pct_print2 replaces NA with na_token", {
  expect_equal(pct_print2(NA_real_), "--%")
  expect_equal(pct_print2(NA_real_, na_token = "N/A"), "N/A%")
})

test_that("pct_print2 handles NA in a mixed vector", {
  result <- pct_print2(c(0.5, NA_real_))
  expect_equal(result[2], "--%")
  expect_false(grepl("NA", result[2]))
})

# ---------------------------------------------------------------------------

test_that("prt rounds and trims whitespace", {
  expect_equal(prt(3.14159), "3.14")
  expect_equal(prt(3.14159, digits = 4), "3.1416")
  # No leading/trailing spaces even for vectors
  expect_false(any(grepl("^ | $", prt(c(1, 10, 100)))))
})

# ---------------------------------------------------------------------------

test_that("pv_prt formats a value above threshold", {
  expect_equal(pv_prt(0.042), "p-val = 0.042")
})

test_that("pv_prt formats a value below threshold", {
  expect_equal(pv_prt(0.0003), "p-val < 0.001")
})

test_that("pv_prt respects pref = FALSE", {
  expect_equal(pv_prt(0.042, pref = FALSE), "0.042")
  expect_equal(pv_prt(0.0003, pref = FALSE), "< 0.001")
})

test_that("pv_prt returns NA_character_ for NA input", {
  expect_true(is.na(pv_prt(NA_real_)))
})
