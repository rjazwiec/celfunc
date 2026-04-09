# geom_mean ----------------------------------------------------------------

test_that("geom_mean returns the correct geometric mean", {
  expect_equal(geom_mean(c(1, 4, 16)), 4)
  expect_equal(geom_mean(c(2, 8)), sqrt(16))
})

test_that("geom_mean ignores NA values", {
  expect_equal(geom_mean(c(1, NA, 4, 16)), 4)
})

test_that("geom_mean returns NA when any non-missing value is <= 0", {
  expect_true(is.na(geom_mean(c(1, -1, 4))))
  expect_true(is.na(geom_mean(c(0, 2, 4))))
})


# geom_mean_sup ------------------------------------------------------------

test_that("geom_mean_sup substitutes zeros before log-transformation", {
  result_with_zero <- geom_mean_sup(c(0, 2, 8))
  result_subst     <- geom_mean(c(0.001, 2, 8))
  expect_equal(result_with_zero, result_subst)
})

test_that("geom_mean_sup respects a custom sup_val", {
  expect_equal(geom_mean_sup(c(0, 4), sup_val = 1), geom_mean(c(1, 4)))
})

test_that("geom_mean_sup returns NA for negative values after substitution", {
  expect_true(is.na(geom_mean_sup(c(-1, 2, 8))))
})


# geom_mean_paired ---------------------------------------------------------

test_that("geom_mean_paired uses only complete pairs", {
  # Pair 2 is incomplete (y is NA), so only x values 1 and 3 are used
  result <- geom_mean_paired(c(1, 2, 3), c(10, NA, 30))
  expect_equal(result, geom_mean(c(1, 3)))
})

test_that("geom_mean_paired returns NA when paired x values are non-positive", {
  expect_true(is.na(geom_mean_paired(c(0, 2), c(1, 1))))
})


# means_long ---------------------------------------------------------------

test_that("means_long returns geometric mean by default", {
  vals <- c(1, 4, 16)
  result <- means_long(vals, "v", is_complete = rep(TRUE, 3), id_col = 1:3)
  expect_equal(result, 4)
})

test_that("means_long returns arithmetic mean when is_geometric = FALSE", {
  vals <- c(1, 2, 3)
  result <- means_long(vals, "v", is_complete = rep(TRUE, 3), id_col = 1:3,
                       is_geometric = FALSE)
  expect_equal(result, 2)
})

test_that("means_long with is_paired restricts to complete subjects", {
  vals        <- c(1, 4, 16)
  is_complete <- c(TRUE, FALSE, TRUE)
  result <- means_long(vals, "v", is_complete, id_col = 1:3)
  # Only subjects 1 and 3 contribute: GM(1, 16) = 4
  expect_equal(result, 4)
})

test_that("means_long returns NA for geometric mean with non-positive values", {
  result <- means_long(c(-1, 4, 16), "v", rep(TRUE, 3), 1:3)
  expect_true(is.na(result))
})


# paired_median ------------------------------------------------------------

test_that("paired_median returns the median of all values", {
  result <- paired_median(c(1, 3, 5), "v", rep(TRUE, 3), 1:3)
  expect_equal(result, 3)
})

test_that("paired_median with is_paired restricts to complete subjects", {
  result <- paired_median(c(1, 100, 3), "v",
                          is_complete = c(TRUE, FALSE, TRUE),
                          id_col = 1:3,
                          is_paired = TRUE)
  # Only 1 and 3 contribute
  expect_equal(result, 2)
})


# geom_mean_r --------------------------------------------------------------

test_that("geom_mean_r returns GMR close to the true ratio", {
  set.seed(42)
  x <- rlnorm(50, meanlog = 0, sdlog = 0.3)
  y <- rlnorm(50, meanlog = log(2), sdlog = 0.3)   # GM(y) / GM(x) ≈ 2
  result <- geom_mean_r(x, y)
  expect_named(result, c("GMR", "CI", "p_val"))
  expect_true(result$GMR > 1.5 && result$GMR < 2.5)
  expect_length(result$CI, 2)
  expect_true(result$p_val < 0.05)
})

test_that("geom_mean_r stops on non-positive values", {
  expect_error(geom_mean_r(c(-1, 2), c(1, 2)), "Non-positive")
  expect_error(geom_mean_r(c(1, 2), c(0, 2)),  "Non-positive")
})


# calc_auc -----------------------------------------------------------------

test_that("calc_auc computes the linear trapezoidal AUC", {
  # Trapezoids: (0+10)/2*1 + (10+10)/2*1 + (10+0)/2*2 = 5 + 10 + 10 = 25
  result <- calc_auc(c(0, 10, 10, 0), c(0, 1, 2, 4))
  expect_equal(result, 25)
})

test_that("calc_auc ignores NA concentrations", {
  result_na  <- calc_auc(c(0, NA, 10, 0), c(0, 0.5, 1, 2))
  result_ref <- calc_auc(c(0, 10, 0), c(0, 1, 2))
  expect_equal(result_na, result_ref)
})

test_that("calc_auc handles a simple example correctly", {
  # Trapezoids: (0+10)/2*1 + (10+6)/2*1 + (6+2)/2*2 = 5 + 8 + 8 = 21
  expect_equal(calc_auc(c(0, 10, 6, 2), c(0, 1, 2, 4)), 21)
})
