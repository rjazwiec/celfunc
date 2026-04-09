#' Geometric mean of a numeric vector
#'
#' @param x Numeric vector. `NA` values are removed before computation.
#' @return A single numeric value, or `NA` if any non-missing value is `<= 0`.
#'
#' @examples
#' geom_mean(c(1, 4, 16))   # 4
#' geom_mean(c(1, -1, 4))   # NA
#'
#' @importFrom stats na.omit
#' @export
geom_mean <- function(x) {
  x <- na.omit(x)
  if (any(x <= 0)) NA else exp(mean(log(x)))
}


#' Geometric mean with zero substitution
#'
#' Zeros in `x` are replaced with `sup_val` before log-transformation.
#' Useful when BLQ values are coded as `0`. Returns `NA` for any negative values.
#'
#' @param x Numeric vector.
#' @param sup_val Numeric. Substitution value for zeros. Default `0.001`.
#'
#' @return A single numeric value, or `NA` if any value (after substitution)
#'   is `< 0`.
#'
#' @examples
#' geom_mean_sup(c(0, 2, 8))           # uses 0.001 in place of 0
#' geom_mean_sup(c(0, 2, 8), sup_val = 0.5)
#'
#' @importFrom stats na.omit
#' @export
geom_mean_sup <- function(x, sup_val = 0.001) {
  x <- na.omit(x)
  x[x == 0] <- sup_val
  if (any(x < 0)) NA else exp(mean(log(x)))
}


#' Geometric mean restricted to complete pairs
#'
#' Computes the geometric mean of `x` using only subjects that have non-`NA`
#' values in **both** `x` and `y`. `y` is used solely to identify complete
#' cases; the mean itself is over `x`.
#'
#' Useful for pre/post comparisons where the baseline GM should be computed
#' on the same subjects that have a post-treatment value.
#'
#' @param x Numeric vector (values to average).
#' @param y Numeric vector (used only to determine complete cases).
#'
#' @return A single numeric value, or `NA` if any paired `x` value is `<= 0`.
#'
#' @examples
#' geom_mean_paired(c(1, 2, NA), c(10, NA, 30))  # GM of c(1) — only pair 1 is complete
#'
#' @importFrom stats complete.cases
#' @export
geom_mean_paired <- function(x, y) {
  paired_x <- x[complete.cases(x, y)]
  if (any(paired_x <= 0)) NA else exp(mean(log(paired_x)))
}


# Internal helper shared by means_long() and paired_median().
# Builds the working tibble and optionally filters to complete subjects only.
#' @keywords internal
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang :=
.build_analysis_df <- function(values, group, is_complete, id_col, is_paired) {
  df <- tibble(id_col, "{group}" := values, is_complete = is_complete)
  if (is_paired) df <- df %>% filter(is_complete)
  df
}


#' Flexible mean calculator for use inside dplyr summarise() pipelines
#'
#' Computes a geometric or arithmetic mean over `values`, optionally restricted
#' to subjects with complete data at all timepoints (`is_paired = TRUE`).
#'
#' @param values Numeric vector of values.
#' @param group Character. Name of the column in the internal working frame.
#' @param is_complete Logical vector. `TRUE` for subjects present at all
#'   timepoints (same length as `values`).
#' @param id_col Vector. Subject identifiers (same length as `values`).
#' @param is_geometric Logical. If `TRUE` (default), returns geometric mean;
#'   `FALSE` returns arithmetic mean.
#' @param is_paired Logical. If `TRUE`, restricts computation to subjects
#'   where `is_complete == TRUE`. Default `FALSE`.
#'
#' @return A single numeric value, or `NA` if the geometric mean cannot be
#'   computed (non-positive values present).
#'
#' @examples
#' means_long(c(1, 4, 16, 8), "conc",
#'            is_complete = c(TRUE, TRUE, FALSE, TRUE),
#'            id_col = 1:4)
#'
#' @export
means_long <- function(values, group, is_complete, id_col,
                       is_geometric = TRUE, is_paired = FALSE) {
  df <- .build_analysis_df(values, group, is_complete, id_col, is_paired)
  if (is_geometric && any(na.omit(df[[group]]) <= 0)) return(NA)
  if (is_geometric) exp(mean(log(df[[group]]), na.rm = TRUE))
  else              mean(df[[group]], na.rm = TRUE)
}


#' Median with optional complete-case restriction
#'
#' Computes the median of `values`, with the same paired/complete-case logic
#' as [means_long()]. Can be swapped in wherever a median is needed instead
#' of a mean.
#'
#' @param values Numeric vector.
#' @param group Character. Name of the column in the internal working frame.
#' @param is_complete Logical vector. `TRUE` for subjects at all timepoints.
#' @param id_col Vector. Subject identifiers.
#' @param is_paired Logical. If `TRUE`, restricts to complete subjects.
#'   Default `FALSE`.
#'
#' @return A single numeric value.
#'
#' @examples
#' paired_median(c(1, 2, 3, 4), "val",
#'              is_complete = c(TRUE, TRUE, FALSE, TRUE),
#'              id_col = 1:4)
#'
#' @importFrom stats median
#' @export
paired_median <- function(values, group, is_complete, id_col, is_paired = FALSE) {
  df <- .build_analysis_df(values, group, is_complete, id_col, is_paired)
  median(df[[group]], na.rm = TRUE)
}


#' Geometric mean ratio with 95% CI and p-value
#'
#' Computes GMR = GM(`y`) / GM(`x`) via Welch's t-test on log-transformed
#' independent samples, then back-transforms. Stops with an error if either
#' vector contains non-positive values.
#'
#' @param x Numeric vector (reference group). Must be all-positive after
#'   removing `NA`.
#' @param y Numeric vector (comparator group). Must be all-positive after
#'   removing `NA`.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{`GMR`}{Point estimate: GM(`y`) / GM(`x`).}
#'     \item{`CI`}{Length-2 numeric vector: 95% confidence interval.}
#'     \item{`p_val`}{Two-sided p-value for the difference in log-means.}
#'   }
#'
#' @examples
#' set.seed(1)
#' r <- geom_mean_r(rlnorm(20, 0, 0.3), rlnorm(20, 0.5, 0.3))
#' r$GMR
#'
#' @importFrom stats t.test na.omit
#' @export
geom_mean_r <- function(x, y) {
  x <- na.omit(x)
  y <- na.omit(y)
  if (any(x <= 0) || any(y <= 0))
    stop("Non-positive values in x or y. Cannot calculate geometric mean ratio.")
  test <- t.test(log(x), log(y))
  list(
    GMR   = unname(exp(test$estimate[2] - test$estimate[1])),
    CI    = exp(test$conf.int),
    p_val = test$p.value
  )
}


#' AUC by the linear trapezoidal rule
#'
#' Computes the area under the concentration-time curve using the linear
#' trapezoidal method (no extrapolation to infinity). Accepts both plain
#' numeric vectors and `units`-class vectors; when both carry units the
#' returned value also carries the product unit (e.g., `ng/mL * h`).
#'
#' `NA` concentrations are removed before computation.
#'
#' @param conc Numeric or `units`-class vector of concentrations.
#' @param time Numeric or `units`-class vector of time points corresponding
#'   to `conc`.
#'
#' @return A numeric scalar, or a `units`-class scalar when both `conc` and
#'   `time` carry units.
#'
#' @examples
#' calc_auc(c(0, 10, 6, 2), c(0, 1, 2, 4))   # 18
#'
#' @importFrom units drop_units set_units deparse_unit
#' @export
calc_auc <- function(conc, time) {
  conc_has_units <- inherits(conc, "units")
  time_has_units <- inherits(time, "units")

  if (conc_has_units) cu <- conc[1]
  if (time_has_units) tu <- time[1]

  conc_num <- if (conc_has_units) drop_units(conc) else conc
  time_num <- if (time_has_units) drop_units(time) else time

  na_mask  <- !is.na(conc_num)
  conc_num <- conc_num[na_mask]
  time_num <- time_num[na_mask]

  n       <- length(conc_num)
  auc_num <- sum((conc_num[-1] + conc_num[-n]) / 2 * (time_num[-1] - time_num[-n]))

  if (conc_has_units && time_has_units)
    set_units(auc_num, deparse_unit(cu * tu), mode = "standard")
  else
    auc_num
}
