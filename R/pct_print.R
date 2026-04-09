#' Format a percentage from a count and total
#'
#' Divides `val` by `all`, multiplies by 100, and returns a formatted
#' character string.
#'
#' @param val Numeric. The count (numerator).
#' @param all Numeric. The total (denominator).
#' @param digits Integer. Number of decimal places. Default `2`.
#' @param prefix Character. String prepended to the result. Default `""`.
#' @param suffix Character. String appended to the result. Default `"%"`.
#' @param ... Ignored; retained for future extensibility.
#'
#' @return A character vector of formatted percentages.
#'
#' @examples
#' pct_print(23, 100)           # "23.00%"
#' pct_print(1, 3, digits = 1)  # "33.3%"
#'
#' @export
pct_print <- function(val, all, digits = 2, prefix = "", suffix = "%", ...) {
  pct <- val / all * 100
  paste0(prefix, format(round(pct, digits), nsmall = digits), suffix)
}


#' Format a pre-computed proportion or percentage
#'
#' Applies an optional multiplier (`conv`), rounds to `digits` decimal places,
#' and returns a formatted character string. `NA` values are replaced with
#' `na_token` before any string formatting is applied.
#'
#' @param pct Numeric vector. Value(s) to format.
#' @param digits Integer. Number of decimal places. Default `2`.
#' @param conv Numeric. Multiplier applied before formatting. Use `100` when
#'   `pct` is a proportion (0--1); use `1` when it is already a percentage
#'   (0--100). Default `100`.
#' @param prefix Character. String prepended to each result. Default `""`.
#' @param suffix Character. String appended to each result. Default `"%"`.
#' @param na_token Character. Replacement string for `NA` values.
#'   Default `"--"`.
#' @param ... Ignored; retained for future extensibility.
#'
#' @return A character vector of formatted percentages.
#'
#' @examples
#' pct_print2(c(0.75, NA, 0.5))         # "75.00%"  "--%" "50.00%"
#' pct_print2(75, conv = 1, digits = 0) # "75%"
#'
#' @export
pct_print2 <- function(pct, digits = 2, conv = 100,
                       prefix = "", suffix = "%", na_token = "--", ...) {
  out <- format(round(pct * conv, digits), nsmall = digits)
  # Replace NA positions with na_token *after* formatting to avoid fragile
  # string-matching on the formatted "NA" literal
  out[is.na(pct)] <- na_token
  paste0(prefix, out, suffix)
}


#' Format a numeric value with fixed decimal places
#'
#' A thin wrapper around [base::format()] and [base::round()] that also strips
#' leading and trailing whitespace, returning a clean character vector suitable
#' for table cells.
#'
#' @param val Numeric vector.
#' @param digits Integer. Number of decimal places. Default `2`.
#'
#' @return A character vector trimmed of whitespace.
#'
#' @importFrom stringr str_trim
#'
#' @examples
#' prt(3.14159)          # "3.14"
#' prt(c(1, 10, 100))   # "1.00"  "10.00"  "100.00"
#'
#' @export
prt <- function(val, digits = 2) {
  str_trim(format(round(val, digits), nsmall = digits))
}


#' Format a p-value with threshold notation
#'
#' Returns a formatted character string. Values below `10^{-digits}` are
#' displayed as `"< threshold"` rather than rounded to zero.
#'
#' @param val Numeric vector of p-values.
#' @param digits Integer. Number of decimal places and exponent of the
#'   threshold. A value of `3` gives a threshold of `0.001`. Default `3`.
#' @param pref Logical. If `TRUE` (default), prepend `"p-val < "` or
#'   `"p-val = "` to the output. If `FALSE`, only `"< "` is prepended for
#'   below-threshold values.
#'
#' @return A character vector. `NA` inputs return `NA_character_`.
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' pv_prt(0.0003)               # "p-val < 0.001"
#' pv_prt(0.042)                # "p-val = 0.042"
#' pv_prt(0.042, pref = FALSE)  # "0.042"
#' pv_prt(NA_real_)             # NA
#'
#' @export
pv_prt <- function(val, digits = 3, pref = TRUE) {
  threshold  <- 10^-digits
  pref_below <- if (pref) "p-val < " else "< "
  pref_above <- if (pref) "p-val = " else ""

  dplyr::case_when(
    is.na(val)      ~ NA_character_,
    val < threshold ~ paste0(pref_below, threshold),
    .default        = paste0(pref_above,
                             format(round(val, digits), nsmall = digits))
  )
}
