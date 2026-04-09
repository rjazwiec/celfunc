#' Format a datetime as a compact timestamp string
#'
#' Returns a compact character string suitable for use in file names or
#' identifiers. The default format produces `"YYMMDD_HHMM"`.
#'
#' @param dt A `POSIXct`/`POSIXlt` datetime. Defaults to [lubridate::now()].
#' @param fmt A [base::strftime()]-compatible format string.
#'   Defaults to `"%y%m%d_%H%M"`.
#'
#' @return A single character string.
#'
#' @examples
#' date_string(as.POSIXct("2026-04-07 13:33:00", tz = "UTC"))  # "260407_1333"
#' date_string(as.POSIXct("2026-04-07 13:33:00", tz = "UTC"), fmt = "%Y%m%d")  # "20260407"
#'
#' @export
date_string <- function(dt = lubridate::now(), fmt = "%y%m%d_%H%M") {
  format(dt, fmt)
}
