#' Snapshot unit strings from a data frame
#'
#' Records the unit of every `units`-class column in `df` and returns them
#' as a two-column data frame. Use this before operations that strip units
#' (e.g., [tidyr::pivot_wider()], [dplyr::bind_rows()]), then restore with
#' [apply_units_from_df()].
#'
#' @param df A data frame.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{`column_name`}{Character. Name of the column that carries units.}
#'     \item{`unit`}{Character. Unit string (as returned by [units::units()]).}
#'   }
#'   Only columns that actually carry units are included; non-unit columns are
#'   excluded so the spec contains only actionable entries for
#'   [apply_units_from_df()].
#'
#' @examples
#' \dontrun{
#' library(units)
#' df <- data.frame(
#'   conc = set_units(c(10, 20), "ng/mL"),
#'   time = set_units(c(0, 1), "h")
#' )
#' spec <- save_units_in_df(df)
#' # spec has 2 rows: conc -> "ng/mL", time -> "h"
#' }
#'
#' @importFrom units deparse_unit set_units
#' @export
save_units_in_df <- function(df) {
  units_info <- vapply(df, function(x) {
    if (inherits(x, "units")) deparse_unit(x) else NA_character_
  }, character(1))

  units_info <- units_info[!is.na(units_info)]

  data.frame(
    column_name = names(units_info),
    unit        = units_info,
    row.names   = NULL
  )
}


#' Restore unit strings to a data frame
#'
#' Re-attaches units to columns of `df` using a spec produced by
#' [save_units_in_df()]. Columns absent from `df` are silently skipped,
#' making it safe to call on a data frame that has been reshaped since the
#' snapshot was taken.
#'
#' @param df A data frame whose unit columns need to be restored.
#' @param units_spec A data frame as returned by [save_units_in_df()], with
#'   columns `column_name` and `unit`.
#'
#' @return `df` with the specified columns converted to `units`-class objects.
#'
#' @examples
#' \dontrun{
#' library(units)
#' df <- data.frame(conc = c(10, 20), time = c(0, 1))
#' spec <- data.frame(column_name = c("conc", "time"),
#'                    unit        = c("ng/mL", "h"))
#' df_restored <- apply_units_from_df(df, spec)
#' }
#'
#' @importFrom units set_units
#' @export
apply_units_from_df <- function(df, units_spec) {
  for (i in seq_len(nrow(units_spec))) {
    col_name <- units_spec$column_name[i]
    unit     <- units_spec$unit[i]
    if (col_name %in% names(df))
      df[[col_name]] <- set_units(df[[col_name]], unit, mode = "standard")
  }
  df
}
