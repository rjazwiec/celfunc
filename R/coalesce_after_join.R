#' Resolve duplicate columns after a join
#'
#' After a `dplyr` join produces paired `.x` / `.y` columns, this function
#' coalesces each pair into a single column: the `.x` value is used when
#' non-`NA`, otherwise the `.y` value is used. The original `.x` / `.y`
#' columns are then dropped.
#'
#' @param df A data frame, typically the result of [dplyr::left_join()] or a
#'   similar join that produces `.x` / `.y` suffix columns.
#'
#' @return A data frame with `.x` / `.y` column pairs replaced by single
#'   coalesced columns carrying the base name.
#'
#' @details
#' Only column pairs where **both** the `.x` and `.y` variants are present are
#' processed. Unpaired `.x`-only or `.y`-only columns are left untouched.
#'
#' @importFrom dplyr coalesce select all_of
#' @importFrom stringr str_ends str_remove
#' @importFrom magrittr %>%
#'
#' @examples
#' library(dplyr)
#' df_a <- data.frame(id = 1:3, val = c(1, NA, 3))
#' df_b <- data.frame(id = 1:3, val = c(10, 20, 30))
#' joined <- left_join(df_a, df_b, by = "id")
#' merge_suffix_columns(joined)
#'
#' @export
merge_suffix_columns <- function(df) {
  x_cols     <- names(df)[str_ends(names(df), "\\.x")]
  y_cols     <- names(df)[str_ends(names(df), "\\.y")]
  base_names <- intersect(str_remove(x_cols, "\\.x"), str_remove(y_cols, "\\.y"))

  # Coalesce each paired column: prefer .x, fall back to .y
  for (name in base_names) {
    df[[name]] <- coalesce(df[[paste0(name, ".x")]], df[[paste0(name, ".y")]])
  }

  # Drop only the columns that were successfully coalesced (paired .x/.y)
  # — unpaired .x-only or .y-only columns are left untouched.
  # Guard before paste0: paste0(character(0), ".x") recycles to ".x" in R,
  # so we must check base_names before building the drop list.
  if (length(base_names) == 0) return(df)
  cols_to_drop <- c(paste0(base_names, ".x"), paste0(base_names, ".y"))
  df %>% select(-all_of(cols_to_drop))
}
