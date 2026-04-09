#' Transpose a data frame
#'
#' Transposes `df` using [base::t()], promotes the first row to column names
#' (via [janitor::row_to_names()]), and converts the existing row names to an
#' explicit column.
#'
#' @param df A data frame to transpose.
#' @param rowname_col Character. Name of the column that will hold the original
#'   row names after transposition. Default `"row_no"`.
#'
#' @return A data frame with rows and columns swapped. **All columns will be of
#'   type `character`** because [base::t()] coerces the input to a character
#'   matrix when it contains mixed types.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' df <- data.frame(param    = c("Cmax", "AUC"),
#'                  value_A  = c("100",  "500"),
#'                  value_B  = c("110",  "490"))
#' rotate_df(df)
#'
#' @export
rotate_df <- function(df, rowname_col = "row_no") {
  df %>%
    t() %>%
    as.data.frame() %>%
    janitor::row_to_names(row_number = 1) %>%
    tibble::rownames_to_column(rowname_col)
}
