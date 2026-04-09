test_that("rotate_df transposes and promotes header row", {
  df <- data.frame(
    param   = c("Cmax", "AUC"),
    value_A = c("100",  "500"),
    value_B = c("110",  "490")
  )
  result <- rotate_df(df)

  # Original 3 cols → 3 rows; first row promoted to header leaves 2 data rows
  expect_equal(nrow(result), 2L)
  expect_named(result, c("row_no", "Cmax", "AUC"))
  expect_equal(result$row_no, c("value_A", "value_B"))
})

test_that("rotate_df respects a custom rowname_col name", {
  df     <- data.frame(x = c("a", "b"), y = c("1", "2"))
  result <- rotate_df(df, rowname_col = "variable")
  expect_true("variable" %in% names(result))
  expect_false("row_no"  %in% names(result))
})
