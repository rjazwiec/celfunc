library(dplyr)

test_that("merge_suffix_columns coalesces paired columns, preferring .x", {
  df_a   <- data.frame(id = 1:3, val = c(1, NA, 3))
  df_b   <- data.frame(id = 1:3, val = c(10, 20, 30))
  joined <- left_join(df_a, df_b, by = "id")
  result <- merge_suffix_columns(joined)

  expect_named(result, c("id", "val"))
  expect_equal(result$val, c(1, 20, 3))  # .x preferred; .y fills the NA
})

test_that("merge_suffix_columns drops all .x and .y columns", {
  df_a   <- data.frame(id = 1:2, x = c(1, NA))
  df_b   <- data.frame(id = 1:2, x = c(10, 20))
  joined <- left_join(df_a, df_b, by = "id")
  result <- merge_suffix_columns(joined)

  expect_false(any(endsWith(names(result), ".x")))
  expect_false(any(endsWith(names(result), ".y")))
})

test_that("merge_suffix_columns leaves unpaired .x-only columns untouched", {
  df <- data.frame(id = 1:2, val.x = c(1, 2))  # no val.y counterpart
  result <- merge_suffix_columns(df)

  expect_named(result, c("id", "val.x"))
})

test_that("merge_suffix_columns handles a join with no suffix columns", {
  df <- data.frame(id = 1:2, a = c("x", "y"))
  expect_equal(merge_suffix_columns(df), df)
})
