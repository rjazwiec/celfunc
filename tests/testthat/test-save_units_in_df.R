# save_units_in_df ---------------------------------------------------------

test_that("save_units_in_df returns only unit-bearing columns", {
  skip_if_not_installed("units")
  df <- data.frame(
    conc  = units::set_units(c(10, 20), "ng/mL"),
    time  = units::set_units(c(0, 1),   "h"),
    label = c("a", "b")        # no units
  )
  spec <- save_units_in_df(df)
  expect_equal(nrow(spec), 2L)
  expect_named(spec, c("column_name", "unit"))
  expect_setequal(spec$column_name, c("conc", "time"))
})

test_that("save_units_in_df returns empty data frame when no units columns", {
  df   <- data.frame(x = 1:3, y = letters[1:3])
  spec <- save_units_in_df(df)
  expect_equal(nrow(spec), 0L)
})

test_that("save_units_in_df records the correct unit strings", {
  skip_if_not_installed("units")
  df <- data.frame(conc = units::set_units(c(5, 10), "ng/mL"))
  spec <- save_units_in_df(df)
  # deparse_unit() returns udunits2 canonical form ("ng mL-1" for "ng/mL")
  expect_equal(spec$unit[spec$column_name == "conc"],
               units::deparse_unit(df$conc))
})


# apply_units_from_df ------------------------------------------------------

test_that("apply_units_from_df restores units to the correct columns", {
  skip_if_not_installed("units")
  df <- data.frame(conc = c(10, 20), time = c(0, 1))
  spec <- data.frame(column_name = c("conc", "time"),
                     unit        = c("ng/mL", "h"),
                     stringsAsFactors = FALSE)
  result <- apply_units_from_df(df, spec)
  expect_true(inherits(result$conc, "units"))
  expect_true(inherits(result$time, "units"))
  # Units are round-tripped through deparse_unit; both sides go through the
  # same normalization so the canonical strings match
  expect_equal(units::deparse_unit(result$conc),
               units::deparse_unit(units::set_units(1, "ng/mL")))
})

test_that("apply_units_from_df silently skips missing columns", {
  df <- data.frame(x = 1:3)
  spec <- data.frame(column_name = "y", unit = "mg", stringsAsFactors = FALSE)
  expect_no_error(apply_units_from_df(df, spec))
  expect_named(apply_units_from_df(df, spec), "x")
})

test_that("save/apply round-trip preserves values and units", {
  skip_if_not_installed("units")
  df <- data.frame(
    conc = units::set_units(c(10, 20, 30), "ng/mL"),
    time = units::set_units(c(0, 1, 2),   "h")
  )
  spec       <- save_units_in_df(df)
  df_stripped <- data.frame(conc = c(10, 20, 30), time = c(0, 1, 2))
  result      <- apply_units_from_df(df_stripped, spec)

  expect_equal(as.numeric(result$conc), as.numeric(df$conc))
  expect_equal(units::deparse_unit(result$conc),
               units::deparse_unit(df$conc))
})
