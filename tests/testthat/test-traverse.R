test_that("get_list_names returns paths of a flat named list", {
  lst <- list(a = 1, b = 2, c = 3)
  expect_equal(get_list_names(lst), c("a", "b", "c"))
})

test_that("get_list_names handles nested named lists", {
  lst    <- list(a = 1, b = list(c = 2, d = list(e = 3)))
  result <- get_list_names(lst)
  expect_equal(result, c("a", "b", "b$c", "b$d", "b$d$e"))
})

test_that("get_list_names uses [[i]] notation for unnamed elements", {
  lst <- list(1, 2, 3)
  expect_equal(get_list_names(lst), c("[[1]]", "[[2]]", "[[3]]"))
})

test_that("get_list_names mixes named and unnamed elements correctly", {
  lst    <- list(1, list(2, 3))
  result <- get_list_names(lst)
  expect_equal(result, c("[[1]]", "[[2]]", "[[2]][[1]]", "[[2]][[2]]"))
})

test_that("get_list_names returns character(0) for non-list input", {
  expect_equal(get_list_names(42),     character(0))
  expect_equal(get_list_names("text"), character(0))
  expect_equal(get_list_names(NULL),   character(0))
})
