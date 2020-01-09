source("~/Github/junkyard/testthat/code/my_code.R", chdir = TRUE)

library(testthat)

test_that("empty vector", {
  expect_null(c())
})

test_that("test NA", {
  expect_true(is.na(increment(NA)))
})