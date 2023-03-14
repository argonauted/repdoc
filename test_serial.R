source("serial.R")

testthat::test_that("firstTry", {
  testthat::expect_that(2,testthat::equals(2))
})

testthat::test_that("secondTry", {
  testthat::expect_that(2,testthat::equals(3))
})