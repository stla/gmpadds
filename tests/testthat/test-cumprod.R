context("cumprod")

library(gmp)
test_that("cumprod", {
  expect_identical(cumprod(as.bigz(c(2, 3, 4))), factorialZ(2:4))
  expect_identical(cumprod(as.bigq(c(2, 3, 4), 2)), factorialZ(2:4)/c(2,4,8))
})
