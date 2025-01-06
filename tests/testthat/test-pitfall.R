test_that("pitfall works", {

  x_vals <- seq(-10, 10, length.out = 100)
  result <- pitfall(
    d = function(x) dnorm(x, mean = 0, sd = 4),
    p = function(x) dnorm(x, mean = 0, sd = 16),
    arglist = list(x = x_vals),
    method = "difference"
  )

  expect_equal(length(result), 4)
})
