n <- c(10, 15, 20, 25, 30, 35, 40)
fe1 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4)

test_that("print.flex_bounds describes the boundaries", {
  b <- flex_update(fe1, c(12, 16, 19))
  expect_output(print(b), "implied error spending function")
  expect_output(print(b), "planned N = 40")
  capture_output(res <- withVisible(print(b)))
  expect_false(res$visible)
  expect_identical(res$value, b)
  nv <- flex_naive(fe1, c(12, 16, 19))
  expect_output(print(nv), "Naive boundaries")
})
