test_that("flex_check_boundary completes the final futility boundary", {
  out <- flex_check_boundary(c(10L, 20L), 1, c(6, 7))
  expect_identical(out$futility, c(1L, 6L))
  expect_identical(out$efficacy, c(6L, 7L))
  out2 <- flex_check_boundary(c(10L, 20L), c(1, 6), c(6, 7))
  expect_identical(out2, out)
  out3 <- flex_check_boundary(c(10L, 20L), c(1, NA), c(6, 7))
  expect_identical(out3, out)
})

test_that("flex_check_boundary replaces missing interim efficacy", {
  out <- flex_check_boundary(c(10L, 15L, 20L), c(1, 2), c(NA, NA, 7))
  expect_identical(out$efficacy, c(11L, 16L, 7L))
})

test_that("flex_check_boundary rejects invalid tables", {
  n <- c(10L, 20L)
  expect_error(flex_check_boundary(n, c(1, 2, 3), c(6, 7)), "one element")
  expect_error(flex_check_boundary(n, 1, c(6, NA)), "final analysis")
  expect_error(flex_check_boundary(n, NA, c(6, 7)), "must not be NA")
  expect_error(flex_check_boundary(n, c(1, 5), c(6, 7)), "minus one")
  expect_error(flex_check_boundary(n, 1.5, c(6, 7)), "whole numbers")
  expect_error(flex_check_boundary(n, -2, c(6, 7)), "between -1")
  expect_error(flex_check_boundary(n, 1, c(12, 7)), "between -1")
  expect_error(flex_check_boundary(n, 6, c(6, 7)), "smaller than")
  expect_error(flex_check_boundary(n, "1", c(6, 7)), "numeric")
})
