test_that("flex_propagate reproduces the binomial distribution", {
  one <- flex_propagate(matrix(1, 1, 1), 10, 0.3)
  expect_equal(as.vector(one), stats::dbinom(0:10, 10, 0.3),
               tolerance = 1e-14)
  two <- flex_propagate(flex_propagate(matrix(1, 1, 1), 4, 0.3), 6, 0.3)
  expect_equal(as.vector(two), stats::dbinom(0:10, 10, 0.3),
               tolerance = 1e-14)
})

test_that("flex_propagate handles several rows and zero increments", {
  dist <- rbind(c(0.5, 0.5), c(1, 0))
  out <- flex_propagate(dist, 1, 0.4)
  expect_equal(out, rbind(c(0.3, 0.5, 0.2), c(0.6, 0.4, 0)),
               tolerance = 1e-14)
  expect_identical(flex_propagate(dist, 0, 0.4), dist)
})
