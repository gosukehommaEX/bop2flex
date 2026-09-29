# Expected values were computed independently in Python with
# scipy.stats.beta.sf and cross-checked by numerical integration of the beta
# density with scipy.integrate.quad (maximum difference below 4e-14).

test_that("flex_posterior matches independent values (p0 = 0.2, m = 10)", {
  expected <- c(0.00857224779888, 0.150152964754, 0.439213595203,
                0.728274225653, 0.904420547333, 0.975717868013,
                0.995598851664, 0.999446783984, 0.999954497276,
                0.999997838655, 0.999999958613)
  expect_equal(flex_posterior(0:10, 10, 0.2), expected, tolerance = 1e-10)
})

test_that("flex_posterior matches independent values (p0 = 0.2, m = 40)", {
  expected <- c(4.20959972992e-06, 0.468733232566, 0.856541315866,
                0.999983684965)
  expect_equal(flex_posterior(c(0, 8, 11, 20), 40, 0.2), expected,
               tolerance = 1e-10)
  expect_equal(flex_posterior(40, 40, 0.2), 1, tolerance = 1e-12)
})

test_that("flex_posterior uses the supplied prior (Jeffreys prior)", {
  expected <- c(0.0180424100315, 0.425068661878, 0.97171309712)
  expect_equal(flex_posterior(c(3, 7, 12), 25, 0.3, a = 0.5, b = 0.5),
               expected, tolerance = 1e-10)
})

test_that("flex_posterior agrees with the beta-binomial identity", {
  # Under a uniform prior, Pr(p > p0 | y, m) = Pr(Bin(m + 1, p0) <= y).
  for (m in c(0, 1, 7, 30)) {
    y <- 0:m
    expect_equal(flex_posterior(y, m, 0.35, a = 1, b = 1),
                 stats::pbinom(y, m + 1, 0.35), tolerance = 1e-12)
  }
})

test_that("flex_posterior is non-decreasing in the number of responses", {
  v <- flex_posterior(0:40, 40, 0.3)
  expect_true(all(diff(v) >= 0))
  # Strictly increasing until the probability rounds to 1 in double precision
  below_one <- v < 1
  expect_true(sum(below_one) > 20)
  expect_true(all(diff(v[below_one]) > 0))
})

test_that("flex_posterior returns one value per element of y", {
  expect_length(flex_posterior(c(1, 1, 2), 5, 0.2), 3)
  expect_length(flex_posterior(0, 0, 0.2), 1)
  # With no patients the posterior equals the prior Beta(p0, 1 - p0).
  expect_equal(flex_posterior(0, 0, 0.2),
               stats::pbeta(0.2, 0.2, 0.8, lower.tail = FALSE),
               tolerance = 1e-12)
})

test_that("flex_posterior accepts whole numbers stored as doubles", {
  expect_equal(flex_posterior(0.3 * 10, 10, 0.2),
               flex_posterior(3L, 10L, 0.2), tolerance = 1e-12)
})

test_that("flex_posterior rejects invalid input", {
  expect_error(flex_posterior(11, 10, 0.2), "'y'")
  expect_error(flex_posterior(-1, 10, 0.2), "'y'")
  expect_error(flex_posterior(1.5, 10, 0.2), "'y'")
  expect_error(flex_posterior(NA, 10, 0.2), "'y'")
  expect_error(flex_posterior(1, 10.5, 0.2), "'m'")
  expect_error(flex_posterior(1, c(10, 20), 0.2), "'m'")
  expect_error(flex_posterior(1, 10, 0), "'p0'")
  expect_error(flex_posterior(1, 10, 1), "'p0'")
  expect_error(flex_posterior(1, 10, c(0.2, 0.3)), "'p0'")
  expect_error(flex_posterior(1, 10, 0.2, a = 0), "'a'")
  expect_error(flex_posterior(1, 10, 0.2, b = -1), "'b'")
})
