# Expected values: Xu et al. (2026, Table 1) and Zhou et al. (2017, Table I)
# for the boundaries, and an independent Python implementation of the same
# grid search for the tuning parameters and operating characteristics.
n <- c(10, 15, 20, 25, 30, 35, 40)

test_that("flex_design selects the BOP2-FE-1 design of Xu et al. (2026)", {
  d <- flex_design(p0 = 0.2, p1 = 0.4, n = n)
  expect_equal(c(d$lambda, d$gamma, d$eta), c(0.87, 0.94, 2.4),
               tolerance = 1e-12)
  expect_identical(d$boundary$futility, c(1L, 2L, 3L, 5L, 7L, 8L, 11L))
  expect_identical(d$boundary$efficacy, c(6L, 8L, 9L, 9L, 10L, 11L, 12L))
  expect_equal(d$oc$type1, 0.098818200534, tolerance = 1e-10)
  expect_equal(d$oc$power, 0.890320443562, tolerance = 1e-10)
  expect_equal(d$oc$en0, 20.336082438939, tolerance = 1e-10)
  expect_identical(d$source, "calibration")
  expect_identical(d$calibration$n_candidates, 3175L)
})

test_that("flex_design selects the BOP2 design of Zhou et al. (2017)", {
  d <- flex_design(p0 = 0.2, p1 = 0.4, n = n, type = "BOP2")
  expect_equal(c(d$lambda, d$gamma), c(0.84, 0.64), tolerance = 1e-12)
  expect_identical(d$boundary$futility, c(1L, 2L, 4L, 5L, 7L, 9L, 10L))
  expect_identical(d$boundary$efficacy[7], 11L)
  expect_equal(d$oc$type1, 0.097428419140, tolerance = 1e-10)
  expect_equal(d$oc$power, 0.881487130225, tolerance = 1e-10)
  expect_equal(d$oc$en0, 20.292654305449, tolerance = 1e-10)
  expect_identical(d$calibration$n_candidates, 222L)
})

test_that("flex_design selects the O'Brien-Fleming type design", {
  skip_on_cran()
  # The selected boundaries are those of Supplementary Table S1 of Xu et al.
  # (2026); Table 2 of the article prints a different BOP2-FE-2 boundary.
  d <- flex_design(p0 = 0.2, p1 = 0.4, n = n, method = "OF")
  expect_equal(c(d$lambda, d$gamma), c(0.88, 0.95), tolerance = 1e-12)
  expect_identical(d$boundary$futility, c(1L, 2L, 3L, 5L, 7L, 9L, 11L))
  expect_identical(d$boundary$efficacy, c(7L, 8L, 8L, 9L, 10L, 11L, 12L))
  expect_equal(d$oc$type1, 0.098285718939, tolerance = 1e-10)
  expect_equal(d$oc$power, 0.886500068653, tolerance = 1e-10)
  expect_equal(d$oc$en0, 20.003747955081, tolerance = 1e-10)
  expect_identical(d$calibration$n_candidates, 502L)
})

test_that("flex_design handles non-binding futility", {
  skip_on_cran()
  d <- flex_design(p0 = 0.2, p1 = 0.4, n = n, binding = FALSE)
  expect_equal(c(d$lambda, d$gamma, d$eta), c(0.91, 1, 2.5), tolerance = 1e-12)
  expect_identical(d$boundary$futility, c(1L, 2L, 3L, 5L, 7L, 9L, 11L))
  expect_identical(d$boundary$efficacy, c(7L, 8L, 9L, 10L, 11L, 12L, 12L))
  expect_equal(d$oc$type1, 0.092100014895, tolerance = 1e-10)
  expect_equal(d$oc$power, 0.877081615488, tolerance = 1e-10)
})

test_that("flex_design handles another setting", {
  skip_on_cran()
  d <- flex_design(p0 = 0.1, p1 = 0.3, n = c(10, 15, 20, 25, 30))
  expect_equal(c(d$lambda, d$gamma, d$eta), c(0.84, 0.7, 1.8),
               tolerance = 1e-12)
  expect_identical(d$boundary$futility, c(0L, 1L, 2L, 3L, 5L))
  expect_identical(d$boundary$efficacy, c(4L, 5L, 5L, 5L, 6L))
  expect_equal(d$oc$type1, 0.099663455255, tolerance = 1e-10)
  expect_equal(d$oc$power, 0.906442026467, tolerance = 1e-10)
})

test_that("flex_design lists the grid points giving the same boundaries", {
  # Expected sizes and extreme members: an independent Python implementation
  # written from the article only, and a translation of this package.
  d <- flex_design(p0 = 0.2, p1 = 0.4, n = n)
  eq <- d$calibration$equivalent
  expect_identical(nrow(eq), 8L)
  expect_equal(unlist(eq[1, ]), c(lambda = 0.87, gamma = 0.94, eta = 2.4),
               tolerance = 1e-12)
  expect_equal(range(eq$lambda), c(0.87, 0.88), tolerance = 1e-12)
  top <- eq[eq$lambda == max(eq$lambda), ]
  expect_equal(unlist(top[1, ]), c(lambda = 0.88, gamma = 1, eta = 2.3),
               tolerance = 1e-12)
  for (r in seq_len(nrow(eq))) {
    x <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = eq$lambda[r],
                   gamma = eq$gamma[r], eta = eq$eta[r])
    expect_identical(x$boundary, d$boundary)
  }
  b <- flex_design(p0 = 0.2, p1 = 0.4, n = n, type = "BOP2")
  eb <- b$calibration$equivalent
  expect_identical(nrow(eb), 12L)
  expect_equal(unique(eb$lambda), c(0.84, 0.85), tolerance = 1e-12)
  expect_true(all(is.na(eb$eta)))
  top_b <- eb[eb$lambda == max(eb$lambda), ]
  expect_equal(unlist(top_b[1, c("lambda", "gamma")]),
               c(lambda = 0.85, gamma = 0.65), tolerance = 1e-12)
})

test_that("flex_design lists equivalent grid points for the OF design", {
  skip_on_cran()
  d <- flex_design(p0 = 0.2, p1 = 0.4, n = n, method = "OF")
  eq <- d$calibration$equivalent
  expect_identical(nrow(eq), 5L)
  expect_equal(unique(eq$lambda), 0.88, tolerance = 1e-12)
  expect_equal(eq$gamma[1], 0.95, tolerance = 1e-12)
})

test_that("flex_design checks its arguments", {
  expect_error(flex_design(p0 = 0.2, p1 = 0.1, n = n), "'p1'")
  expect_error(flex_design(p0 = 0.2, p1 = 0.4, n = n, gamma_grid = 0),
               "'gamma_grid'")
  expect_error(flex_design(p0 = 0.2, p1 = 0.4, n = n, lambda_grid = 2),
               "'lambda_grid'")
  expect_error(flex_design(p0 = 0.2, p1 = 0.4, n = n, alpha = 1e-6,
                           lambda_grid = 0.5, type = "BOP2"),
               "No design")
})
