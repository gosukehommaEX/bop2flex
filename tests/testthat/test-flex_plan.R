n <- c(10, 15, 20, 25, 30, 35, 40)

test_that("flex_plan builds the design from tuning parameters", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4, p1 = 0.4)
  expect_s3_class(d, "flex_design")
  expect_identical(d$boundary$futility, c(1L, 2L, 3L, 5L, 7L, 8L, 11L))
  expect_identical(d$boundary$efficacy, c(6L, 8L, 9L, 9L, 10L, 11L, 12L))
  expect_equal(d$oc$type1, 0.098818200534, tolerance = 1e-10)
  expect_equal(d$oc$power, 0.890320443562, tolerance = 1e-10)
  expect_equal(d$oc$en0, 20.336082438939, tolerance = 1e-10)
  expect_equal(d$oc$en1, 21.673110347581, tolerance = 1e-10)
  expect_identical(d$type, "BOP2-FE")
  expect_identical(d$method, "power")
  expect_identical(d$source, "parameters")
  expect_identical(d$N, 40L)
})

test_that("flex_plan builds the same design from its boundary table", {
  d1 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                  eta = 2.4, p1 = 0.4)
  d2 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, futility = c(1, 2, 3, 5, 7, 8),
                  efficacy = c(6, 8, 9, 9, 10, 11, 12), p1 = 0.4)
  expect_identical(d2$boundary, d1$boundary)
  expect_equal(d2$oc, d1$oc, tolerance = 1e-14)
  expect_null(d2$lambda)
  expect_identical(d2$source, "table")
  expect_identical(d2$type, "BOP2-FE")
})

test_that("flex_plan infers the BOP2 design from a table", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, futility = c(1, 2, 4, 5, 7, 9),
                 efficacy = c(rep(NA, 6), 11), p1 = 0.4)
  expect_identical(d$type, "BOP2")
  expect_equal(d$oc$type1, 0.097428419140, tolerance = 1e-10)
  expect_equal(d$oc$power, 0.881487130225, tolerance = 1e-10)
})

test_that("flex_plan stores the O'Brien-Fleming type design", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.88, gamma = 0.95,
                 method = "OF", p1 = 0.4)
  expect_identical(d$method, "OF")
  expect_null(d$eta)
  expect_equal(d$oc$type1, 0.098285718939, tolerance = 1e-10)
})

test_that("flex_plan computes the non-binding type I error rate", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.2, lambda = 0.87, gamma = 0.94,
                 eta = 2.4, binding = FALSE)
  expect_equal(d$oc$type1, 0.116814959248, tolerance = 1e-10)
  expect_equal(d$oc$en0, 20.336082438939, tolerance = 1e-10)
  expect_true(is.na(d$oc$power))
})

test_that("flex_plan checks its arguments", {
  expect_error(flex_plan(p0 = 0.2, n = n, alpha = 0.05, lambda = 0.87,
                         gamma = 0.94, eta = 2.4), "exceeds 'alpha'")
  expect_error(flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87,
                         gamma = 0.94, eta = 2.4, futility = 1:6,
                         efficacy = 1:7), "but not both")
  expect_error(flex_plan(p0 = 0.2, n = n, alpha = 0.1), "Supply either")
  expect_error(flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87),
               "Both 'lambda' and 'gamma'")
  expect_error(flex_plan(p0 = 0.2, n = c(10, 5, 5, 5, 5, 5, 5), alpha = 0.1,
                         lambda = 0.87, gamma = 0.94, eta = 2.4), "cumulative")
  expect_error(flex_plan(p0 = 0.2, n = 40, alpha = 0.1, lambda = 0.87,
                         gamma = 0.94, eta = 2.4), "at least one interim")
  expect_error(flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87,
                         gamma = 0.94, eta = 2.4, p1 = 0.2), "'p1'")
  expect_error(flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87,
                         gamma = 0.94), "'eta'")
})
