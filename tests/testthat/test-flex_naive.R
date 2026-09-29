n <- c(10, 15, 20, 25, 30, 35, 40)
fe1 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4, p1 = 0.4)

test_that("flex_naive inflates the type I error rate in the worked example", {
  # Independent Python values
  b <- flex_naive(fe1, c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
  expect_s3_class(b, "flex_bounds")
  expect_identical(b$method, "naive")
  expect_identical(b$bounds$futility, c(1L, 2L, 3L, 5L, 7L, 9L, 10L))
  expect_identical(b$bounds$efficacy, c(7L, 8L, 8L, 9L, 10L, 11L, 11L))
  expect_equal(b$bounds$alpha_spent,
               c(0.0039031316479999994, 0.008364206804172799,
                 0.023972324116267013, 0.05127549340311768,
                 0.08286365143224941, 0.11137284424228794,
                 0.1272429387770368), tolerance = 1e-10)
  expect_true(all(is.na(b$bounds$alpha_target)))
})

test_that("flex_naive recovers the planned boundaries on the plan", {
  b <- flex_naive(fe1, n, final = TRUE)
  expect_identical(b$bounds$futility, fe1$boundary$futility)
  expect_identical(b$bounds$efficacy, fe1$boundary$efficacy)
  bop2 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
                    type = "BOP2")
  b <- flex_naive(bop2, n, final = TRUE)
  expect_identical(b$bounds$efficacy, bop2$boundary$efficacy)
})

test_that("flex_naive requires the tuning parameters", {
  tab <- flex_plan(p0 = 0.2, n = n, alpha = 0.1,
                   futility = c(1, 2, 3, 5, 7, 8),
                   efficacy = c(6, 8, 9, 9, 10, 11, 12))
  expect_error(flex_naive(tab, n, final = TRUE), "tuning parameters")
})

test_that("flex_naive reports decisions", {
  b <- flex_naive(fe1, c(12, 16), y = c(4, 5))
  expect_identical(b$bounds$decision, c("continue", "continue"))
})
