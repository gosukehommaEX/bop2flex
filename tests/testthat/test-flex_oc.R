n <- c(10, 15, 20, 25, 30, 35, 40)

test_that("flex_oc matches independent exact values", {
  # Independent Python implementation (numpy convolution)
  o <- flex_oc(n, c(1, 2, 3, 5, 7, 8), c(6, 8, 9, 9, 10, 11, 12), c(0.2, 0.4))
  expect_equal(o$reject, c(0.098818200534, 0.890320443562), tolerance = 1e-10)
  expect_equal(o$early_efficacy, c(0.086398732306, 0.867194161050),
               tolerance = 1e-10)
  expect_equal(o$early_futility, c(0.828032763155, 0.090527010828),
               tolerance = 1e-10)
  expect_equal(o$en, c(20.336082438939, 21.673110347581), tolerance = 1e-10)
  nb <- flex_oc(n, c(1, 2, 3, 5, 7, 8), c(6, 8, 9, 9, 10, 11, 12), 0.2,
                binding = FALSE)
  expect_equal(nb$reject, 0.116814959248, tolerance = 1e-10)
  expect_equal(nb$en, 38.765166251023, tolerance = 1e-10)
})

test_that("flex_oc agrees with the simulations of Zhou et al. (2017)", {
  # Table II, scenario 1 (10,000 simulated trials): PRN, PET and sample size.
  # The PRN of 55.2 percent at ORR 0.3 is not used: with PET 46.2 percent the
  # two add to more than 100 percent, and Xu et al. (2026, Table 3) report
  # 52.8 percent for the same boundaries. The tolerances are about three
  # simulation standard errors plus the rounding of the published values.
  o <- flex_oc(n, c(1, 2, 4, 5, 7, 9), c(rep(NA, 6), 11),
               c(0.2, 0.3, 0.4, 0.5))
  expect_lt(max(abs(o$reject[-2] - c(0.096, 0.883, 0.982))), 0.016)
  expect_lt(max(abs(o$early_futility - c(0.888, 0.462, 0.114, 0.018))), 0.016)
  expect_lt(max(abs(o$en - c(20.2, 31.0, 37.6, 39.5))), 0.4)
  expect_equal(o$early_efficacy, rep(0, 4))
})

test_that("flex_oc agrees with the simulations of Xu et al. (2026)", {
  # Table 3, scenario 1 (10,000 simulated trials), percent rejecting and
  # average sample size
  p <- c(0.2, 0.3, 0.4, 0.5)
  bop2 <- flex_oc(n, c(1, 2, 4, 5, 7, 9), c(rep(NA, 6), 11), p)
  expect_lt(max(abs(bop2$reject - c(0.097, 0.528, 0.881, 0.982))), 0.016)
  expect_lt(max(abs(bop2$en - c(20.3, 31.1, 37.6, 39.6))), 0.4)
  fe1 <- flex_oc(n, c(1, 2, 3, 5, 7, 8), c(6, 8, 9, 9, 10, 11, 12), p)
  expect_lt(max(abs(fe1$reject - c(0.097, 0.531, 0.890, 0.986))), 0.016)
  expect_lt(max(abs(fe1$en - c(20.3, 24.7, 21.8, 16.8))), 0.4)
  # BOP2-FE-2 boundaries of Table 2
  fe2 <- flex_oc(n, c(1, 2, 4, 5, 7, 8), c(7, 7, 8, 9, 10, 11, 12), p)
  expect_lt(max(abs(fe2$reject - c(0.100, 0.523, 0.879, 0.983))), 0.016)
  expect_lt(max(abs(fe2$en - c(19.2, 23.1, 20.3, 16.4))), 0.4)
  # Exact values from the independent Python implementation
  expect_equal(fe2$reject, c(0.099929123404, 0.526455152596, 0.881974164894,
                             0.982476042824), tolerance = 1e-10)
  expect_equal(fe2$en, c(19.171030940045, 23.199814214815, 20.374649557479,
                         16.372535730334), tolerance = 1e-10)
})

test_that("flex_oc accepts a final futility boundary and checks input", {
  a <- flex_oc(n, c(1, 2, 3, 5, 7, 8), c(6, 8, 9, 9, 10, 11, 12), 0.3)
  b <- flex_oc(n, c(1, 2, 3, 5, 7, 8, 11), c(6, 8, 9, 9, 10, 11, 12), 0.3)
  expect_identical(a, b)
  expect_error(flex_oc(n, c(1, 2, 3, 5, 7, 8), c(6, 8, 9, 9, 10, 11, 12),
                       1.2), "'p'")
  expect_error(flex_oc(c(10, 5, 5), c(1, 2), c(NA, NA, 7), 0.2), "cumulative")
  expect_error(flex_oc(n, c(1, 2, 3, 5, 7, 8), c(6, 8, 9, 9, 10, 11, 12), 0.2,
                       binding = NA), "'binding'")
})
