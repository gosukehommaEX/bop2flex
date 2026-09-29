n <- c(10, 15, 20, 25, 30, 35, 40)

test_that("flex_spending gives the implied spending of BOP2-FE-1", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4)
  sp <- flex_spending(d)
  # Independent Python values
  expect_equal(sp$alpha_cum,
               c(0, 0.006369382400000006, 0.008556191842304006,
                 0.014644336846564228, 0.04800419083851749,
                 0.06899296283833029, 0.08639873230572939,
                 0.0988182005343823), tolerance = 1e-10)
  expect_equal(sp$t, c(0, n / 40))
  expect_identical(sp$look, 0:7)
  expect_equal(sp$alpha_cum[8], d$oc$type1, tolerance = 1e-14)
})

test_that("flex_spending uses non-binding futility when requested", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.91, gamma = 1,
                 eta = 2.5, binding = FALSE)
  expect_equal(flex_spending(d)$alpha_cum,
               c(0, 0.0008643584, 0.004497447747583999, 0.01130383910196215,
                 0.020744445137887892, 0.03208857145687885,
                 0.04469044123136747, 0.09210001489509292), tolerance = 1e-10)
})

test_that("flex_spending is zero at the interim analyses of BOP2", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
                 type = "BOP2")
  sp <- flex_spending(d)
  expect_equal(sp$alpha_cum[1:7], rep(0, 7))
  expect_equal(sp$alpha_cum[8], 0.097428419140, tolerance = 1e-10)
})

test_that("flex_spending requires a design", {
  expect_error(flex_spending(list()), "'design'")
})
