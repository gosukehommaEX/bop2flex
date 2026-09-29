# Expected values: independent Python implementation, which also reproduces
# the results of the earlier feasibility study.
n <- c(10, 15, 20, 25, 30, 35, 40)
fe1 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4, p1 = 0.4)
schedules <- list(c(10, 15, 20, 25, 30, 35, 44),
                  c(12, 17, 22, 27, 32, 37, 40),
                  c(10, 15, 25, 30, 35, 40),
                  c(10, 15, 17, 20, 25, 30, 35, 40))

test_that("flex_compare matches independent values", {
  res <- flex_compare(fe1, schedules)
  expect_identical(nrow(res), 8L)
  naive <- res[res$method == "naive", ]
  spend <- res[res$method == "spending", ]
  expect_equal(naive$type1, c(0.1001466338, 0.0992652942, 0.0990773795,
                              0.0968345078), tolerance = 1e-9)
  expect_equal(spend$type1, c(0.0904816619, 0.0830564519, 0.0949429172,
                              0.0963635649), tolerance = 1e-9)
  expect_equal(naive$power, c(0.8961125304, 0.9002411510, 0.8910120265,
                              0.8827105778), tolerance = 1e-9)
  expect_equal(spend$power, c(0.8855080424, 0.8925392633, 0.8895559088,
                              0.8825997159), tolerance = 1e-9)
  expect_equal(naive$en0, c(20.6783564571, 21.4584660723, 20.7040917697,
                            19.4669380895), tolerance = 1e-9)
  expect_equal(spend$en0, c(20.6783564571, 21.6105546692, 20.8176974533,
                            19.5060864368), tolerance = 1e-9)
  expect_equal(spend$en1, c(21.8422256601, 23.5194639180, 22.8816279355,
                            21.3964154921), tolerance = 1e-9)
  expect_identical(naive$exceeds_alpha, c(TRUE, FALSE, FALSE, FALSE))
  expect_false(any(spend$exceeds_alpha))
  expect_identical(res$looks[1], "10,15,20,25,30,35,44")
})

test_that("flex_compare accepts a single schedule and one method", {
  res <- flex_compare(fe1, c(12, 16, 19, 25, 31, 37, 39), methods = "spending")
  expect_identical(nrow(res), 1L)
  expect_equal(res$type1, 0.084972493469, tolerance = 1e-10)
  expect_equal(res$power, 0.897105659472, tolerance = 1e-10)
})

test_that("flex_compare returns NA power without p1", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4)
  res <- flex_compare(d, n)
  expect_true(all(is.na(res$power)))
  expect_equal(res$type1, rep(d$oc$type1, 2), tolerance = 1e-12)
})

test_that("flex_compare checks its arguments", {
  expect_error(flex_compare(fe1, list()), "'schedules'")
  expect_error(flex_compare(fe1, n, methods = "other"), "'arg'")
})
