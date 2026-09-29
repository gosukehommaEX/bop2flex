# Expected values were computed with an independent Python implementation of
# the same procedure.
n <- c(10, 15, 20, 25, 30, 35, 40)
fe1 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4, p1 = 0.4)
type1 <- function(b, design) {
  flex_oc(b$bounds$m, b$bounds$futility, b$bounds$efficacy, design$p0,
          binding = design$binding)$reject
}

test_that("flex_update recovers the planned boundaries", {
  designs <- list(
    fe1,
    flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
              type = "BOP2"),
    flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.88, gamma = 0.95,
              method = "OF"),
    flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.91, gamma = 1,
              eta = 2.5, binding = FALSE),
    flex_plan(p0 = 0.1, n = c(10, 15, 20, 25, 30), alpha = 0.1, lambda = 0.84,
              gamma = 0.7, eta = 1.8)
  )
  for (d in designs) {
    b <- flex_update(d, d$n, final = TRUE)
    expect_identical(b$bounds$futility, d$boundary$futility)
    expect_identical(b$bounds$efficacy, d$boundary$efficacy)
    expect_equal(b$bounds$alpha_spent, flex_spending(d)$alpha_cum[-1],
                 tolerance = 1e-12)
  }
})

test_that("flex_update gives the boundaries of the worked example", {
  b <- flex_update(fe1, c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
  expect_s3_class(b, "flex_bounds")
  expect_identical(b$bounds$futility, c(1L, 2L, 3L, 5L, 7L, 9L, 11L))
  expect_identical(b$bounds$efficacy, c(7L, 8L, 9L, 9L, 11L, 12L, 12L))
  expect_equal(b$bounds$alpha_target,
               c(0.007244106177, 0.009773820843, 0.013426707846,
                 0.048004190839, 0.072474116732, 0.091366519597, 0.1),
               tolerance = 1e-9)
  expect_equal(b$bounds$alpha_spent,
               c(0.003903131648, 0.008364206804, 0.010706936229,
                 0.047798051561, 0.055614321787, 0.071558518756,
                 0.084972493469), tolerance = 1e-9)
  expect_equal(type1(b, fe1), 0.084972493469, tolerance = 1e-10)
})

test_that("flex_update handles BOP2 and non-binding designs", {
  bop2 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
                    type = "BOP2")
  b <- flex_update(bop2, c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
  expect_identical(b$bounds$futility, c(2L, 3L, 4L, 5L, 7L, 9L, 10L))
  # An interim analysis after the last planned one (35 patients) receives
  # part of the error rate through the linear spending function.
  expect_identical(b$bounds$efficacy, c(13L, 17L, 20L, 26L, 32L, 13L, 11L))
  expect_equal(type1(b, bop2), 0.093521486536, tolerance = 1e-10)
  nb <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.91, gamma = 1,
                  eta = 2.5, binding = FALSE)
  b <- flex_update(nb, c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
  expect_identical(b$bounds$futility, c(1L, 2L, 3L, 5L, 7L, 10L, 11L))
  expect_identical(b$bounds$efficacy, c(8L, 9L, 9L, 10L, 12L, 12L, 12L))
  expect_equal(type1(b, nb), 0.077369122132, tolerance = 1e-10)
})

test_that("flex_update never claims efficacy below the final cutoff", {
  # Without the floor the final efficacy boundary at 50 patients would be 0.
  b <- flex_update(fe1, c(45, 50), final = TRUE)
  expect_identical(b$bounds$futility, c(12L, 13L))
  expect_identical(b$bounds$efficacy, c(14L, 14L))
  expect_equal(type1(b, fe1), 0.083927488883, tolerance = 1e-10)
  b <- flex_update(fe1, c(39, 40, 41), final = TRUE)
  expect_identical(b$bounds$efficacy, c(12L, 12L, 12L))
  expect_equal(type1(b, fe1), 0.087505235922, tolerance = 1e-10)
  b <- flex_update(fe1, 40, final = TRUE)
  expect_identical(c(b$bounds$futility, b$bounds$efficacy), c(11L, 12L))
  b <- flex_update(fe1, c(5, 6, 7, 8, 9, 10, 40), final = TRUE)
  expect_identical(b$bounds$futility, c(0L, 0L, 0L, 0L, 1L, 1L, 11L))
  expect_identical(b$bounds$efficacy, c(5L, 5L, 6L, 6L, 6L, 7L, 12L))
  expect_equal(type1(b, fe1), 0.072359556402, tolerance = 1e-10)
})

test_that("flex_update controls the type I error rate on random schedules", {
  designs <- list(
    fe1,
    flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
              type = "BOP2"),
    flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.88, gamma = 0.95,
              method = "OF"),
    flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.91, gamma = 1,
              eta = 2.5, binding = FALSE)
  )
  for (d in designs) {
    sch <- flex_schedules(d, n_sim = 100, interim_shift = 3, final_shift = 6,
                          seed = 2026)
    t1 <- vapply(sch, function(s) type1(flex_update(d, s, final = TRUE), d),
                 numeric(1))
    expect_true(all(t1 <= d$alpha * (1 + 1e-9)))
    # The efficacy boundaries never fall below the final posterior cutoff
    for (s in sch[1:20]) {
      b <- flex_update(d, s, final = TRUE)$bounds
      floor <- vapply(b$m, function(m) {
        flex_efficacy_count(flex_posterior(0:m, m, d$p0), d$lambda)
      }, integer(1))
      expect_true(all(b$efficacy >= floor))
    }
  }
})

test_that("flex_update gives the same earlier boundaries at each analysis", {
  full <- flex_update(fe1, c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
  part <- flex_update(fe1, c(12, 16, 19))
  expect_identical(part$bounds$efficacy, full$bounds$efficacy[1:3])
  expect_identical(part$bounds$futility, full$bounds$futility[1:3])
  expect_false(any(part$bounds$final))
})

test_that("flex_update works with a design given by its boundary table", {
  tab <- flex_plan(p0 = 0.2, n = n, alpha = 0.1,
                   futility = c(1, 2, 3, 5, 7, 8),
                   efficacy = c(6, 8, 9, 9, 10, 11, 12))
  b <- flex_update(tab, n, final = TRUE)
  expect_identical(b$bounds$efficacy, fe1$boundary$efficacy)
  expect_error(flex_update(tab, c(12, 16), final = FALSE), "'futility'")
  b2 <- flex_update(tab, c(12, 16), futility = c(1, 2))
  expect_identical(b2$bounds$futility, c(1L, 2L))
  expect_identical(b2$bounds$efficacy, c(7L, 8L))
})

test_that("flex_update uses the futility boundaries supplied", {
  b <- flex_update(fe1, c(12, 16), futility = c(NA, 0))
  expect_identical(b$bounds$futility, c(1L, 0L))
  expect_error(flex_update(fe1, c(12, 16), futility = 1), "one element")
  expect_error(flex_update(fe1, c(12, 16), futility = c(1, 20)), "between -1")
})

test_that("flex_update reports decisions", {
  b <- flex_update(fe1, c(10, 15), y = c(3, 8))
  expect_identical(b$bounds$decision, c("continue", "stop for efficacy"))
  b <- flex_update(fe1, c(10, 15, 20, 25, 30, 35, 40), final = TRUE,
                   y = c(2, 3, 5, 6, 8, 9, 11))
  expect_identical(b$bounds$decision, c(rep("continue", 6), "no-go"))
})

test_that("flex_update checks its arguments", {
  expect_error(flex_update(list(), 10), "'design'")
  expect_error(flex_update(fe1, c(10, 5)), "cumulative")
  expect_error(flex_update(fe1, 10, final = NA), "'final'")
})
