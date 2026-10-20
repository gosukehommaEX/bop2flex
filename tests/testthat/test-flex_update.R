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
  # The BOP2 design does not stop for efficacy at an interim analysis, also
  # after the last planned one (35 patients), where the implied spending
  # function is positive
  expect_identical(b$bounds$efficacy, c(13L, 17L, 20L, 26L, 32L, 38L, 11L))
  expect_equal(type1(b, bop2), 0.093521486536, tolerance = 1e-10)
  nb <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.91, gamma = 1,
                  eta = 2.5, binding = FALSE)
  b <- flex_update(nb, c(12, 16, 19, 25, 31, 37, 39), final = TRUE)
  expect_identical(b$bounds$futility, c(1L, 2L, 3L, 5L, 7L, 10L, 11L))
  expect_identical(b$bounds$efficacy, c(8L, 9L, 9L, 10L, 12L, 12L, 12L))
  expect_equal(type1(b, nb), 0.077369122132, tolerance = 1e-10)
})

test_that("flex_update never claims efficacy below the planned cutoffs", {
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
    # nor below the naive boundaries, and the futility boundaries at the
    # interim analyses are the naive ones
    for (s in sch[1:20]) {
      b <- flex_update(d, s, final = TRUE)$bounds
      nv <- flex_naive(d, s, final = TRUE)$bounds
      floor <- vapply(b$m, function(m) {
        flex_efficacy_count(flex_posterior(0:m, m, d$p0), d$lambda)
      }, integer(1))
      interim <- seq_len(length(s) - 1)
      expect_true(all(b$efficacy >= floor))
      expect_true(all(b$efficacy >= nv$efficacy))
      expect_identical(b$futility[interim], nv$futility[interim])
      if (d$type == "BOP2") {
        expect_identical(b$efficacy[interim], as.integer(s[interim]) + 1L)
      }
    }
  }
})

test_that("the floor at an interim analysis is the efficacy cutoff", {
  # Expected values from the independent Python implementation. With the
  # O'Brien-Fleming type cutoff, C_E(7 / 40) requires 7 responses among 7
  # patients, whereas the spending function alone would allow 6.
  of <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.88, gamma = 0.95,
                  method = "OF")
  cut <- 2 * stats::pnorm(stats::qnorm((1 + 0.88) / 2) / sqrt(7 / 40)) - 1
  post <- stats::pbeta(0.2, 0.2 + 0:7, 0.8 + 7 - 0:7, lower.tail = FALSE)
  expect_identical(min(which(post >= cut)) - 1L, 7L)
  b <- flex_update(of, c(7, 15))
  expect_identical(b$bounds$efficacy, c(7L, 8L))
  expect_equal(b$bounds$alpha_spent, c(0.0000128, 0.004241360323),
               tolerance = 1e-9)
  b0 <- flex_update(of, c(7, 15), efficacy_floor = FALSE)
  expect_identical(b0$bounds$efficacy, c(6L, 8L))
  b <- flex_update(of, c(7, 18, 20, 25, 33, 36, 41), final = TRUE)
  expect_identical(b$bounds$futility, c(0L, 3L, 3L, 5L, 8L, 9L, 12L))
  expect_identical(b$bounds$efficacy, c(7L, 8L, 8L, 10L, 11L, 11L, 13L))
  expect_equal(type1(b, of), 0.091240128865, tolerance = 1e-10)
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
  # A BOP2 design given by its table does not stop for efficacy at an
  # interim analysis
  bop2 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
                    type = "BOP2")
  tab2 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1,
                    futility = bop2$boundary$futility[-7],
                    efficacy = bop2$boundary$efficacy)
  expect_identical(tab2$type, "BOP2")
  b3 <- flex_update(tab2, c(12, 16, 19, 25, 31, 37, 39), final = TRUE,
                    futility = c(2, 3, 4, 5, 7, 9, NA))
  expect_identical(b3$bounds$efficacy, c(13L, 17L, 20L, 26L, 32L, 38L, 11L))
  expect_equal(type1(b3, tab2), 0.093521486536, tolerance = 1e-10)
})

test_that("tables of calibrated designs give the planned boundaries", {
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
    tab <- flex_plan(p0 = d$p0, n = d$n, alpha = d$alpha,
                     futility = d$boundary$futility,
                     efficacy = d$boundary$efficacy, binding = d$binding)
    b <- flex_update(tab, d$n, final = TRUE)
    expect_identical(b$bounds$futility, d$boundary$futility)
    expect_identical(b$bounds$efficacy, d$boundary$efficacy)
  }
})

test_that("a boundary table sets the floor at its efficacy boundaries", {
  tab <- flex_plan(p0 = 0.2, n = n, alpha = 0.1,
                   futility = fe1$boundary$futility,
                   efficacy = fe1$boundary$efficacy)
  # Posterior probability computed directly from the beta distribution
  post <- function(s, m) {
    stats::pbeta(0.2, 0.2 + s, 0.8 + m - s, lower.tail = FALSE)
  }
  e <- fe1$boundary$efficacy
  reach <- e <= n
  cut <- min(post(e[reach], n[reach]))
  # Larger than the lambda of both grid points that give this table (0.87
  # and 0.88), so the final boundary at 50 patients is higher than with the
  # tuning parameters
  expect_gt(cut, 0.88)
  b <- flex_update(tab, c(45, 50), final = TRUE, futility = c(12, NA))
  s <- 0:50
  expect_identical(b$bounds$efficacy[2], min(s[post(s, 50) >= cut]))
  expect_gt(b$bounds$efficacy[2],
            flex_update(fe1, c(45, 50), final = TRUE)$bounds$efficacy[2])
  expect_true(type1(b, tab) <= tab$alpha * (1 + 1e-9))
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

test_that("flex_update can remove the efficacy floor", {
  # Without the floor the final efficacy boundary at 50 patients is 0
  b <- flex_update(fe1, c(45, 50), final = TRUE, efficacy_floor = FALSE)
  expect_identical(b$bounds$futility, c(12L, -1L))
  expect_identical(b$bounds$efficacy, c(14L, 0L))
  expect_equal(type1(b, fe1), 0.099454192392, tolerance = 1e-10)
  b <- flex_update(fe1, c(39, 40, 41), final = TRUE, efficacy_floor = FALSE)
  expect_identical(b$bounds$efficacy, c(12L, 12L, 0L))
  expect_equal(type1(b, fe1), 0.087505235922, tolerance = 1e-10)
  # Without the floor a BOP2 trial can stop for efficacy at an interim
  # analysis after the last planned one
  bop2 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
                    type = "BOP2")
  b <- flex_update(bop2, c(12, 16, 19, 25, 31, 37, 39), final = TRUE,
                   efficacy_floor = FALSE)
  expect_identical(b$bounds$efficacy, c(13L, 17L, 20L, 26L, 32L, 13L, 11L))
  expect_equal(b$bounds$alpha_spent[6], 0.020397744167, tolerance = 1e-9)
  sch <- flex_schedules(fe1, n_sim = 50, interim_shift = 3, final_shift = 6,
                        seed = 11)
  t1 <- vapply(sch, function(s) {
    type1(flex_update(fe1, s, final = TRUE, efficacy_floor = FALSE), fe1)
  }, numeric(1))
  expect_true(all(t1 <= fe1$alpha * (1 + 1e-9)))
  expect_error(flex_update(fe1, 10, efficacy_floor = NA), "'efficacy_floor'")
})

test_that("flex_update accepts another spending function", {
  # Independent Python values for the spending functions 0.1 t and 0.1 t^2.
  # The linear function exceeds the implied one at every planned interim
  # analysis, so with the floor it recovers the planned boundaries.
  lin <- data.frame(t = c(0, 1), alpha_cum = c(0, 0.1))
  expect_true(all(0.1 * fe1$n[-7] / 40 >= flex_spending(fe1)$alpha_cum[2:7]))
  b <- flex_update(fe1, n, final = TRUE, spending = lin)
  expect_identical(b$bounds$futility, fe1$boundary$futility)
  expect_identical(b$bounds$efficacy, fe1$boundary$efficacy)
  b <- flex_update(fe1, n, final = TRUE, spending = lin,
                   efficacy_floor = FALSE)
  expect_identical(b$bounds$futility, c(1L, 2L, 3L, 5L, 7L, 8L, 12L))
  expect_identical(b$bounds$efficacy, c(6L, 7L, 8L, 9L, 11L, 11L, 13L))
  expect_equal(type1(b, fe1), 0.089813386978, tolerance = 1e-10)
  b <- flex_update(fe1, c(12, 16, 19, 25, 31, 37, 39), final = TRUE,
                   spending = lin)
  expect_identical(b$bounds$efficacy, c(7L, 8L, 8L, 9L, 11L, 12L, 12L))
  expect_equal(type1(b, fe1), 0.087352614587, tolerance = 1e-10)
  tt <- seq(0, 1, length.out = 201)
  quad <- data.frame(t = tt, alpha_cum = 0.1 * tt^2)
  b <- flex_update(fe1, n, final = TRUE, spending = quad)
  expect_identical(b$bounds$efficacy, c(7L, 8L, 9L, 10L, 11L, 11L, 12L))
  expect_equal(type1(b, fe1), 0.085641340041, tolerance = 1e-10)
  # The implied spending function supplied explicitly gives the default
  imp <- flex_spending(fe1)
  expect_identical(flex_update(fe1, n, final = TRUE, spending = imp)$bounds,
                   flex_update(fe1, n, final = TRUE)$bounds)
})

test_that("flex_update checks the spending function", {
  expect_error(flex_update(fe1, 10, spending = data.frame(t = c(0, 0.5),
                                                          alpha_cum = c(0,
                                                                        0.1))),
               "'spending'")
  expect_error(flex_update(fe1, 10, spending = data.frame(t = c(0, 1),
                                                          alpha_cum = c(0,
                                                                        0.2))),
               "'spending'")
  expect_error(flex_update(fe1, 10, spending = data.frame(t = c(0, 1),
                                                          alpha_cum = c(0.01,
                                                                        0.1))),
               "'spending'")
  expect_error(flex_update(fe1, 10, spending = list(t = c(0, 1))),
               "'spending'")
})
