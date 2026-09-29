# Expected values were computed independently in Python (scipy.stats.norm).

test_that("flex_cutoff gives the power cutoffs", {
  t <- c(10, 20, 35) / 40
  out <- flex_cutoff(t, 0.87, 0.94, 2.4, "BOP2-FE", "power")
  expect_equal(out$futility,
               c(0.23636483259941765, 0.4534726059658878, 0.7673735441635585),
               tolerance = 1e-12)
  expect_equal(out$efficacy,
               c(0.9953334129328245, 0.9753696057942061, 0.9056454865331633),
               tolerance = 1e-12)
})

test_that("flex_cutoff gives the O'Brien-Fleming type cutoff", {
  t <- c(10, 20, 35) / 40
  out <- flex_cutoff(t, 0.88, 0.95, NULL, "BOP2-FE", "OF")
  expect_equal(out$efficacy,
               c(0.9981262564887075, 0.972106566165835, 0.9035118625549274),
               tolerance = 1e-12)
  expect_equal(out$futility,
               c(0.23579016175798448, 0.45551656649020617,
                 0.7751581588583608), tolerance = 1e-12)
})

test_that("flex_cutoff reduces to lambda at t = 1 and caps t", {
  for (method in c("power", "OF")) {
    out <- flex_cutoff(c(1, 1.2), 0.87, 0.94, 2.4, "BOP2-FE", method)
    expect_equal(out$futility, c(0.87, 0.87), tolerance = 1e-12)
    expect_equal(out$efficacy, c(0.87, 0.87), tolerance = 1e-12)
  }
})

test_that("flex_cutoff returns NA efficacy cutoffs for BOP2", {
  out <- flex_cutoff(c(0.25, 0.5), 0.84, 0.64, NULL, "BOP2")
  expect_true(all(is.na(out$efficacy)))
  expect_equal(out$futility, 0.84 * c(0.25, 0.5)^0.64, tolerance = 1e-12)
})

test_that("flex_cutoff is vectorised over gamma and eta", {
  out <- flex_cutoff(0.5, 0.8, c(0.5, 1), c(1, 2), "BOP2-FE", "power")
  expect_equal(out$futility, 0.8 * 0.5^c(0.5, 1), tolerance = 1e-12)
  expect_equal(out$efficacy, 1 - 0.2 * 0.5^c(1, 2), tolerance = 1e-12)
})
