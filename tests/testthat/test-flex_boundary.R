n <- c(10, 15, 20, 25, 30, 35, 40)
last <- c(rep(FALSE, 6), TRUE)

test_that("flex_boundary reproduces Xu et al. (2025, Table 1), BOP2-FE-1", {
  bd <- flex_boundary(n, 40, 0.2, 0.87, 0.94, 2.4, final = last)
  expect_identical(bd$futility, c(1L, 2L, 3L, 5L, 7L, 8L, 11L))
  expect_identical(bd$efficacy, c(6L, 8L, 9L, 9L, 10L, 11L, 12L))
  expect_identical(bd$final, last)
  expect_equal(bd$cutoff_futility[7], 0.87)
})

test_that("flex_boundary reproduces Zhou et al. (2017, Table I), BOP2", {
  # lambda = 0.84 and gamma = 0.64 are the values selected by flex_design()
  bd <- flex_boundary(n, 40, 0.2, 0.84, 0.64, type = "BOP2", final = last)
  expect_identical(bd$futility, c(1L, 2L, 4L, 5L, 7L, 9L, 10L))
  expect_identical(bd$efficacy, c(11L, 16L, 21L, 26L, 31L, 36L, 11L))
  expect_true(all(is.na(bd$cutoff_efficacy[1:6])))
})

test_that("flex_boundary gives the O'Brien-Fleming type boundaries", {
  # Supplementary Table S1 of Xu et al. (2025), BOP2-FE-2, single binary
  # endpoint; lambda = 0.88 and gamma = 0.95 are selected by flex_design()
  bd <- flex_boundary(n, 40, 0.2, 0.88, 0.95, type = "BOP2-FE",
                      method = "OF", final = last)
  expect_identical(bd$futility, c(1L, 2L, 3L, 5L, 7L, 9L, 11L))
  expect_identical(bd$efficacy, c(7L, 8L, 8L, 9L, 10L, 11L, 12L))
})

test_that("flex_boundary caps the information fraction at one", {
  interim <- flex_boundary(44, 40, 0.2, 0.87, 0.94, 2.4, final = FALSE)
  final <- flex_boundary(44, 40, 0.2, 0.87, 0.94, 2.4, final = TRUE)
  expect_equal(interim$t, 1)
  expect_identical(c(interim$futility, interim$efficacy), c(12L, 13L))
  expect_identical(c(final$futility, final$efficacy), c(12L, 13L))
})

test_that("flex_boundary agrees with the posterior probabilities", {
  bd <- flex_boundary(n, 40, 0.2, 0.87, 0.94, 2.4, final = last)
  for (i in seq_along(n)) {
    post <- flex_posterior(0:n[i], n[i], 0.2)
    f <- bd$futility[i]
    e <- bd$efficacy[i]
    expect_true(all(post[seq_len(f + 1)] < bd$cutoff_futility[i]) ||
                  bd$futility[i] == bd$efficacy[i] - 1)
    expect_true(all(post[(e + 1):(n[i] + 1)] >= bd$cutoff_efficacy[i]))
    expect_true(post[e] < bd$cutoff_efficacy[i])
  }
})

test_that("flex_boundary checks its arguments", {
  expect_error(flex_boundary(n, 40, 0.2, 0.87, 0.94), "'eta'")
  expect_error(flex_boundary(n, 40, 0.2, 0.87, 0, 2.4), "'gamma'")
  expect_error(flex_boundary(n, 40, 0.2, 1.2, 0.9, 2.4), "'lambda'")
  expect_error(flex_boundary(0, 40, 0.2, 0.87, 0.94, 2.4), "'m'")
  expect_error(flex_boundary(n, c(40, 50), 0.2, 0.87, 0.94, 2.4), "'N'")
  expect_error(flex_boundary(n, 40, 0.2, 0.87, 0.94, 2.4, final = c(TRUE,
                                                                     FALSE)),
               "'final'")
})
