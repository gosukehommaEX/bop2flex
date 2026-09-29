test_that("flex_alpha_at interpolates linearly and caps t at one", {
  sp <- data.frame(t = c(0, 0.5, 1), alpha_cum = c(0, 0.02, 0.1))
  expect_equal(flex_alpha_at(c(0, 0.25, 0.5, 0.75, 1, 1.3), sp),
               c(0, 0.01, 0.02, 0.06, 0.1, 0.1), tolerance = 1e-14)
})
