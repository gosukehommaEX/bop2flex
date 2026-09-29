test_that("flex_check_design accepts design objects", {
  design <- flex_plan(p0 = 0.2, n = c(10, 20), alpha = 0.1,
                      futility = 1, efficacy = c(NA, 7))
  expect_invisible(flex_check_design(design))
})

test_that("flex_check_design rejects other objects", {
  expect_error(flex_check_design(list(p0 = 0.2)), "flex_design\\(\\)")
  expect_error(flex_check_design(NULL), "'design'")
})
