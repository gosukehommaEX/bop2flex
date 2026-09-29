test_that("flex_check_flag accepts TRUE and FALSE", {
  expect_invisible(flex_check_flag(TRUE, "f"))
  expect_false(flex_check_flag(FALSE, "f"))
})

test_that("flex_check_flag rejects other input", {
  expect_error(flex_check_flag(NA, "f"), "'f' must be TRUE or FALSE")
  expect_error(flex_check_flag(c(TRUE, FALSE), "f"), "'f'")
  expect_error(flex_check_flag(1, "f"), "'f'")
})
