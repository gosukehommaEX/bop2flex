test_that("flex_check_number accepts values in the range", {
  expect_invisible(flex_check_number(0.5, "x", 0, 1))
  expect_equal(flex_check_number(0, "x", 0, 1), 0)
  expect_equal(flex_check_number(1, "x", 0, 1), 1)
})

test_that("flex_check_number respects open limits", {
  expect_error(flex_check_number(0, "x", 0, 1, open_lower = TRUE),
               "'x' must be a single number in the range \\(0, 1\\]")
  expect_error(flex_check_number(1, "x", 0, 1, open_upper = TRUE),
               "\\[0, 1\\)")
})

test_that("flex_check_number rejects invalid input", {
  expect_error(flex_check_number(c(0.1, 0.2), "x", 0, 1), "'x'")
  expect_error(flex_check_number(NA_real_, "x", 0, 1), "'x'")
  expect_error(flex_check_number("0.1", "x", 0, 1), "'x'")
  expect_error(flex_check_number(Inf, "x"), "'x'")
  expect_error(flex_check_number(2, "x", 0, 1), "'x'")
})
