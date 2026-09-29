test_that("flex_check_looks returns integers", {
  out <- flex_check_looks(c(10, 15, 20), "n")
  expect_identical(out, c(10L, 15L, 20L))
  expect_identical(flex_check_looks(40, "n"), 40L)
})

test_that("flex_check_looks rejects increments and invalid values", {
  expect_error(flex_check_looks(c(10, 5, 5), "n"), "cumulative")
  expect_error(flex_check_looks(c(10, 10), "n"), "strictly increasing")
  expect_error(flex_check_looks(c(0, 10), "n"), "positive whole")
  expect_error(flex_check_looks(c(10.5, 20), "m"), "'m'")
  expect_error(flex_check_looks(numeric(0), "n"), "'n'")
})
