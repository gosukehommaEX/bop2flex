test_that("flex_is_whole accepts whole numbers", {
  expect_true(flex_is_whole(c(0, 1, 40)))
  expect_true(flex_is_whole(3L))
  expect_true(flex_is_whole(0.3 * 10))
  expect_true(flex_is_whole(-2))
})

test_that("flex_is_whole rejects other input", {
  expect_false(flex_is_whole(1.5))
  expect_false(flex_is_whole(c(1, NA)))
  expect_false(flex_is_whole(Inf))
  expect_false(flex_is_whole(numeric(0)))
  expect_false(flex_is_whole("1"))
  expect_false(flex_is_whole(TRUE))
})
