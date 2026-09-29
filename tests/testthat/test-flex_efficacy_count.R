test_that("flex_efficacy_count returns the smallest count at the cutoff", {
  post <- c(0.01, 0.2, 0.5, 0.9, 1)
  expect_identical(flex_efficacy_count(post, 0.3), 2L)
  expect_identical(flex_efficacy_count(post, c(0, 0.2, 0.95, 1.5)),
                   c(0L, 1L, 4L, 5L))
})

test_that("flex_efficacy_count treats NA as no efficacy stopping", {
  expect_identical(flex_efficacy_count(c(0.1, 0.5, 0.9), NA_real_), 3L)
})

test_that("flex_efficacy_count uses a non-strict inequality", {
  expect_identical(flex_efficacy_count(c(0.1, 0.5, 0.9), 0.5), 1L)
})
