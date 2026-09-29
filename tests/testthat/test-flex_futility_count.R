test_that("flex_futility_count returns the largest count below the cutoff", {
  post <- c(0.01, 0.2, 0.5, 0.9, 1)
  expect_identical(flex_futility_count(post, 0.3), 1L)
  expect_identical(flex_futility_count(post, c(0.005, 0.2, 0.95, 2)),
                   c(-1L, 0L, 3L, 4L))
})

test_that("flex_futility_count uses a strict inequality", {
  post <- c(0.1, 0.5, 0.9)
  expect_identical(flex_futility_count(post, 0.5), 0L)
})
