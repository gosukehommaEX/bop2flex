bounds <- data.frame(m = c(10L, 20L, 30L), final = c(FALSE, FALSE, TRUE),
                     futility = c(1L, 4L, 8L), efficacy = c(6L, 9L, 9L))

test_that("flex_decide reports the decision at each analysis", {
  out <- flex_decide(bounds, c(3, 6, 10))
  expect_identical(out$decision, c("continue", "continue", "go (reject H0)"))
  expect_identical(out$responses, c(3L, 6L, 10L))
  out2 <- flex_decide(bounds, c(3, 6, 8))
  expect_identical(out2$decision[3], "no-go")
})

test_that("flex_decide stops at a boundary and warns about later rows", {
  expect_warning(out <- flex_decide(bounds, c(1, 5, 7)), "NA")
  expect_identical(out$decision, c("stop for futility", NA, NA))
  expect_warning(out2 <- flex_decide(bounds, c(6, 7, 9)), "NA")
  expect_identical(out2$decision[1], "stop for efficacy")
  out3 <- flex_decide(bounds[1:2, ], c(2, 9))
  expect_identical(out3$decision, c("continue", "stop for efficacy"))
})

test_that("flex_decide rejects impossible response counts", {
  expect_error(flex_decide(bounds, c(3, 6)), "one element")
  expect_error(flex_decide(bounds, c(3, 2, 5)), "cumulative")
  expect_error(flex_decide(bounds, c(3, 14, 15)), "cumulative")
  expect_error(flex_decide(bounds, c(11, 12, 13)), "between 0")
})
