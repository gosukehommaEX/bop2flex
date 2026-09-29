test_that("flex_oc_engine agrees with a direct two-stage calculation", {
  # Stage 1: 3 patients, stop for futility if S <= 0, for efficacy if S >= 3.
  # Stage 2: 2 more patients, reject if S >= 3.
  p <- 0.4
  s1 <- stats::dbinom(0:3, 3, p)
  cont <- s1[2] * stats::pbinom(1, 2, p, lower.tail = FALSE) +
    s1[3] * stats::pbinom(0, 2, p, lower.tail = FALSE)
  reject <- s1[4] + cont
  en <- 3 * (s1[1] + s1[4]) + 5 * (s1[2] + s1[3])
  o <- flex_oc_engine(c(3L, 5L), matrix(c(0L, 2L), 1), matrix(c(3L, 3L), 1),
                      p, TRUE)
  expect_equal(o$reject, reject, tolerance = 1e-14)
  expect_equal(o$early_efficacy, s1[4], tolerance = 1e-14)
  expect_equal(o$early_futility, s1[1], tolerance = 1e-14)
  expect_equal(o$en, en, tolerance = 1e-14)
})

test_that("flex_oc_engine ignores futility when non-binding", {
  p <- 0.4
  s1 <- stats::dbinom(0:3, 3, p)
  reject <- s1[4] + s1[1] * stats::pbinom(2, 2, p, lower.tail = FALSE) +
    s1[2] * stats::pbinom(1, 2, p, lower.tail = FALSE) +
    s1[3] * stats::pbinom(0, 2, p, lower.tail = FALSE)
  o <- flex_oc_engine(c(3L, 5L), matrix(c(0L, 2L), 1), matrix(c(3L, 3L), 1),
                      p, FALSE)
  expect_equal(o$reject, reject, tolerance = 1e-14)
  expect_equal(o$early_futility, 0)
})

test_that("flex_oc_engine evaluates several designs at once", {
  n <- c(10L, 15L, 20L, 25L, 30L, 35L, 40L)
  fut <- rbind(c(1L, 2L, 3L, 5L, 7L, 8L, 11L), c(1L, 2L, 4L, 5L, 7L, 9L, 10L))
  eff <- rbind(c(6L, 8L, 9L, 9L, 10L, 11L, 12L),
               c(11L, 16L, 21L, 26L, 31L, 36L, 11L))
  o <- flex_oc_engine(n, fut, eff, 0.2, TRUE)
  # Independent Python values
  expect_equal(o$reject, c(0.098818200534, 0.097428419140), tolerance = 1e-10)
  expect_equal(o$en, c(20.336082438939, 20.292654305449), tolerance = 1e-10)
  expect_equal(rowSums(o$efficacy_by_look), o$reject, tolerance = 1e-14)
  # Probabilities of all outcomes sum to one
  final_accept <- 1 - o$reject - o$early_futility
  expect_true(all(final_accept >= 0))
})
