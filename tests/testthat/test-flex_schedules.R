n <- c(10, 15, 20, 25, 30, 35, 40)
fe1 <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4)

test_that("flex_schedules returns valid schedules", {
  sch <- flex_schedules(fe1, n_sim = 200, seed = 1)
  expect_length(sch, 200)
  expect_true(all(vapply(sch, is.integer, logical(1))))
  expect_true(all(vapply(sch, function(s) all(diff(s) > 0) && s[1] >= 1,
                         logical(1))))
  last <- vapply(sch, function(s) s[length(s)], integer(1))
  expect_true(all(last >= 36 & last <= 44))
  first <- vapply(sch, function(s) s[1], integer(1))
  expect_true(all(first >= 8 & first <= 12))
  expect_true(length(unique(last)) > 5)
})

test_that("flex_schedules is reproducible with a seed", {
  expect_identical(flex_schedules(fe1, 20, seed = 3),
                   flex_schedules(fe1, 20, seed = 3))
})

test_that("flex_schedules without shifts returns the plan", {
  sch <- flex_schedules(fe1, n_sim = 3, interim_shift = 0, final_shift = 0)
  for (s in sch) expect_identical(s, fe1$n)
})

test_that("flex_schedules checks its arguments", {
  expect_error(flex_schedules(fe1, n_sim = 0), "'n_sim'")
  expect_error(flex_schedules(fe1, interim_shift = -1), "'interim_shift'")
  expect_error(flex_schedules(fe1, final_shift = 1.5), "'final_shift'")
  expect_error(flex_schedules(list()), "'design'")
})
