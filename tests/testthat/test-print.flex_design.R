n <- c(10, 15, 20, 25, 30, 35, 40)

test_that("print.flex_design describes the design", {
  d <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.87, gamma = 0.94,
                 eta = 2.4, p1 = 0.4)
  expect_output(print(d), "BOP2-FE design \\(power efficacy cutoff\\)")
  expect_output(print(d), "lambda = 0.87, gamma = 0.94, eta = 2.4")
  expect_output(print(d), "binding futility")
  capture_output(res <- withVisible(print(d)))
  expect_false(res$visible)
  expect_identical(res$value, d)
  b <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.84, gamma = 0.64,
                 type = "BOP2")
  expect_output(print(b), "BOP2 design \\(futility stopping only\\)")
  expect_output(print(b), "p1 = NA")
  of <- flex_plan(p0 = 0.2, n = n, alpha = 0.1, lambda = 0.88, gamma = 0.95,
                  method = "OF")
  expect_output(print(of), "O'Brien-Fleming")
  tab <- flex_plan(p0 = 0.2, n = n, alpha = 0.1,
                   futility = c(1, 2, 3, 5, 7, 8),
                   efficacy = c(6, 8, 9, 9, 10, 11, 12), binding = TRUE)
  expect_output(print(tab), "Specified by: table")
})
