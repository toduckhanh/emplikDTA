test_that("VUS formula and vector methods agree for the same class order", {
  set.seed(34)
  x <- rnorm(15, 0, 1)
  y <- rnorm(15, 2.5, 1.1)
  z <- rnorm(15, 3.69, 1.2)
  data <- data.frame(marker = c(x, y, z), group = rep(1:3, each = 15))

  vector_fit <- vus(x, y, z, ci_level = 0.9, B = 50)
  formula_fit <- vus(marker ~ group, data = data, ci_level = 0.9, B = 50)

  expect_s3_class(formula_fit, "vus")
  expect_equal(formula_fit$estimate, vector_fit$estimate)
  expect_equal(formula_fit$conf.int, vector_fit$conf.int)
  expect_identical(formula_fit$ci_level, 0.9)
})
