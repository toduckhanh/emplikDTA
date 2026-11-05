test_that("auc_work_default", {
  mu_true <- c(0, 1)
  sigma_true <- c(1, 1.1)
  n1 <- 15
  n2 <- 15
  X1 <- rnorm(n1, mu_true[1], sigma_true[1])
  X2 <- rnorm(n2, mu_true[2], sigma_true[2])
  expect_s3_class(
    object = auc(x = X1, y = X2),
    class = "auc")
})

test_that("auc_work_formula", {
  mu_true <- c(0, 1)
  sigma_true <- c(1, 1.1)
  n1 <- 15
  n2 <- 15
  X1 <- rnorm(n1, mu_true[1], sigma_true[1])
  X2 <- rnorm(n2, mu_true[2], sigma_true[2])
  data_test <- data.frame(biom = c(X1, X2), 
                          diag = rep(c("1", "2"), c(n1, n2)))
  expect_s3_class(
    object = auc(formula = biom ~ diag, data = data_test),
    class = "auc")
})

test_that("auc_work_formula_ci", {
  mu_true <- c(0, 1)
  sigma_true <- c(1, 1.1)
  n1 <- 15
  n2 <- 15
  X1 <- rnorm(n1, mu_true[1], sigma_true[1])
  X2 <- rnorm(n2, mu_true[2], sigma_true[2])
  data_test <- data.frame(biom = c(X1, X2), 
                          diag = rep(c("1", "2"), c(n1, n2)))
  expect_s3_class(
    object = auc(formula = biom ~ diag, data = data_test, ci_level = 0.9),
    class = "auc")
})

test_that("auc_work_formula_levl", {
  mu_true <- c(0, 1)
  sigma_true <- c(1, 1.1)
  n1 <- 15
  n2 <- 15
  X1 <- rnorm(n1, mu_true[1], sigma_true[1])
  X2 <- rnorm(n2, mu_true[2], sigma_true[2])
  data_test <- data.frame(biom = c(X1, X2), 
                          diag = rep(c("1", "2"), c(n1, n2)))
  expect_s3_class(
    object = auc(formula = biom ~ diag, data = data_test, 
                 diag_levels = c("2", "1")),
    class = "auc")
})

test_that("AUC p-values use the upper direction on the AUC scale", {
  x <- qnorm(seq(0.02, 0.98, length.out = 30))
  y <- x + 0.5
  above <- auc(x, y, B = 100, seed = 34)
  below <- auc(y, x, B = 100, seed = 34)
  equal <- auc(x, x, B = 100, seed = 34)
  expect_gt(above$estimate, 0.5)
  expect_lt(below$estimate, 0.5)
  expect_identical(equal$estimate, 0.5)

  above_tail <- pchisq(above$r.adj * above$ll.value, 1, lower.tail = FALSE)
  below_tail <- pchisq(below$r.adj * below$ll.value, 1, lower.tail = FALSE)
  expect_equal(above$p.value / above_tail, 0.5)
  expect_equal(below$p.value, 1 - below_tail / 2)
  expect_identical(equal$p.value, 0.5)
  expect_identical(above$alternative, "greater")

  less <- auc(y, x, alternative = "less", B = 100, seed = 34)
  expect_equal(less$p.value / below_tail, 0.5)
  expect_identical(less$alternative, "less")

  groups <- c("low", "high")
  data <- data.frame(score = c(x, y),
                     group = factor(rep(groups, each = length(x)), levels = groups))
  two_sided <- auc(score ~ group, data = data, diag_levels = groups,
                   alternative = "two.sided", B = 100, seed = 34)
  expect_equal(two_sided$p.value / above_tail, 1)
  expect_identical(summary(two_sided)$alternative, "two.sided")
  expect_output(print(two_sided), "p-value (two.sided)", fixed = TRUE)

  at_null <- auc(x, y, auc0 = above$estimate, alternative = "two.sided",
                 B = 100, seed = 34)
  expect_identical(at_null$p.value, 1)
  expect_identical(at_null$conf.int, above$conf.int)
  expect_error(auc(x, y, alternative = "invalid"), "arg")
})
