test_that("VUS p-values use the upper direction on the VUS scale", {
  x <- qnorm(seq(0.02, 0.98, length.out = 30))
  y <- qnorm(seq(0.02, 0.98, length.out = 32)) + 0.5
  z <- qnorm(seq(0.02, 0.98, length.out = 34)) + 1

  above <- vus(x, y, z, B = 100, seed = 34)
  below <- vus(z, y, x, B = 100, seed = 34)
  expect_gt(above$estimate, 1/6)
  expect_lt(below$estimate, 1/6)

  above_tail <- pchisq(above$r.adj * above$ll.value, 1, lower.tail = FALSE)
  below_tail <- pchisq(below$r.adj * below$ll.value, 1, lower.tail = FALSE)
  expect_equal(above$p.value / above_tail, 0.5)
  expect_equal(below$p.value, 1 - below_tail / 2)
  expect_gt(below$p.value, 0.5)
  expect_identical(above$alternative, "greater")

  less <- vus(z, y, x, alternative = "less", B = 100, seed = 34)
  expect_equal(less$p.value / below_tail, 0.5)
  expect_identical(less$alternative, "less")

  groups <- c("low", "middle", "high")
  data <- data.frame(score = c(x, y, z),
                     group = factor(rep(groups, c(length(x), length(y), length(z))),
                                    levels = groups))
  two_sided <- vus(score ~ group, data = data, diag_levels = groups,
                   alternative = "two.sided", B = 100, seed = 34)
  expect_equal(two_sided$p.value / above_tail, 1)
  expect_identical(two_sided$alternative, "two.sided")

  at_null <- vus(x, y, z, vus0 = above$estimate, B = 100, seed = 34)
  expect_identical(at_null$p.value, 0.5)
  expect_identical(at_null$conf.int, above$conf.int)
  expect_error(vus(x, y, z, alternative = "invalid"), "arg")
})
