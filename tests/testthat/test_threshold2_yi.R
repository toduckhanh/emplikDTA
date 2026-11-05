test_that("Youden optimisation finds a higher peak away from the pooled means", {
  negative <- c(rep(-8, 8), rep(2, 4), rep(12, 4))
  positive <- c(rep(-4, 8), rep(5, 4), rep(13, 4))

  for (kernel in c("gauss", "epan")) {
    for (scale in c(1, 1000)) {
      fit <- threshold2(negative * scale, positive * scale, method = "YI",
                        kernel_type = kernel, bwd = rep(0.4 * scale, 2),
                        ci = FALSE)

      expect_lt(fit$opt_thresh / scale, -4)
      expect_gt(fit$spe_opt + fit$sen_opt - 1, 0.49)
    }
  }
})
