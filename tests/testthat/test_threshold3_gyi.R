test_that("GYI searches beyond the peak near the initial thresholds", {
  set.seed(129001L)
  x <- rnorm(20L)
  y <- rnorm(20L, 1)
  z <- rnorm(20L, 2)
  fit <- threshold3(x, y, z, method = "GYI", ci = FALSE)

  expect_equal(fit$opt_thresh, c(0.4130254, 2.230229), tolerance = 1e-5)
  expect_gt(fit$tcf1_opt + fit$tcf2_opt + fit$tcf3_opt, 1.5898)
  expect_false(fit$coincident_thresholds)
})

test_that("GYI retains a maximum on the diagonal for both kernels", {
  for (kernel in c("gauss", "epan")) {
    fit <- threshold3(rep(0, 4), c(-4, -4, 4, 4), rep(1, 4),
                      method = "GYI", bwd = rep(1, 3), kernel_type = kernel,
                      ci = FALSE)

    expect_equal(fit$opt_thresh, c(0.5, 0.5), tolerance = 1e-6)
    expect_true(fit$coincident_thresholds)
    expect_identical(fit$tcf2_opt, 0)
  }
})

test_that("GYI applies the same ordered solver to every bootstrap sample", {
  set.seed(129014L)
  x <- rnorm(80L)
  y <- rnorm(80L, 0.5)
  z <- rnorm(80L, 1)
  bwd <- vapply(list(x, y, z), bwd_plugin, numeric(1))
  a0 <- c((mean(x) + mean(y)) / 2, (mean(y) + mean(z)) / 2)
  a1 <- c(min(x, y), min(y, z))
  a2 <- c(max(x, y), max(y, z))
  B <- 30L
  seed <- 193014L
  fit <- threshold3(x, y, z, method = "GYI", bwd = bwd, B = B, seed = seed)

  resample <- function(u) c(sample(u, length(u), replace = TRUE), min(u), max(u))
  set.seed(seed)
  expected <- replicate(B, GYI_kern(resample(x), resample(y), resample(z),
                                    bwd[1L], bwd[2L], bwd[3L], "gauss",
                                    a0, a1, a2))
  tau <- unname(fit$out_bts[1:2, , drop = FALSE])
  expect_equal(tau, expected)
  expect_true(all(tau[1L, ] <= tau[2L, ]))
  expect_true(any(tau[1L, ] == tau[2L, ]))
  expect_identical(fit$coincident_thresholds_bts, tau[1L, ] == tau[2L, ])
})

test_that("independent assessment bootstraps TCFs at the selected fixed pair", {
  x <- rep(0, 4)
  y <- c(-4, -4, 4, 4)
  z <- rep(1, 4)
  assessment <- list(x = c(2, 3), y = c(-1, 1), z = c(-3, -2, -1))
  B <- 50L
  seed <- 40L
  fit <- threshold3(x, y, z, bwd = rep(1, 3), assessment = assessment,
                    B = B, seed = seed)

  expect_true(fit$coincident_thresholds)
  expect_true(all(fit$coincident_thresholds_bts))
  expect_null(fit$ci_tau)
  expect_equal(unname(fit$out_bts[1:2, ]), matrix(fit$opt_thresh, 2, B))
  obj <- do.call(empi_llike3_prepare, c(assessment, list(tau = fit$opt_thresh)))
  set.seed(seed)
  expected <- bts_fixed_tcfs(obj, matrix(runif(3 * B), ncol = 3L))
  expect_true(all(is.finite(expected)))
  expect_equal(unname(fit$out_bts[3, ]), expected)
  expect_equal(fit$c_alp, unname(quantile(expected, 0.95)))
})
