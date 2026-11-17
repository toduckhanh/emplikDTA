test_that("two-class deviance includes endpoints outside the observed overlap", {
  x <- c(-3, -2, -1)
  y <- c(1, 2, 3, 4)
  obj <- empi_llike_prepare(x, y, tau = 0)

  expect_equal(empi_llike_2C(obj, spe = c(1, 0.7, 0), sen = 1),
               c(0, -2 * length(x) * log(0.7), Inf))
  expect_equal(empi_llike_2C(obj, spe = 1, sen = c(1, 0.8, 0)),
               c(0, -2 * length(y) * log(0.8), Inf))
  expect_equal(empi_llike_proj(x, y, 3, 4, 0.7, 0.8, tau = 0),
               -2 * (3 * log(0.7) + 4 * log(0.8)))
  expect_equal(empi_llike_2C(empi_llike_prepare(x, y, tau = -4), 0, 1), 0)
  expect_equal(empi_llike_2C(empi_llike_prepare(x, y, tau = 5), 1, 0), 0)

  roc <- ROC_emp_curve(x, y, plot = FALSE)
  p <- plot(roc) + geom_confidence_region(roc, cpt = 0)
  expect_gt(nrow(ggplot2::ggplot_build(p)$data[[3L]]), 0)
})

test_that("threshold refitting gives a finite deviance with separated groups", {
  x <- c(-3, -2, -1)
  y <- c(1, 2, 3, 4)
  fit <- threshold2(x, y, method = "all", kernel_type = "epan",
                    bwd = c(0.25, 0.25), B = 10, seed = 91)

  for (component in fit$results) {
    expect_equal(component$spe_sen_emp, c(sp = 1, se = 1))
    expect_true(all(is.finite(component$out_bts)))
    expect_equal(component$c_alp, 0)
    expect_equal(tail(component$grid_sen_spe$z, 1), 0)
    expect_length(component$ci_tau, 2)
  }
})

test_that("fixed-threshold assessment uses half counts only to generate the bootstrap", {
  x <- c(-3, -2, -1)
  y <- c(1, 2, 3, 4)
  assessment <- list(x = 10:14, y = 20:26)
  B <- 50L
  seed <- 91L
  set.seed(seed)
  draws <- matrix(runif(2 * B), ncol = 2)
  n <- lengths(assessment)
  ref <- c(0.5 / (n[1L] + 1), (n[2L] + 0.5) / (n[2L] + 1))
  ll <- rowSums(vapply(seq_along(n), function(j) {
    k <- qbinom(draws[, j], n[j], ref[j])
    2 * (dbinom(k, n[j], k / n[j], log = TRUE) -
           dbinom(k, n[j], ref[j], log = TRUE))
  }, numeric(B)))

  fit <- threshold2(x, y, method = "all", bwd = c(0.25, 0.25),
                    assessment = assessment, B = B, seed = seed)
  expect_false(any(grepl("^ci_tau", names(fit$summary))))
  for (component in fit$results) {
    expect_equal(component$spe_sen_emp, c(sp = 0, se = 1))
    expect_null(component$ci_tau)
    expect_equal(unname(component$out_bts["tau", ]), rep(unname(component$opt_thresh), B))
    expect_equal(unname(component$out_bts["ll", ]), ll)
    expect_equal(component$c_alp, unname(quantile(ll, 0.95)))
    grid <- component$grid_sen_spe
    expect_equal(grid$z[grid$Spe == 0 & grid$Sen == 1], 0)
  }

  data <- data.frame(marker = c(x, y),
                     group = factor(rep(c("negative", "positive"), c(length(x), length(y)))))
  via_formula <- threshold2(marker ~ group, data = data, bwd = c(0.25, 0.25),
                            assessment = assessment, B = B, seed = seed)
  expect_equal(via_formula$out_bts, fit$results$YI$out_bts)
  expect_gt(nrow(ggplot2::ggplot_build(plot(via_formula))$data[[2L]]), 0)
})
