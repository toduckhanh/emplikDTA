threshold3_test_data <- function() {
  list(x = qnorm(seq(0.01, 0.99, length.out = 40)),
       y = qnorm(seq(0.01, 0.99, length.out = 50)) + 0.6,
       z = qnorm(seq(0.01, 0.99, length.out = 60)) + 1.2)
}

test_that("all criteria match individual fits for both kernels", {
  dat <- threshold3_test_data()
  methods <- c("GYI", "CtP", "MV")
  for (kernel in c("gauss", "epan")) {
    args <- c(dat, list(kernel_type = kernel, ci = FALSE))
    fit <- do.call(threshold3, c(args, list(method = "all")))
    expect_s3_class(fit, "threshold3_all")
    expect_named(fit$results, methods)
    expect_identical(fit$summary$method, methods)
    expect_equal(fit$n, c(n1 = 40, n2 = 50, n3 = 60))
    expect_equal(fit$bwd, vapply(dat, bwd_plugin, numeric(1)),
                 ignore_attr = TRUE)
    expect_false(any(grepl("^ci_", names(fit$summary))))
    for (i in seq_along(methods)) {
      single <- do.call(threshold3, c(args, list(method = methods[i])))
      component <- fit$results[[i]]
      expect_s3_class(component, "threshold3")
      fields <- setdiff(names(single), "call")
      expect_equal(component[fields], single[fields])
      expect_null(component$out_bts)
      expect_equal(unlist(fit$summary[i, c("tau1", "tau2")],
                           use.names = FALSE), single$opt_thresh)
      tcf_fields <- c("tcf1_opt", "tcf2_opt", "tcf3_opt")
      expect_equal(unlist(fit$summary[i, tcf_fields], use.names = FALSE),
                   unlist(single[tcf_fields], use.names = FALSE))
    }
    expect_equal(do.call(threshold3, args)$opt_thresh,
                 fit$results$GYI$opt_thresh)
  }
})

test_that("all criteria share reproducible bootstrap inference", {
  dat <- threshold3_test_data()
  args <- c(dat, list(bwd = c(0.35, 0.4, 0.45), ci = TRUE,
                     ci_level = 0.9, B = 7, seed = 81))
  fit <- do.call(threshold3, c(args, list(method = "all")))
  rng_after_all <- .Random.seed
  for (i in seq_along(fit$results)) {
    single <- do.call(threshold3,
                      c(args, list(method = names(fit$results)[i])))
    expect_identical(.Random.seed, rng_after_all)
    component <- fit$results[[i]]
    fields <- setdiff(names(single), "call")
    expect_equal(component[fields], single[fields])
    expect_equal(dim(component$out_bts), c(3L, 7L))
    expect_equal(dim(component$ci_tau), c(2L, 2L))
    expect_true(all(is.finite(component$out_bts[1:2, ])))
    expect_false(anyNA(component$out_bts[3, ]))
    expect_equal(component$c_alp,
                 unname(quantile(component$out_bts[3, ], 0.9)))
    expect_equal(as.numeric(fit$summary[i, c("ci_tau1_lower", "ci_tau1_upper")]),
                 unname(component$ci_tau[1, ]))
    expect_equal(as.numeric(fit$summary[i, c("ci_tau2_lower", "ci_tau2_upper")]),
                 unname(component$ci_tau[2, ]))
  }
  args$seed <- NULL
  args$B <- 1
  default_seed <- do.call(threshold3, c(args, list(method = "all")))
  explicit_seed <- do.call(threshold3,
                           c(args, list(method = "all", seed = 34)))
  for (m in names(fit$results)) {
    expect_equal(dim(default_seed$results[[m]]$out_bts), c(3L, 1L))
    expect_equal(default_seed$results[[m]]$out_bts,
                 explicit_seed$results[[m]]$out_bts)
  }
})

test_that("formula all-method fits retain metadata in every result", {
  dat <- threshold3_test_data()
  groups <- c("low", "middle", "high")
  data <- data.frame(marker = unlist(dat, use.names = FALSE),
                     group = factor(rep(groups, lengths(dat)), levels = groups))
  fit <- threshold3(marker ~ group, data = data,
                    diag_levels = c("low", "middle", "high"),
                    method = "all", bwd = rep(0.4, 3), ci = FALSE)
  direct <- do.call(threshold3, c(dat, list(method = "all",
                                          bwd = rep(0.4, 3), ci = FALSE)))
  expect_equal(fit$summary, direct$summary)
  expect_equal(fit$formula, marker ~ group)
  expect_equal(fit$group.levels, c("low", "middle", "high"))
  for (component in fit$results) {
    expect_equal(component$formula, fit$formula)
    expect_identical(component$group.levels, fit$group.levels)
    expect_identical(component$call, fit$call)
  }
})

test_that("CtP and MV optimise ordered thresholds on the original and bootstrap samples", {
  # Scenario 6, RT bandwidths. Both legacy optimisers reversed this pair.
  set.seed(130356L)
  x <- rt(20L, 2)
  y <- rbeta(20L, 2, 2)
  z <- rchisq(20L, 1)
  bounds <- rbind(c(min(x, y), min(y, z)), c(max(x, y), max(y, z)))
  bwd <- vapply(list(x, y, z), bwd_plugin, numeric(1))
  grid <- sort(unique(c(seq(min(bounds), max(bounds), length.out = 301L), bounds)))
  a <- grid[grid >= bounds[1L, 1L] & grid <= bounds[2L, 1L]]
  b <- grid[grid >= bounds[1L, 2L] & grid <= bounds[2L, 2L]]
  feasible <- outer(a, b, "<=")

  for (kernel in c("gauss", "epan")) {
    fit <- threshold3(x, y, z, method = "all", kernel_type = kernel,
                      bwd = bwd, B = 25L, seed = 81L)
    F1 <- cdf_kernel(a, x, kernel, bwd[1L])
    F2 <- outer(cdf_kernel(a, y, kernel, bwd[2L]),
                cdf_kernel(b, y, kernel, bwd[2L]), function(u, v) v - u)
    tcf3 <- rep(1 - cdf_kernel(b, z, kernel, bwd[3L]), each = length(a))
    grid_ctp <- (1 - F1)^2 + (1 - F2)^2 + (1 - tcf3)^2
    grid_mv <- F1 * F2 * tcf3
    tcf <- as.matrix(fit$summary[, c("tcf1_opt", "tcf2_opt", "tcf3_opt")])
    expect_lte(sum((1 - tcf[2L, ])^2), min(grid_ctp[feasible]) + 1e-6)
    expect_gte(prod(tcf[3L, ]), max(grid_mv[feasible]) - 1e-6)

    for (method in c("CtP", "MV")) {
      component <- fit$results[[method]]
      pairs <- cbind(component$opt_thresh, component$out_bts[1:2, ])
      expect_true(all(pairs[1L, ] <= pairs[2L, ]))
      expect_true(all(pairs >= bounds[1L, ] & pairs <= bounds[2L, ]))
      expect_identical(component$coincident_thresholds_bts,
                       pairs[1L, -1L] == pairs[2L, -1L])
    }
  }
})

test_that("CtP and MV can return coincident thresholds when the search bounds collapse", {
  x <- rep(1, 4)
  fit <- threshold3(x, x, x, method = "all", bwd = rep(1, 3), B = 3)
  for (method in c("CtP", "MV")) {
    component <- fit$results[[method]]
    expect_equal(component$opt_thresh, c(1, 1))
    expect_true(component$coincident_thresholds)
    expect_true(all(component$coincident_thresholds_bts))
    expect_equal(component$tcf2_opt, 0)
    expect_true(is.finite(component$c_alp))
  }
})
