####========================================================================####
## This file contains functions for estimating the empirical likelihood ratio ##
## for three TCFs at given pair of thresholds                                 ##
####========================================================================####

#' @import stats
#' @import utils

## ---- bootstrap procedure to compute w for EL for TCF2 ----
bts_func_3C <- function(x, y, z, n1, n2, n3, tcf1, tcf2, tcf3,
                        enlarged = TRUE, B, type_F) {
  empi_bts <- sapply(1:B, function(i){
    # flag <- 0
    # while(flag == 0){
      x.b <- sample(x, n1, replace = TRUE)
      y.b <- sample(y, n2, replace = TRUE)
      z.b <- sample(z, n3, replace = TRUE)
      if (enlarged) {
        x.b <- c(x.b, range(x))
        y.b <- c(y.b, range(y))
        z.b <- c(z.b, range(z))
        n1 <- n1 + 2
        n2 <- n2 + 2
        n3 <- n3 + 2
      }
      # flag <- as.numeric((mean(x.b) < mean(y.b)) * (mean(y.b) < mean(z.b)))
    # }
    tau1_est <- quantile(x.b, tcf1, names = FALSE, type = 8)
    tau2_est <- quantile(z.b, 1 - tcf3, names = FALSE, type = 8)
    if (tau1_est > tau2_est) {
      temp <- tau2_est
      tau2_est <- tau1_est
      tau1_est <- temp
    }
    res <- empi_llike_3C(x = x.b, y = y.b, z = z.b, n1 = n1, n2 = n2, n3 = n3, 
                         tcf1 = tcf1, tcf2 = tcf2, tcf3 = tcf3, 
                         tau = c(tau1_est, tau2_est), type_F = type_F)
    return(res)
  })
  empi_bts[is.na(empi_bts)] <- Inf
  return(empi_bts)
}

ll_tcf2_adj <- function(theta, x, y, z, tcf1, tcf3, tau_est, r_adj, qc, n) {
  ll_est <- empi_llike_3C(x = x, y = y, z = z, n1 = n[1], n2 = n[2],
                          n3 = n[3], tcf1 = tcf1, tcf2 = theta, tcf3 = tcf3,
                          tau = tau_est, type_F = "empi")
  if(is.na(ll_est)) ll_est <- Inf
  ll_est_adj <- ll_est
  if(!is.infinite(ll_est)){
    ll_est_adj <- r_adj * ll_est_adj
  }
  return(ll_est_adj - qc)
}

.safe_uniroot_tcf2 <- function(interval, ...) {
  f.lower <- ll_tcf2_adj(interval[1], ...)
  f.upper <- ll_tcf2_adj(interval[2], ...)
  if (is.nan(f.lower) || is.nan(f.upper)) return(NA_real_)
  if (f.lower * f.upper > 0) return(NA_real_)
  out <- uniroot(ll_tcf2_adj, interval = interval, ...)$root
  return(out)
}

# function for plotting the likelihood ratio and confidence interval
plot_tcf2 <- function(tcf2_est, r_est, ci_level, n, ci, 
                       x, y, z, tcf1, tcf3, tau_est, qc) {
  xgrid <- seq(0, 1, by = 0.001)
  ll <- sapply(xgrid, function(t) {
    ll_tcf2_adj(theta = t, x = x, y = y, z = z, tcf1 = tcf1, tcf3 = tcf3, 
                tau_est = tau_est, r_adj = r_est, qc = qc, n = n)
  })
  ll <- ll + qc
  df <- data.frame(tcf2 = xgrid, elr = exp(-0.5 * ll))
  df$inside_ci <- df$tcf2 >= ci[1] & df$tcf2 <= ci[2]
  cutoff <- exp(-0.5 * qc)
  p <- ggplot(data = df, mapping = aes(x = tcf2, y = elr)) +
    geom_line(linewidth = 0.75) +
    geom_ribbon(data = df[df$inside_ci, ], mapping = aes(ymin = 0, ymax = elr),
                alpha = 0.2) +
    geom_vline(xintercept = ci, linetype = "dashed") +
    geom_vline(xintercept = tcf2_est, color = "blue", linewidth = 0.75) +
    geom_hline(yintercept = cutoff, linetype = "dotted") +
    labs(x = "TCF2", y = "Empirical likelihood ratio") +
    theme_bw()
  return(p)
}


#'@export
tcf2 <- function(x, ...) {
  UseMethod("tcf2")
}


#' @exportS3Method
tcf2.default <- function(x, y, z, tcf10, tcf30, tcf20 = NULL,
                         ci_level = 0.95, B = 500, seed, plot = FALSE) {
  call <- match.call()
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_numeric(z, "z")
  .check_ci(ci_level)
  n1 <- length(x)
  n2 <- length(y)
  n3 <- length(z)
  tau1_est <- quantile(x, probs = tcf10, names = FALSE, type = 8)
  tau2_est <- quantile(z, probs = 1 - tcf30, names = FALSE, type = 8)
  tcf2_emp <- mean(y <= tau2_est) - mean(y <= tau1_est)
  out <- list(estimate = tcf2_emp, tau_est = c(tau1_est, tau2_est),
              tcf1 = tcf10, tcf3 = tcf30,
              n = c(n1 = n1, n2 = n2, n3 = n3), call = call)
  if (tcf2_emp == 1) {
    out$estimate <- tcf2_emp / (1 + 0.5 / n2)
    class(out) <- "tcf2"
    return(out)
  }
  if (tcf2_emp == 0) {
    out$estimate <- 0
    class(out) <- "tcf2"
    return(out)
  }
  if(missing(seed)) seed <- 34
  set.seed(seed)
  r_bts <- bts_func_3C(x = x, y = y, z = z, n1 = n1, n2 = n2, n3 = n3,
                       tcf1 = tcf10, tcf2 = tcf2_emp, tcf3 = tcf30,
                       enlarged = TRUE, B = B, type_F = "Adi_ties")
  r_est <- qchisq(0.5, 1) / median(r_bts)
  ##
  qc <- qchisq(ci_level, 1)
  eps <- .Machine$double.eps^0.5
  LI <- .safe_uniroot_tcf2(interval = c(eps, tcf2_emp), x = x, y = y, z = z, 
                           tcf1 = tcf10, tcf3 = tcf30, 
                           tau_est = c(tau1_est, tau2_est), r_adj = r_est, 
                           qc = qc, n = c(n1, n2, n3))
  UI <- .safe_uniroot_tcf2(interval = c(tcf2_emp, 1 - eps), x = x, y = y, z = z, 
                           tcf1 = tcf10, tcf3 = tcf30, 
                           tau_est = c(tau1_est, tau2_est), r_adj = r_est, 
                           qc = qc, n = c(n1, n2, n3))
  ci_tcf2 <- c(LI, UI)
  if (plot) {
    pl <- plot_tcf2(tcf2_est = tcf2_emp, r_est = r_est, ci_level = ci_level,
                    n = c(n1, n2, n3), ci = ci_tcf2, x = x, y = y, z = z, 
                    tcf1 = tcf10, tcf3 = tcf30, 
                    tau_est = c(tau1_est, tau2_est), qc = qc)
    print(pl)
  }
  ##
  out$conf.int <- ci_tcf2
  out$r.bts <- r_bts
  out$r.adj <- r_est
  out$ci_level <- ci_level
  class(out) <- "tcf2"
  ## p-value
  if(!is.null(tcf20)){
    ll_0 <- ll_tcf2_adj(theta = tcf20, x = x, y = y, z = z, 
                        tcf1 = tcf10, tcf3 = tcf30, 
                        tau_est = c(tau1_est, tau2_est), r_adj = r_est, 
                        qc = qc, n = c(n1, n2, n3))
    p_val <- pchisq(ll_0 + qc, df = 1, lower.tail = FALSE)
    out$p.value <- p_val
    out$ll.value <- ll_0
  }
  return(out)
}

#' @exportS3Method
tcf2.formula <- function(formula, data, diag_levels = NULL, subset, 
                         na.action, tcf10, tcf30, tcf20 = NULL, ...) {
  call <- match.call()
  dat <- .extract_formula_data(formula = formula, data = data,
                               diag_levels = diag_levels, subset = subset,
                               na.action = na.action)
  res <- tcf2.default(dat$split[[1]], dat$split[[2]], dat$split[[3]], 
                      tcf10 = tcf10, tcf30 = tcf30, tcf20 = tcf20, ...)
  res$formula <- formula
  res$group.levels <- dat$levl_class
  res$call <- call
  return(res)
}

#' @export
print.tcf2 <- function(x, ...) {
  cat("TCF1:\n")
  print(x$tcf1)
  cat("TCF3:\n")
  print(x$tcf3)
  cat("Thresholds estimate:\n")
  print(x$tau_est)
  cat("TCF2 estimate:\n")
  print(x$estimate)
  if (!is.null(x$conf.int)) {
    cat(paste0("\n", x$ci_level * 100, "% Confidence interval:\n"))
    print(x$conf.int)
  }
  if (!is.null(x$p.value)) {
    cat("\n p-value:", x$p.value, "\n")
  }
  invisible(x)
}

#' @export
summary.tcf2 <- function(object, ...) {
  out <- list(estimate = object$estimate, conf.int = object$conf.int,
              p.value = object$p.value, n = object$n)
  class(out) <- "summary.tcf2"
  out
}

