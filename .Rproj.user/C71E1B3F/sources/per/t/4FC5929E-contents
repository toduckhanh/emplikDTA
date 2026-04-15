# bootstrap procedure for EL vus ----
bts_auc <- function(X1, X2, n1, n2, n, auc_est, B) {
  empi_bts <- sapply(1:B, function(i){
    flag <- 0
    while(flag == 0){
      X1.b <- sample(X1, n1, replace = TRUE)
      X2.b <- sample(X2, n2, replace = TRUE)
      flag <- as.numeric(mean(X1.b) < mean(X2.b))
    }
    auc_est_bts <- auc_core(X1.b, X2.b)
    if (auc_est_bts == 1) {
      auc_est_bts <- auc_est_bts/(1 + 0.5 / n1 / n2)
    }
    res <- ll_prob(theta = auc_est, theta_est = auc_est_bts, n = n)
    return(res)
  })
  return(empi_bts)
}

# function for plotting the likelihood ratio and confidence interval
.plot_auc <- function(auc_est, r_est, ci_level, n, ci) {
  xgrid <- seq(0, 1, by = 0.001)
  ll <- sapply(xgrid, function(x) {
    ll_prob_adj(
      x,
      theta_est = auc_est,
      r_adj = r_est,
      qc = qchisq(ci_level, 1),
      n = n
    ) + qchisq(ci_level, 1)
  })
  df <- data.frame(
    vus = xgrid,
    elr = exp(-0.5 * ll)
  )
  df$inside_ci <- df$vus >= ci[1] & df$vus <= ci[2]
  cutoff <- exp(-0.5 * qchisq(ci_level, 1))
  p <- ggplot(data = df, mapping = aes(x = vus, y = elr)) +
    geom_line(linewidth = 0.75) +
    geom_ribbon(
      data = df[df$inside_ci, ],
      mapping = aes(ymin = 0, ymax = elr),
      alpha = 0.2
    ) +
    geom_vline(xintercept = ci, linetype = "dashed") +
    geom_vline(
      xintercept = auc_est,
      color = "blue",
      linewidth = 0.75
    ) +
    geom_hline(
      yintercept = cutoff,
      linetype = "dotted"
    ) +
    labs(
      x = "AUC",
      y = "Empirical likelihood ratio"
    ) +
    theme_bw()
  return(p)
}

# Empirical Likelihood Inference for auc ----
#' Area Under ROC surface (AUC) estimation and Empirical Likelihood Inference for AUC
#' @param x,y Numeric vectors (default method)
#' @param formula A formula of the form y ~ group
#' @param data Data frame
#' @param theta0 Null hypothesis value
#' @param ci_level Confidence level
#' @param B Bootstrap size
#' @param seed Random seed
#' @param plot Logical; plot empirical likelihood
#' @export
auc <- function(x, ...) {
  UseMethod("auc")
}

#' @exportS3Method
auc.default <- function(x, y, auc0 = 1/2, ci_level = 0.95, B = 500, seed,
                        plot = FALSE) {
  call <- match.call()
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_ci(ci_level)
  m1 <- mean(x)
  m2 <- mean(y)
  if (m1 > m2) warning("the orders of groups may not hold")
  n1 <- length(x)
  n2 <- length(y)
  n  <- n1 + n2
  auc_est <- auc_core(x, y)
  out <- list(
    estimate = auc_est,
    n = c(n1 = n1, n2 = n2),
    call = call
  )
  if (auc_est == 1) {
    out$estimate <- auc_est / (1 + 0.5 / n1 / n2)
    class(out) <- "auc"
    return(out)
  }
  if (missing(seed)) seed <- 34
  set.seed(seed)
  r_bts <- bts_auc(X1 = x, X2 = y, n1 = n1, n2 = n2, n = n, auc_est = auc_est,
                   B = B)
  r_est <- qchisq(0.5, 1) / median(r_bts)
  qc <- qchisq(ci_level, 1)
  LI <- uniroot(f = ll_prob_adj, interval = c(0, auc_est), 
                theta_est = auc_est, qc = qc, r_adj = r_est, n = n)$root
  UI <- uniroot(f = ll_prob_adj, interval = c(auc_est, 1), 
                theta_est = auc_est, qc = qc, r_adj = r_est, n = n)$root
  ci <- c(LI, UI)
  if (plot) {
    pl <- .plot_auc(auc_est, r_est, ci_level, n, ci)
    print(pl)
  }
  ## p-value
  ll_0 <- ll_prob(theta = auc0, theta_est = auc_est, n = n)
  p_val <- pchisq(r_est * ll_0, df = 1, lower.tail = FALSE)
  ##
  out$conf.int <- ci
  out$p.value <- p_val
  out$r.adj <- r_est
  out$ci_level <- ci_level
  class(out) <- "auc"
  return(out)
}

#' @exportS3Method
auc.formula <- function(formula, data, diag_levels = NULL, subset, 
                        na.action, ...) {
  call <- match.call()
  if (missing(data)) {
    stop("'data' must be provided for formula method", call. = FALSE)
  }
  mf <- match.call(expand.dots = FALSE)
  mf$diag_levels <- NULL
  mf$... <- NULL
  mf[[1]] <- quote(model.frame)
  mf <- eval(mf, parent.frame())
  response <- model.response(mf)
  group <- mf[[2]]
  if (!is.factor(group)) {
    group <- factor(group)
  }
  if (nlevels(group) != 2) {
    stop("diagnostic group must have exactly 2 levels", call. = FALSE)
  }
  mean_temp <- aggregate(formula, FUN = mean, data = data)
  temp_levl <- mean_temp[order(mean_temp[, 2]), 1]
  out_check_levl <- .check_levl_class(trace = TRUE, diag_levels, temp_levl,
                                      n_class = 2)
  levl_class <- out_check_levl$levl_class
  x <- response[group == levl_class[1]]
  y <- response[group == levl_class[2]]
  res <- auc.default(x, y, ...)
  res$formula <- formula
  res$group.levels <- levl_class
  res$call <- call
  return(res)
}

#' @export
print.auc <- function(x, ...) {
  cat("AUC estimate:\n")
  print(x$estimate)
  if (!is.null(x$conf.int)) {
    cat(paste0("\n", x$ci_level * 100, "% Confidence interval:\n"))
    print(x$conf.int)
  }
  if (!is.null(x$p.value)) {
    cat("\nP-value:", x$p.value, "\n")
  }
  invisible(x)
}

#' @export
summary.auc <- function(object, ...) {
  out <- list(
    estimate = object$estimate,
    conf.int = object$conf.int,
    p.value = object$p.value,
    n = object$n
  )
  class(out) <- "summary.auc"
  out
}
