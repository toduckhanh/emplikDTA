# bootstrap procedure for EL vus ----
bts_vus <- function(X1, X2, X3, n1, n2, n3, n, vus_est, B) {
  empi_bts <- sapply(1:B, function(i){
    flag <- 0
    while(flag == 0){
      X1.b <- sample(X1, n1, replace = TRUE)
      X2.b <- sample(X2, n2, replace = TRUE)
      X3.b <- sample(X3, n3, replace = TRUE)
      flag <- as.numeric((mean(X1.b) < mean(X2.b)) * (mean(X2.b) < mean(X3.b)))
    }
    vus_est_bts <- vus_core(X1.b, X2.b, X3.b)
    if (vus_est_bts == 1) {
      vus_est_bts <- vus_est_bts/(1 + 0.5 / n1 / n2 / n3)
    }
    res <- ll_prob(theta = vus_est, theta_est = vus_est_bts, n = n)
    return(res)
  })
  return(empi_bts)
}

ll_vus_fun <- function(theta, theta_est, r_adj, qc, n) {
  ll_est <- ll_prob(theta, theta_est, n)
  if (is.na(ll_est)) ll_est <- Inf
  ll_est_adj <- ll_est
  if (!is.infinite(ll_est)){
    ll_est_adj <- r_adj * ll_est_adj
  }
  return(ll_est_adj - qc)
}

.plot_vus <- function(vus_est, r_est, ci_level, n, ci) {
  xgrid <- seq(0, 1, by = 0.001)
  
  ll <- sapply(xgrid, function(x) {
    ll_vus_fun(x,
               theta_est = vus_est,
               r_adj = r_est,
               qc = qchisq(ci_level, 1),
               n = n) + qchisq(ci_level, 1)
  })
  
  plot(xgrid, exp(-0.5 * ll), type = "l",
       xlab = "VUS", ylab = "Empirical likelihood ratio")
  
  abline(v = ci, lty = 2)
  abline(v = vus_est, col = "blue")
}


# Empirical Likelihood Inference for vus ----
#' Volume Under Surface (VUS) estimation and Empirical Likelihood Inference for VUS
#' @param x,y,z Numeric vectors (default method)
#' @param formula A formula of the form y ~ group
#' @param data Data frame
#' @param theta0 Null hypothesis value
#' @param ci_level Confidence level
#' @param B Bootstrap size
#' @param seed Random seed
#' @param plot Logical; plot empirical likelihood
#' @export
vus <- function(x, ...) {
  UseMethod("vus")
}

#' @exportS3Method
vus.default <- function(x, y, z, theta0 = 1/6, ci_level = 0.95, B = 500, seed,
                        plot = FALSE) {
  call <- match.call()
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_numeric(z, "z")
  .check_ci(ci_level)
  n1 <- length(x)
  n2 <- length(y)
  n3 <- length(z)
  n  <- n1 + n2 + n3
  vus_est <- vus_core(x, y, z)
  out <- list(
    estimate = vus_est,
    n = c(n1 = n1, n2 = n2, n3 = n3),
    call = call
  )
  if (vus_est == 1) {
    out$estimate <- vus_est / (1 + 0.5 / n1 / n2 / n3)
    class(out) <- "vus"
    return(out)
  }
  if (missing(seed)) seed <- 34
  set.seed(seed)
  r_est <- bts_vus(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3, 
                   n = n, vus_est = vus_est, B = B)
  qc <- qchisq(ci_level, 1)
  LI <- uniroot(f = ll_vus_fun, interval = c(0, vus_est), 
                theta_est = vus_est, qc = qc, r_adj = r_est, n = n)$root
  UI <- uniroot(f = ll_vus_fun, interval = c(vus_est, 1), 
                theta_est = vus_est, qc = qc, r_adj = r_est, n = n)$root
  ci <- c(LI, UI)
  if (plot) {
    .plot_vus(vus_est, r_est, ci_level, n, ci)
  }
  ## p-value
  ll_0 <- ll_prob(theta = theta0, theta_est = vus_est, n = n)
  p_val <- pchisq(r_est * ll_0, df = 1, lower.tail = FALSE)
  ##
  out$conf.int <- ci
  out$p.value  <- p_val
  out$r.adj    <- r_est
  class(out) <- "vus"
  return(out)
}

#' @exportS3Method
vus.formula <- function(formula, data, subset, na.action, ...) {
  call <- match.call()
  if (missing(data)) {
    stop("'data' must be provided for formula method", call. = FALSE)
  }
  mf <- match.call(expand.dots = FALSE)
  mf[[1]] <- quote(model.frame)
  mf <- eval(mf, parent.frame())
  response <- model.response(mf)
  group <- mf[[2]]
  if (!is.factor(group)) {
    group <- factor(group)
  }
  if (nlevels(group) != 3) {
    stop("group must have exactly 3 levels", call. = FALSE)
  }
  lev <- levels(group)
  x <- response[group == lev[1]]
  y <- response[group == lev[2]]
  z <- response[group == lev[3]]
  res <- vus.default(x, y, z, ...)
  res$formula <- formula
  res$call <- call
  return(res)
}

#' @export
print.vus <- function(x, ...) {
  cat("VUS estimate:\n")
  print(x$estimate)
  if (!is.null(x$conf.int)) {
    cat("\nConfidence interval:\n")
    print(x$conf.int)
  }
  if (!is.null(x$p.value)) {
    cat("\nP-value:", x$p.value, "\n")
  }
  invisible(x)
}

#' @export
summary.vus <- function(object, ...) {
  out <- list(
    estimate = object$estimate,
    conf.int = object$conf.int,
    p.value = object$p.value,
    n = object$n
  )
  class(out) <- "summary.vus"
  out
}
