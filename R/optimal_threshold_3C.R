## Functions to obtain optimal threshold based on Youden index, CtP, MA
GYI_kern <- function(x, y, z, h1, h2, h3, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, z, h1, h2, h3, type_ker){
    if(tau[1] > tau[2]) tau <- rev(tau)
    term1 <- cdf_kernel(x = tau[1], X = x, kernel_type = type_ker, bwd = h1)
    term21 <- cdf_kernel(x = tau[1], X = y, kernel_type = type_ker, bwd = h2)
    term22 <- cdf_kernel(x = tau[2], X = y, kernel_type = type_ker, bwd = h2)
    term3 <- cdf_kernel(x = tau[2], X = z, kernel_type = type_ker, bwd = h3)
    res <- term1 + term22 - term21 + 1 - term3
    return(res)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, 
               upper = a2, control = list(fnscale = -1), 
               x = x, y = y, z = z, h1 = h1,
               h2 = h2, h3 = h3, type_ker = type_ker)$par
  return(out)
}

CtP3_kern <- function(x, y, z, h1, h2, h3, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, z, h1, h2, h3, type_ker){
    if(tau[1] > tau[2]) tau <- rev(tau)
    term1 <- cdf_kernel(x = tau[1], X = x, kernel_type = type_ker, bwd = h1)
    term21 <- cdf_kernel(x = tau[1], X = y, kernel_type = type_ker, bwd = h2)
    term22 <- cdf_kernel(x = tau[2], X = y, kernel_type = type_ker, bwd = h2)
    term3 <- cdf_kernel(x = tau[2], X = z, kernel_type = type_ker, bwd = h3)
    res <- sqrt((1 - term1)^2 + (1 - term22 + term21)^2 + term3^2)
    return(res)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, 
               upper = a2, x = x, y = y, z = z,
               h1 = h1, h2 = h2, h3 = h3, 
               type_ker = type_ker)$par
  return(out)
}

MV_kern <- function(x, y, z, h1, h2, h3, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, z, h1, h2, h3, type_ker){
    if(tau[1] > tau[2]) tau <- rev(tau)
    term1 <- cdf_kernel(x = tau[1], X = x, kernel_type = type_ker, bwd = h1)
    term21 <- cdf_kernel(x = tau[1], X = y, kernel_type = type_ker, bwd = h2)
    term22 <- cdf_kernel(x = tau[2], X = y, kernel_type = type_ker, bwd = h2)
    term3 <- cdf_kernel(x = tau[2], X = z, kernel_type = type_ker, bwd = h3)
    res <- term1 * (term22 - term21) * (1 - term3)
    return(res)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, 
               upper = a2, control = list(fnscale = -1), 
               x = x, y = y, z = z, h1 = h1,
               h2 = h2, h3 = h3, type_ker = type_ker)$par
  return(out)
}


## bootstrap function
bts_opt_tcfs <- function(data_bts, tcf_est, h1, h2, h3, a0, a1, a2,
                         type_ker, type_opt){
  tau_fun <- switch(type_opt,
                    "GYI" = GYI_kern,
                    "CtP" = CtP3_kern,
                    "MV" = MV_kern)
  empi_bts <- vapply(data_bts, function(u){
    x_b <- u$x
    y_b <- u$y
    z_b <- u$z
    tau_kern_b <- tau_fun(x = x_b, y = y_b, z = z_b, h1 = h1, h2 = h2, h3 = h3,
                          type_ker = type_ker, a0 = a0, a1 = a1, a2 = a2)
    obj <- empi_llike3_prepare(x = x_b, y = y_b, z = z_b, tau = tau_kern_b, 
                               type_F = "empi")
    ll_tcfs_b <- empi_llike_3C(obj, tcf1 = tcf_est[1], tcf2 = tcf_est[2], 
                               tcf3 = tcf_est[3])
    return(c(tau = unname(tau_kern_b), ll = unname(ll_tcfs_b)))
  }, FUN.VALUE = c(tau1 = 0, tau2 = 0, ll = 0))
  return(empi_bts)
}

## Main functions

#' @name threshold3
#' 
#' @title Selecting the optimal pair of thresholds and Empirical Likelihood Inference
#' @aliases threshold3
#' @aliases threshold3.default
#' @aliases threshold3.formula
#' @description Estimate two thresholds for three independent ordered groups
#' using kernel CDFs, optionally with bootstrap inference.
#' @param x,y,z Numeric marker vectors for the three groups in increasing order.
#' Use finite data with missing values already handled.
#' @param method Criterion: \code{"GYI"} maximises the sum of TCFs;
#' \code{"CtP"} minimises distance to (1,1,1); \code{"MV"} maximises their
#' product. The default is \code{"GYI"}. \code{"all"} runs all three criteria
#' using shared bandwidths and, when requested, shared bootstrap samples.
#' @param kernel_type Gaussian ("gauss") or Epanechnikov ("epan") kernel.
#' @param bwd Three positive bandwidths in group order, or NULL to estimate them.
#' @param bwd_type RT or AZZ plug-in bandwidth, or CV cross-validation.
#' @param ci Whether to compute bootstrap inference.
#' @param ci_level Confidence level in (0, 1).
#' @param B Positive integer number of bootstrap replicates.
#' @param seed Seed passed to set.seed; default 34. Changes the global RNG state.
#' @param plot Add the joint EL region to an empirical ROC surface when
#' \code{ci = TRUE}.
#' @param formula Formula \code{marker ~ group} with exactly three groups.
#' @param data Data frame containing formula variables.
#' @param diag_levels Group labels in increasing order; NULL uses factor levels.
#' @param subset Optional rows passed to model.frame.
#' @param na.action Missing-value action passed to model.frame.
#' @param ... Arguments passed to the default method.
#' @return For a single criterion, \code{threshold3} returns an object of class
#' "threshold3" which is a list containing the following components:
#'
#' \item{opt_thresh}{Vector of two estimated optimal thresholds.}
#' \item{tcf1_opt,tcf2_opt,tcf3_opt}{Kernel-based estimates of the three true
#' class fractions at \code{opt_thresh}.}
#' \item{n}{Named vector of original group sizes, \code{n1}, \code{n2},
#' and \code{n3}.}
#' \item{call}{The matched call.}
#'
#' When \code{ci = TRUE}, the object additionally contains:
#'
#' \item{out_bts}{A three-row bootstrap matrix containing two estimated
#' thresholds and an EL statistic per replicate. Columns correspond to
#' bootstrap replicates.}
#' \item{c_alp}{Bootstrap-calibrated cutoff for the joint EL region.}
#' \item{ci_tau}{A two-by-two matrix with thresholds in rows and lower/upper
#' bootstrap percentile confidence limits in columns.}
#'
#' For \code{method = "all"}, an object of class \code{"threshold3_all"}
#' is returned. This is a list containing the following components:
#'
#' \item{results}{Named list of \code{threshold3} objects in the order
#' \code{GYI}, \code{CtP}, and \code{MV}, each as described above.}
#' \item{summary}{Data frame with one row per criterion and columns
#' \code{method}, \code{tau1}, \code{tau2}, \code{tcf1_opt},
#' \code{tcf2_opt}, and \code{tcf3_opt}. When \code{ci = TRUE}, it also
#' contains \code{ci_tau1_lower}, \code{ci_tau1_upper},
#' \code{ci_tau2_lower}, and \code{ci_tau2_upper}.}
#' \item{method}{The value \code{"all"}.}
#' \item{n}{Named vector of original group sizes.}
#' \item{bwd}{Bandwidths used for the three groups.}
#' \item{kernel_type}{Kernel used for CDF estimation.}
#' \item{call}{The matched call.}
#' \item{ci_level}{The requested confidence level when inference is computed,
#' or \code{NULL} otherwise.}
#'
#' The formula method additionally returns the following components in the
#' returned object and in each component of \code{results}, when present:
#'
#' \item{formula}{The supplied formula.}
#' \item{group.levels}{Group labels in increasing disease order.}
#' 
#' 
#' @details At ordered thresholds a and b, the fractions are \eqn{F_1(a)},
#' \eqn{F_2(b) - F_2(a)}, and \eqn{1 - F_3(b)}. Optimisation uses \code{L-BFGS-B} with bounds based on
#' adjacent groups. Bootstrap resampling appends the original group extrema,
#' keeps bandwidths fixed and re-estimates both thresholds. Threshold intervals
#' use quantile type 8; the EL cutoff uses the default quantile type.
#' For \code{method = "all"}, bandwidth selection and bootstrap resampling
#' are performed once; each criterion then estimates its own thresholds,
#' TCFs, confidence intervals and EL cutoff. Bootstrap samples are stored
#' as lists, allowing unequal group sizes.
#' The optimiser does not guarantee ordered returned
#' thresholds; inspect \code{opt_thresh} before using the result.
#' @examples
#' set.seed(21)
#' x <- rnorm(40, 0, 1)
#' y <- rnorm(40, 1, 1)
#' z <- rnorm(40, 2, 1)
#' fit <- threshold3(x, y, z, method = "CtP", bwd = rep(0.4, 3), ci = FALSE)
#' fit$opt_thresh
#'
#' fit_all <- threshold3(rnorm(40), rnorm(50, 1), rnorm(60, 2),
#'                       method = "all", bwd = rep(0.4, 3), ci = FALSE)
#' fit_all$summary
#' fit_all$results$GYI$opt_thresh
#' 
#'@export
threshold3 <- function(x, ...) {
  UseMethod("threshold3")
}

#' @rdname threshold3
#' @exportS3Method
threshold3.default <- function(x, y, z, method = c("GYI", "CtP", "MV", "all"),
                               kernel_type = c("gauss", "epan"), bwd = NULL, 
                               bwd_type = c("RT", "AZZ", "CV"), ci = TRUE,
                               ci_level = 0.95, B = 500, seed, plot = FALSE, ...) {
  rlang::check_dots_empty()
  call <- match.call()
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_numeric(z, "z")
  method <- match.arg(method)
  kernel_type <- match.arg(kernel_type)
  bwd_type <- match.arg(bwd_type)
  if(is.null(bwd)){
    bwd <- rep(0, 3)
    bwd[1] <- switch(bwd_type,
                     RT = bwd_plugin(X = x, bwd_method = "RT"),
                     AZZ = bwd_plugin(X = x, bwd_method = "AZZ"),
                     CV = bwd_cv(X = x, kernel_type = kernel_type)$h_opt)
    bwd[2] <- switch(bwd_type,
                     RT = bwd_plugin(X = y, bwd_method = "RT"),
                     AZZ = bwd_plugin(X = y, bwd_method = "AZZ"),
                     CV = bwd_cv(X = y, kernel_type = kernel_type)$h_opt)
    bwd[3] <- switch(bwd_type,
                     RT = bwd_plugin(X = z, bwd_method = "RT"),
                     AZZ = bwd_plugin(X = z, bwd_method = "AZZ"),
                     CV = bwd_cv(X = z, kernel_type = kernel_type)$h_opt)
  } else {
    if(length(bwd) != 3) stop("The bandwidths need to be a vector of three elements.")
  }
  n1 <- length(x)
  n2 <- length(y)
  n3 <- length(z)
  a1 <- c(min(x, y), min(y, z))
  a2 <- c(max(x, y), max(y, z))
  a0 <- c((mean(x) + mean(y))/2, (mean(y) + mean(z))/2)
  methods <- if (method == "all") c("GYI", "CtP", "MV") else method
  tau_functions <- list(GYI = GYI_kern, CtP = CtP3_kern, MV = MV_kern)
  results <- setNames(lapply(methods, function(m) {
    out_thresh <- tau_functions[[m]](
      x = x, y = y, z = z, h1 = bwd[1], h2 = bwd[2], h3 = bwd[3],
      type_ker = kernel_type, a0 = a0, a1 = a1, a2 = a2)
    tcf1_opt <- cdf_kernel(x = out_thresh[1], X = x,
                           kernel_type = kernel_type, bwd = bwd[1])
    tcf3_opt <- 1 - cdf_kernel(x = out_thresh[2], X = z,
                               kernel_type = kernel_type, bwd = bwd[3])
    tcf21_opt <- cdf_kernel(x = out_thresh[1], X = y,
                            kernel_type = kernel_type, bwd = bwd[2])
    tcf22_opt <- cdf_kernel(x = out_thresh[2], X = y,
                            kernel_type = kernel_type, bwd = bwd[2])
    list(opt_thresh = out_thresh, tcf1_opt = tcf1_opt,
         tcf2_opt = tcf22_opt - tcf21_opt, tcf3_opt = tcf3_opt,
         n = c(n1 = n1, n2 = n2, n3 = n3), call = call)
  }), methods)
  if(ci){
    if (missing(seed)) seed <- 34
    set.seed(seed)
    data_bts <- lapply(seq_len(B), function(i, x, y, z, n1, n2, n3){
      x_b <- sample(x, n1, replace = TRUE)
      y_b <- sample(y, n2, replace = TRUE)
      z_b <- sample(z, n3, replace = TRUE)
      x_b <- c(x_b, min(x), max(x))
      y_b <- c(y_b, min(y), max(y))
      z_b <- c(z_b, min(z), max(z))
      return(list(x = x_b, y = y_b, z = z_b))
    }, x = x, y = y, z = z, n1 = n1, n2 = n2, n3 = n3)
    if (plot) {
      ROC_emp_surface(x, y, z)
    }
    for (m in methods) {
      fit <- results[[m]]
      out_bts <- bts_opt_tcfs(
        data_bts = data_bts,
        tcf_est = c(fit$tcf1_opt, fit$tcf2_opt, fit$tcf3_opt),
        h1 = bwd[1], h2 = bwd[2], h3 = bwd[3], a0 = a0, a1 = a1, a2 = a2,
        type_ker = kernel_type, type_opt = m)
      c_alp <- quantile(out_bts[3,], probs = ci_level, names = FALSE,
                        na.rm = TRUE)
      ci_tau_1 <- quantile(out_bts[1,], probs = (1 + c(-1, 1)*ci_level)/2,
                           type = 8, names = FALSE)
      ci_tau_2 <- quantile(out_bts[2,], probs = (1 + c(-1, 1)*ci_level)/2,
                           type = 8, names = FALSE)
      if (plot) {
        tcf1_grid <- seq(0, 1, length.out = 51)
        tcf2_grid <- seq(0, 1, length.out = 51)
        tcf3_grid <- seq(0, 1, length.out = 51)
        obj_tcfs <- empi_llike3_prepare(x = x, y = y, z = z,
                                        tau = fit$opt_thresh, type_F = "empi")
        contour3d(f = function(x, y, z){
          empi_llike_3C(obj_tcfs, tcf1 = x, tcf2 = z, tcf3 = y)
        }, level = c_alp, x = tcf1_grid, y = tcf3_grid, z = tcf2_grid,
        draw = TRUE, add = TRUE, color = "blue", alpha = 0.5, fill = FALSE)
        tcf_orgi <- tcfs_emp(x = x, y = y, z = z, tau = fit$opt_thresh)
        plot3d(tcf_orgi[1], tcf_orgi[3], tcf_orgi[2], type = "s", col = "red",
               radius = 0.01, add = TRUE)
      }
      fit$out_bts <- out_bts
      fit$c_alp <- c_alp
      fit$ci_tau <- rbind(ci_tau_1, ci_tau_2)
      results[[m]] <- fit
    }
  }
  results <- lapply(results, function(fit) {
    class(fit) <- "threshold3"
    fit
  })
  if (method == "all") {
    summary_table <- do.call(rbind, lapply(methods, function(m) {
      fit <- results[[m]]
      row <- data.frame(method = m, tau1 = unname(fit$opt_thresh[1]),
                         tau2 = unname(fit$opt_thresh[2]),
                         tcf1_opt = unname(fit$tcf1_opt),
                         tcf2_opt = unname(fit$tcf2_opt),
                         tcf3_opt = unname(fit$tcf3_opt))
      if (ci) {
        row$ci_tau1_lower <- fit$ci_tau[1, 1]
        row$ci_tau1_upper <- fit$ci_tau[1, 2]
        row$ci_tau2_lower <- fit$ci_tau[2, 1]
        row$ci_tau2_upper <- fit$ci_tau[2, 2]
      }
      row
    }))
    rownames(summary_table) <- NULL
    out <- list(results = results, summary = summary_table, method = method,
                n = c(n1 = n1, n2 = n2, n3 = n3), bwd = bwd,
                kernel_type = kernel_type, call = call,
                ci_level = if (ci) ci_level else NULL)
    class(out) <- "threshold3_all"
  } else {
    out <- results[[1L]]
  }
  return(out)
}

#' @rdname threshold3
#' @exportS3Method
threshold3.formula <- function(formula, data, diag_levels = NULL, subset, 
                               na.action, ...){
  call <- match.call()
  dat <- .extract_formula_data(formula = formula, data = data,
                               diag_levels = diag_levels, subset = subset,
                               na.action = na.action, n_class = 3)
  res <- threshold3.default(dat$split[[1]], dat$split[[2]], dat$split[[3]], ...)
  res$formula <- formula
  res$group.levels <- dat$group.levels
  res$call <- call
  if (inherits(res, "threshold3_all")) {
    res$results <- lapply(res$results, function(fit) {
      fit$formula <- formula
      fit$group.levels <- dat$group.levels
      fit$call <- call
      fit
    })
  }
  return(res)
}

