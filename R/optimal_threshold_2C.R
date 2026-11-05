## Functions to obtain optimal threshold based on Youden index, CtP, MA
YI_kern <- function(x, y, h1, h2, type_ker, a0, a1, a2) {
  youden <- function(tau) {
    cdf_kernel(x = tau, X = x, kernel_type = type_ker, bwd = h1) -
      cdf_kernel(x = tau, X = y, kernel_type = type_ker, bwd = h2)
  }
  # Add sample landmarks and bandwidth offsets to resolve narrow peaks.
  landmarks <- function(sample, bandwidth) {
    sample <- sort(unique(sample))
    if (length(sample) > 65L) {
      sample <- sample[round(seq(1, length(sample), length.out = 65L))]
    }
    c(sample - bandwidth, sample, sample + bandwidth)
  }
  grid <- sort(unique(c(seq(a1, a2, length.out = 129L), a0,
                        landmarks(x, h1), landmarks(y, h2))))
  grid <- grid[grid >= a1 & grid <= a2]
  scores <- youden(grid)
  if (length(grid) == 1L || all(scores == scores[1L])) return(a0)

  # Refine each detected peak. Keep the grid points and both bounds as
  # candidates so a local refinement cannot lower the best observed score.
  left <- c(-Inf, head(scores, -1L))
  right <- c(tail(scores, -1L), -Inf)
  peaks <- which(scores >= left & scores >= right &
                   (scores > left | scores > right))
  refined <- lapply(peaks, function(i) {
    lower <- grid[max(1L, i - 1L)]
    width <- grid[min(length(grid), i + 1L)] - lower
    fit <- stats::optimize(function(p) youden(lower + p * width),
                           interval = c(0, 1), maximum = TRUE, tol = 1e-7)
    c(threshold = lower + fit$maximum * width, score = fit$objective)
  })
  refined <- do.call(rbind, refined)
  candidates <- c(grid, refined[, "threshold"])
  scores <- c(scores, refined[, "score"])
  best <- which(scores == max(scores))
  candidates[best[which.min(abs(candidates[best] - a0))]]
}

CtP_kern <- function(x, y, h1, h2, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, h1, h2, type_ker){
    F0 <- cdf_kernel(x = tau, X = x, kernel_type = type_ker, bwd = h1)
    F1 <- cdf_kernel(x = tau, X = y, kernel_type = type_ker, bwd = h2)
    res <- sqrt((1 - F0)^2 + F1^2)
    return(res)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, upper = a2,
               x = x, y = y, h1 = h1, h2 = h2, type_ker = type_ker)$par
  return(out)
}

MA_kern <- function(x, y, h1, h2, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, h1, h2, type_ker){
    F0 <- cdf_kernel(x = tau, X = x, kernel_type = type_ker, bwd = h1)
    F1 <- cdf_kernel(x = tau, X = y, kernel_type = type_ker, bwd = h2)
    res <- log(F0) + log(1 - F1)
    return(res)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, upper = a2,
               control = list(fnscale = -1),
               x = x, y = y, h1 = h1, h2 = h2, type_ker = type_ker)$par
  return(out)
}

## bootstrap function
bts_opt_sen_spe <- function(data_bts, se_est, sp_est, h1, h2, a0, a1, a2,
                            type_ker, type_opt){
  tau_fun <- switch(type_opt,
                    "YI" = YI_kern,
                    "CtP" = CtP_kern,
                    "MA" = MA_kern)
  if (is.null(tau_fun)) stop("Unknown bootstrap threshold method.")
  empi_bts <- vapply(data_bts, function(u){
    x_b <- u$x
    y_b <- u$y
    tau_kern_b <- tau_fun(x = x_b, y = y_b, h1 = h1, h2 = h2,
                          type_ker = type_ker, a0 = a0, a1 = a1, a2 = a2)
    obj <- empi_llike_prepare(x = x_b, y = y_b, tau = tau_kern_b, 
                              type_F = "empi")
    ll_se_sp_b <- empi_llike_2C(obj, spe = sp_est, sen = se_est)
    return(c(tau = unname(tau_kern_b), ll = unname(ll_se_sp_b)))
  }, FUN.VALUE = c(tau = 0, ll = 0))
  return(empi_bts)
}


## Main functions

#' @name threshold2
#' 
#' @title Optimal threshold selection and empirical likelihood inference
#' @aliases threshold2
#' @aliases threshold2.default
#' @aliases threshold2.formula
#' @description Estimate an optimal threshold for a continuous diagnostic
#'   marker in two independent groups using kernel-smoothed distribution
#'   functions. Optionally compute a bootstrap percentile confidence interval
#'   for the threshold and a bootstrap-calibrated joint empirical likelihood
#'   (EL) confidence region for specificity and sensitivity.
#'
#' @param x Numeric vector of marker measurements in the reference-negative
#'   group (class 0), or a formula supplied as the first argument to the generic.
#'   Larger marker values are assumed to indicate the reference-positive group.
#' @param y Numeric vector of marker measurements in the reference-positive
#'   group (class 1). Used by the default method.
#' @param method Threshold-selection criterion: \code{"YI"} maximises the
#'   Youden index; \code{"CtP"} minimises the distance to perfect classification;
#'   \code{"MA"} maximises the product of sensitivity and specificity.
#'   The default is \code{"YI"}. \code{"all"} runs all three criteria
#'   using shared bandwidths and, when requested, shared bootstrap samples.
#' @param kernel_type Kernel used for CDF estimation: \code{"gauss"} for
#'   the Gaussian kernel (default), or \code{"epan"} for the Epanechnikov kernel.
#' @param bwd Numeric vector of two positive bandwidths, in the order
#'   \code{c(bandwidth_for_x, bandwidth_for_y)}. If \code{NULL}, bandwidths
#'   are selected separately for the two groups using \code{bwd_type}.
#' @param bwd_type Bandwidth-selection method when \code{bwd = NULL}.
#'   \code{"RT"} (default) and \code{"AZZ"} use the corresponding methods
#'   of \code{bwd_plugin}; \code{"CV"} uses \code{bwd_cv}.
#'   Ignored when \code{bwd} is supplied.
#' @param ci Logical; whether to perform bootstrap and EL inference.
#'   Defaults to \code{TRUE}. If \code{FALSE}, only the threshold and
#'   kernel-based specificity and sensitivity estimates are computed.
#' @param ci_level Nominal confidence level, strictly between 0 and 1.
#'   Defaults to \code{0.95}. Used for both the percentile interval of the
#'   threshold and the bootstrap calibration of the joint EL region.
#' @param B Positive integer giving the number of bootstrap replicates.
#'   Defaults to \code{500}; used only when \code{ci = TRUE}.
#' @param seed Random seed for the bootstrap. If omitted, \code{34} is used.
#'   Used only when \code{ci = TRUE}. The function calls \code{set.seed}
#'   and therefore changes the global random-number generator state.
#' @param plot Logical; whether to print \code{plot.threshold2} for the
#'   fitted object. Defaults to \code{FALSE}. Retained for convenience;
#'   estimation and plotting can also be performed separately using
#'   \code{plot(result)}. For \code{method = "all"}, the default plot has
#'   one panel per criterion.
#' @param formula A two-sided formula of the form \code{marker ~ group},
#'   where \code{marker} is numeric and \code{group} identifies the two
#'   reference classes.
#' @param data Data frame containing the variables used in \code{formula}.
#' @param diag_levels Optional vector specifying the two reference-group
#'   labels, in reference-negative then reference-positive order. If
#'   \code{NULL}, group ordering is determined by the formula-data extraction
#'   helper. Ensure that this ordering agrees with the assumed marker direction.
#' @param subset Optional expression specifying the subset of observations
#'   to use in the formula method; passed to the formula-data extraction helper.
#' @param na.action Missing-value handling for the formula method; passed
#'   to the formula-data extraction helper. For the default method, supply
#'   finite observations with missing values already handled.
#' @param ... Arguments passed from the generic or formula method to
#'   \code{threshold2.default}.
#'
#' @details
#' Let \eqn{F_0} and \eqn{F_1} denote the distribution functions in the
#' reference-negative and reference-positive groups. An observation is
#' classified as positive when its marker value exceeds the threshold
#' \eqn{\tau}. Thus,
#' \deqn{\mathrm{Sp}(\tau) = F_0(\tau), \qquad
#'       \mathrm{Se}(\tau) = 1 - F_1(\tau).}
#' The two CDFs are estimated using \code{cdf_kernel}. The threshold is
#' optimised over the range of the pooled observations. For \code{"YI"},
#' a grid search locates candidate peaks and bounded one-dimensional
#' optimisation refines them. This search is also used in each bootstrap
#' replicate. For \code{"CtP"} and \code{"MA"}, optimisation uses bounded
#' \code{optim} with method \code{"L-BFGS-B"}.
#'
#' The available criteria are:
#' \describe{
#'   \item{\code{"YI"}}{Maximise
#'     \eqn{\mathrm{Sp}(\tau) + \mathrm{Se}(\tau) - 1
#'     = F_0(\tau) - F_1(\tau)}.}
#'   \item{\code{"CtP"}}{Minimise
#'     \eqn{\sqrt{[1 - \mathrm{Sp}(\tau)]^2 +
#'     [1 - \mathrm{Se}(\tau)]^2}}, the distance to
#'     \eqn{(\mathrm{Sp}, \mathrm{Se}) = (1, 1)}.}
#'   \item{\code{"MA"}}{Maximise
#'     \eqn{\mathrm{Sp}(\tau)\mathrm{Se}(\tau)} by maximising
#'     \eqn{\log \mathrm{Sp}(\tau) + \log \mathrm{Se}(\tau)}.}
#' }
#' With \code{method = "all"}, bandwidth selection and bootstrap resampling
#' are performed only once. The three criteria are fitted to the same original
#' observations and the same bootstrap replicates. Each criterion retains
#' its own threshold, threshold interval and bootstrap EL cutoff. EL grid
#' coordinates are constructed once; grid statistics are reused only when
#' two fitted thresholds are exactly identical.
#' Shared resamples do not make the three confidence regions jointly
#' calibrated across criteria: each region has nominal level
#' \code{ci_level} separately.
#'
#' When \code{ci = TRUE}, observations are resampled independently within
#' each group. The original minimum and maximum of each group are appended
#' to its resampled observations. The threshold is re-estimated in every
#' replicate, keeping the original bandwidths and optimisation bounds fixed.
#' The threshold interval uses bootstrap quantiles at
#' \eqn{(1-\mathrm{ci\_level})/2} and
#' \eqn{(1+\mathrm{ci\_level})/2}, with \code{quantile(type = 8)}.
#' This interval is a bootstrap percentile interval, not an EL interval.
#'
#' The bootstrap EL statistic is evaluated at the original kernel estimates
#' of specificity and sensitivity, using each replicate's estimated threshold
#' and empirical CDFs. Its \code{ci_level} quantile, computed using the default
#' \code{quantile} type, is stored as \code{c_alp}. For the original samples,
#' the empirical-CDF EL statistic at the estimated threshold is evaluated on
#' a \eqn{501 \times 501} grid over \eqn{[0,1]^2}. The contour
#' \eqn{z = \mathrm{c\_alp}} gives the plotted joint confidence boundary.
#' This is not a pair of separate marginal confidence intervals.
#'
#' The default plot displays the empirical point stored in
#' \code{spe_sen_emp} when available, matching the original plotting behaviour.
#' The returned \code{spe_opt} and \code{sen_opt}, by contrast, are
#' kernel-based estimates. Use \code{plot(result, point = "kernel")} to
#' display those estimates. Changing the displayed point does not recompute
#' the stored EL region. With \code{ci = FALSE}, the plot shows only the
#' kernel-based point.
#'
#' @section Computational considerations:
#' The two groups may have different sample sizes. Bootstrap samples are
#' stored as pairs of vectors rather than as equal-length data-frame columns.
#' EL grid evaluation may be computationally expensive, particularly for
#' \code{method = "all"} when all three fitted thresholds differ.
#' A non-finite bootstrap EL cutoff causes an error rather than a plot with
#' an undefined confidence boundary.
#'
#' @return For a single criterion, \code{threshold2} returns an object of class
#' "threshold2" which is a list containing the following components:
#'
#' \item{opt_thresh}{Estimated optimal threshold.}
#' \item{spe_opt}{Kernel-based specificity at \code{opt_thresh}.}
#' \item{sen_opt}{Kernel-based sensitivity at \code{opt_thresh}.}
#' \item{n}{Named vector of original group sizes, \code{n1} and \code{n2}.}
#' \item{call}{Matched function call.}
#' \item{method}{The fitted criterion: \code{"YI"}, \code{"CtP"}, or
#' \code{"MA"}.}
#' \item{bwd}{Bandwidths used for the two groups.}
#' \item{kernel_type}{Kernel used for CDF estimation.}
#' \item{ci_level}{The requested confidence level when inference is
#' computed, or \code{NULL} otherwise.}
#'
#' When \code{ci = TRUE}, the object additionally contains:
#'
#' \item{out_bts}{A two-row bootstrap matrix: row \code{tau} contains
#' estimated thresholds and row \code{ll} contains EL statistics.
#' Columns correspond to bootstrap replicates.}
#' \item{c_alp}{Bootstrap-calibrated cutoff for the joint EL region.}
#' \item{ci_tau}{Lower and upper bootstrap percentile confidence limits
#' for the threshold.}
#' \item{grid_sen_spe}{Data frame with columns \code{Spe}, \code{Sen},
#' and \code{z}, containing grid coordinates and their EL statistics.}
#' \item{spe_sen_emp}{Empirical specificity and sensitivity at the
#' estimated threshold, with components \code{sp} and \code{se}.}
#'
#' The formula method additionally returns:
#'
#' \item{formula}{The supplied formula.}
#'
#' Group-label metadata are currently absent because the wrapper reads a
#' field name not returned by the formula-data helper.
#'
#' For \code{method = "all"}, an object of class \code{"threshold2_all"}
#' is returned. This is a list containing the following components:
#'
#' \item{results}{Named list with elements \code{YI}, \code{CtP}, and
#' \code{MA}, each a complete \code{threshold2} object as described above.}
#' \item{summary}{Three-row data frame with columns \code{method},
#' \code{opt_thresh}, \code{spe_opt}, and \code{sen_opt}.
#' Columns \code{ci_tau_lower} and \code{ci_tau_upper} are added when
#' \code{ci = TRUE}.}
#' \item{method}{The value \code{"all"}.}
#' \item{n,bwd,kernel_type,call,ci_level}{Shared metadata as described above.}
#' \item{formula}{The supplied formula, for the formula interface only.}
#'
#' For the formula interface, the formula and matched call are stored both
#' in the parent object and in each fit.
#' Printing the all-methods object displays its summary table.
#'
#' @seealso \code{\link{plot.threshold2}}
#' @examples
#' set.seed(123)
#' negative <- rnorm(60, 0, 1)
#' positive <- rnorm(60, 1.5, 1)
#'
#' # Point estimation without bootstrap or EL grid evaluation
#' fit <- threshold2(negative, positive, method = "YI",
#'                   bwd = c(0.4, 0.4), ci = FALSE)
#' fit$opt_thresh
#' c(Specificity = fit$spe_opt, Sensitivity = fit$sen_opt)
#'
#' # All criteria, without inference
#' fits <- threshold2(negative, positive, method = "all",
#'                    bwd = c(0.4, 0.4), ci = FALSE)
#' fits$summary
#' fits$results$MA$opt_thresh
#'
#' # Formula interface with explicit reference-group ordering
#' dat <- data.frame(
#'   marker = c(negative, positive),
#'   group = factor(rep(c("negative", "positive"), each = 60),
#'                  levels = c("negative", "positive"))
#' )
#' fit_formula <- threshold2(marker ~ group, data = dat,
#'                           diag_levels = c("negative", "positive"),
#'                           method = "CtP", bwd = c(0.4, 0.4), ci = FALSE)
#'
#' \dontrun{
#' # Full inference; the EL grid can take time to compute
#' fit_el <- threshold2(negative, positive, method = "YI",
#'                      bwd = c(0.4, 0.4), ci = TRUE,
#'                      ci_level = 0.95, B = 500, seed = 34)
#' fit_el$ci_tau
#' plot(fit_el)
#' plot(fit_el, point = "kernel", ci_colour = "red")
#'
#' fits_el <- threshold2(negative, positive, method = "all",
#'                       bwd = c(0.4, 0.4), ci = TRUE, B = 500, seed = 34)
#' fits_el$summary
#' plot(fits_el)                 # One panel per criterion
#' plot(fits_el, method = "YI")  # Only the YI result
#' plot(fits_el$results$CtP)     # A child fit behaves like any threshold2 object
#' }
#' @export
threshold2 <- function(x, ...) {
  UseMethod("threshold2")
}

#' @rdname threshold2
#' @exportS3Method
threshold2.default <- function(x, y, method = c("YI", "CtP", "MA", "all"),
                               kernel_type = c("gauss", "epan"), bwd = NULL, 
                               bwd_type = c("RT", "AZZ", "CV"), ci = TRUE,
                               ci_level = 0.95, B = 500, seed, plot = FALSE, ...) {
  rlang::check_dots_empty()
  call <- match.call()
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  method <- match.arg(method)
  kernel_type <- match.arg(kernel_type)
  bwd_type <- match.arg(bwd_type)
  if (!length(x) || !length(y) || any(!is.finite(x)) || any(!is.finite(y))) {
    stop("'x' and 'y' must contain finite observations.")
  }
  if (!is.logical(ci) || length(ci) != 1L || is.na(ci) ||
      !is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    stop("'ci' and 'plot' must be TRUE or FALSE.")
  }
  if (ci) {
    if (!is.numeric(ci_level) || length(ci_level) != 1L ||
        !is.finite(ci_level) || ci_level <= 0 || ci_level >= 1) {
      stop("'ci_level' must be strictly between 0 and 1.")
    }
    if (!is.numeric(B) || length(B) != 1L || !is.finite(B) ||
        B < 1 || B != floor(B)) {
      stop("'B' must be a positive integer.")
    }
  }
  if(is.null(bwd)){
    bwd <- rep(0, 2)
    bwd[1] <- switch(bwd_type,
                     RT = bwd_plugin(X = x, bwd_method = "RT"),
                     AZZ = bwd_plugin(X = x, bwd_method = "AZZ"),
                     CV = bwd_cv(X = x, kernel_type = kernel_type)$h_opt)
    bwd[2] <- switch(bwd_type,
                     RT = bwd_plugin(X = y, bwd_method = "RT"),
                     AZZ = bwd_plugin(X = y, bwd_method = "AZZ"),
                     CV = bwd_cv(X = y, kernel_type = kernel_type)$h_opt)
  }
  if (!is.numeric(bwd) || length(bwd) != 2L ||
      any(!is.finite(bwd)) || any(bwd <= 0)) {
    stop("The bandwidths must be two finite, positive numbers.")
  }
  n1 <- length(x)
  n2 <- length(y)
  a1 <- min(c(x, y))
  a2 <- max(c(x, y))
  if (a1 == a2) stop("The pooled marker values must not all be identical.")
  a0 <- mean(c(mean(x), mean(y)))
  methods <- if (method == "all") c("YI", "CtP", "MA") else method
  tau_functions <- list(YI = YI_kern, CtP = CtP_kern, MA = MA_kern)

  # Prepare shared inputs once, then fit each criterion independently.
  results <- setNames(lapply(methods, function(m) {
    tau <- tau_functions[[m]](
      x = x, y = y, h1 = bwd[1], h2 = bwd[2], type_ker = kernel_type,
      a0 = a0, a1 = a1, a2 = a2)
    list(
      opt_thresh = tau,
      spe_opt = cdf_kernel(x = tau, X = x, kernel_type = kernel_type,
                           bwd = bwd[1]),
      sen_opt = 1 - cdf_kernel(x = tau, X = y, kernel_type = kernel_type,
                               bwd = bwd[2]),
      n = c(n1 = n1, n2 = n2), call = call, method = m,
      bwd = bwd, kernel_type = kernel_type,
      ci_level = if (ci) ci_level else NULL)
  }), methods)
  if(ci){
    if (missing(seed)) seed <- 34
    set.seed(seed)
    data_bts <- lapply(seq_len(B), function(i){
      x_b <- x[sample.int(n1, n1, replace = TRUE)]
      y_b <- y[sample.int(n2, n2, replace = TRUE)]
      x_b <- c(x_b, min(x), max(x))
      y_b <- c(y_b, min(y), max(y))
      # Lists support different sample sizes without data-frame recycling.
      list(x = x_b, y = y_b)
    })
    grid_coordinates <- expand.grid(
      Spe = seq(0, 1, length.out = 501),
      Sen = seq(0, 1, length.out = 501), KEEP.OUT.ATTRS = FALSE)
    fitted_thresholds <- vapply(results, function(r) unname(r$opt_thresh),
                                numeric(1))
    for (i in seq_along(methods)) {
      m <- methods[i]
      fit <- results[[m]]
      fit$out_bts <- bts_opt_sen_spe(
        data_bts = data_bts, se_est = fit$sen_opt, sp_est = fit$spe_opt,
        h1 = bwd[1], h2 = bwd[2], a0 = a0, a1 = a1, a2 = a2,
        type_ker = kernel_type, type_opt = m)
      if (any(!is.finite(fit$out_bts["tau", ])) ||
          anyNA(fit$out_bts["ll", ])) {
        stop(sprintf("Invalid bootstrap results for method '%s'.", m))
      }
      fit$c_alp <- quantile(fit$out_bts["ll", ], probs = ci_level,
                            names = FALSE)
      if (!is.finite(fit$c_alp)) {
        stop(sprintf("The bootstrap EL cutoff is not finite for method '%s'.", m))
      }
      fit$ci_tau <- quantile(fit$out_bts["tau", ],
                             probs = (1 + c(-1, 1)*ci_level)/2,
                             type = 8, names = FALSE)
      # Identical thresholds have identical original-sample EL grids;
      # bootstrap cutoffs remain separately calibrated for each criterion.
      previous <- match(fitted_thresholds[i], fitted_thresholds)
      if (previous < i) {
        fit$grid_sen_spe <- results[[previous]]$grid_sen_spe
        fit$spe_sen_emp <- results[[previous]]$spe_sen_emp
      } else {
        obj <- empi_llike_prepare(x = x, y = y, tau = fit$opt_thresh,
                                  type_F = "empi")
        fit$grid_sen_spe <- grid_coordinates
        fit$grid_sen_spe$z <- empi_llike_2C(
          obj, spe = grid_coordinates$Spe, sen = grid_coordinates$Sen)
        fit$spe_sen_emp <- sp_se_emp(x = x, y = y, tau = fit$opt_thresh)
      }
      results[[m]] <- fit
    }
  }
  results <- lapply(results, function(fit) {
    class(fit) <- "threshold2"
    fit
  })
  if (method == "all") {
    summary_table <- do.call(rbind, lapply(methods, function(m) {
      fit <- results[[m]]
      row <- data.frame(method = m, opt_thresh = unname(fit$opt_thresh),
                         spe_opt = unname(fit$spe_opt),
                         sen_opt = unname(fit$sen_opt))
      if (ci) {
        row$ci_tau_lower <- fit$ci_tau[1]
        row$ci_tau_upper <- fit$ci_tau[2]
      }
      row
    }))
    rownames(summary_table) <- NULL
    out <- list(results = results, summary = summary_table, method = method,
                n = c(n1 = n1, n2 = n2), bwd = bwd,
                kernel_type = kernel_type, call = call,
                ci_level = if (ci) ci_level else NULL)
    class(out) <- "threshold2_all"
  } else {
    out <- results[[1L]]
  }
  if (plot) print(graphics::plot(out))
  return(out)
}

#' @rdname threshold2
#' @exportS3Method
threshold2.formula <- function(formula, data, diag_levels = NULL, subset, 
                               na.action, ...){
  call <- match.call()
  dat <- .extract_formula_data(formula = formula, data = data,
                               diag_levels = diag_levels, subset = subset,
                               na.action = na.action, n_class = 2)
  res <- threshold2.default(dat$split[[1]], dat$split[[2]], ...)
  res$formula <- formula
  res$group.levels <- dat$levl_class
  res$call <- call
  if (inherits(res, "threshold2_all")) {
    res$results <- lapply(res$results, function(fit) {
      fit$formula <- formula
      fit$group.levels <- dat$levl_class
      fit$call <- call
      fit
    })
  }
  return(res)
}

#' Print a comparison of the three threshold-selection methods
#'
#' @param x An object of class \code{threshold2_all}.
#' @param ... Additional arguments passed to \code{print.data.frame}.
#' @return The input object, invisibly.
#' @seealso \code{\link{threshold2}}
#' @exportS3Method
print.threshold2_all <- function(x, ...){
  print(x$summary, ...)
  invisible(x)
}


#' Plot sensitivity and specificity at the estimated optimal threshold
#'
#' @description Plot a point estimate and, when available, the boundary of
#'   the joint empirical likelihood (EL) confidence region. Plotting uses
#'   the stored results and does not repeat estimation or bootstrap sampling.
#' @param x An object of class \code{threshold2} or, for the all-methods
#'   plot method, \code{threshold2_all}.
#' @param ci Logical; whether to draw the EL confidence boundary. If
#'   \code{NULL}, draw it when the object contains the required EL results.
#' @param point Which point estimate to display. \code{"auto"} uses the
#'   empirical estimates when available, as in the original plot, and otherwise
#'   uses the kernel estimates. \code{"empirical"} requires
#'   \code{x$spe_sen_emp}; \code{"kernel"} uses \code{x$spe_opt} and
#'   \code{x$sen_opt}.
#' @param point_colour Colour of the point estimate.
#' @param point_size Size of the point estimate.
#' @param ci_colour Colour of the EL confidence boundary.
#' @param ci_linewidth Line width of the EL confidence boundary.
#' @param xlab,ylab Axis labels.
#' @param method For the all-methods plot, \code{"all"} (default) displays
#'   three panels; \code{"YI"}, \code{"CtP"}, or \code{"MA"} displays only
#'   the selected method.
#' @param ... Reserved for additional arguments.
#' @details The contour at \code{x$c_alp} is the boundary of a joint
#'   confidence region for specificity and sensitivity, rather than two
#'   separate marginal confidence intervals. Changing \code{point} changes
#'   only the displayed point; the stored EL region remains the same.
#' @return A \code{ggplot} object that can be printed or further customised.
#' @exportS3Method
plot.threshold2 <- function(x, ci = NULL,
                            point = c("auto", "empirical", "kernel"),
                            point_colour = "black", point_size = 1.5,
                            ci_colour = "blue", ci_linewidth = 0.8,
                            xlab = "Specificity", ylab = "Sensitivity", ...){
  dat <- .threshold2_plot_data(x, ci = ci, point = match.arg(point))
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = dat$point,
                        mapping = ggplot2::aes(x = .data$Spe, y = .data$Sen),
                        colour = point_colour, size = point_size)
  if (!is.null(dat$grid)) {
    p <- p + ggplot2::geom_contour(
      data = dat$grid,
      mapping = ggplot2::aes(x = .data$Spe, y = .data$Sen, z = .data$z),
      breaks = x$c_alp, colour = ci_colour, linewidth = ci_linewidth)
  }
  p + ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_bw()
}

# Internal preparation shared by single- and all-methods plots.
.threshold2_plot_data <- function(x, ci, point){
  if (!inherits(x, "threshold2")) {
    stop("'x' must be an object of class 'threshold2'.")
  }
  has_empirical <- !is.null(x$spe_sen_emp)
  has_ci <- !is.null(x$grid_sen_spe) && !is.null(x$c_alp)
  if (is.null(ci)) ci <- has_ci
  if (!is.logical(ci) || length(ci) != 1L || is.na(ci)) {
    stop("'ci' must be TRUE, FALSE, or NULL.")
  }
  if (point == "auto") {
    point <- if (has_empirical) "empirical" else "kernel"
  }
  if (point == "empirical") {
    if (!has_empirical) {
      stop("Empirical estimates are unavailable; use point = 'kernel'.")
    }
    spe <- unname(x$spe_sen_emp["sp"])
    sen <- unname(x$spe_sen_emp["se"])
  } else {
    spe <- unname(x$spe_opt)
    sen <- unname(x$sen_opt)
  }
  valid_probability <- function(u) {
    is.numeric(u) && length(u) == 1L && is.finite(u) && u >= 0 && u <= 1
  }
  if (!valid_probability(spe) || !valid_probability(sen)) {
    stop("The selected specificity and sensitivity must be finite values in [0, 1].")
  }

  df <- NULL
  if (ci) {
    if (!has_ci) {
      stop("EL results are unavailable; refit with ci = TRUE or plot with ci = FALSE.")
    }
    df <- x$grid_sen_spe
    if (!is.data.frame(df) || !all(c("Spe", "Sen", "z") %in% names(df)) ||
        !all(vapply(df[c("Spe", "Sen", "z")], is.numeric, logical(1)))) {
      stop("'x$grid_sen_spe' must contain numeric columns 'Spe', 'Sen', and 'z'.")
    }
    if (!is.numeric(x$c_alp) || length(x$c_alp) != 1L || !is.finite(x$c_alp)) {
      stop("'x$c_alp' must be a finite numeric value.")
    }
    df <- df[is.finite(df$Spe) & is.finite(df$Sen) & is.finite(df$z), ]
    if (nrow(df) == 0L) stop("No finite EL grid values are available to plot.")
  }
  list(point = data.frame(Spe = spe, Sen = sen), grid = df)
}

#' @rdname plot.threshold2
#' @exportS3Method
plot.threshold2_all <- function(x, method = c("all", "YI", "CtP", "MA"),
                                ci = NULL,
                                point = c("auto", "empirical", "kernel"),
                                point_colour = "black", point_size = 1.5,
                                ci_colour = "blue", ci_linewidth = 0.8,
                                xlab = "Specificity", ylab = "Sensitivity", ...){
  if (!inherits(x, "threshold2_all")) {
    stop("'x' must be an object of class 'threshold2_all'.")
  }
  method <- match.arg(method)
  point <- match.arg(point)
  if (method != "all") {
    return(plot.threshold2(
      x$results[[method]], ci = ci, point = point,
      point_colour = point_colour, point_size = point_size,
      ci_colour = ci_colour, ci_linewidth = ci_linewidth,
      xlab = xlab, ylab = ylab, ...))
  }
  methods <- c("YI", "CtP", "MA")
  prepared <- lapply(methods, function(m) {
    dat <- .threshold2_plot_data(x$results[[m]], ci = ci, point = point)
    dat$point$Method <- factor(m, levels = methods)
    if (!is.null(dat$grid)) {
      dat$grid$Method <- factor(m, levels = methods)
      # A zero contour allows each panel to use its own EL cutoff.
      dat$grid$z <- dat$grid$z - x$results[[m]]$c_alp
    }
    dat
  })
  points <- do.call(rbind, lapply(prepared, function(dat) dat$point))
  grids <- do.call(rbind, lapply(prepared, function(dat) dat$grid))
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = points,
                        mapping = ggplot2::aes(x = .data$Spe, y = .data$Sen),
                        colour = point_colour, size = point_size)
  if (!is.null(grids)) {
    p <- p + ggplot2::geom_contour(
      data = grids,
      mapping = ggplot2::aes(x = .data$Spe, y = .data$Sen, z = .data$z),
      breaks = 0, colour = ci_colour, linewidth = ci_linewidth)
  }
  p + ggplot2::facet_wrap(ggplot2::vars(.data$Method), nrow = 1) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_bw()
}


