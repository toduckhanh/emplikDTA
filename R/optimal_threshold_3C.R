## Functions to select ordered thresholds by GYI, CtP or MV
threshold3_grid <- function(x, y, z, h1, h2, h3, a0, a1, a2,
                            n_grid, n_landmarks) {
  # Add observations and bandwidth offsets to resolve narrow peaks.
  landmarks <- function(sample, bandwidth) {
    sample <- sort(unique(sample))
    if (length(sample) > n_landmarks) {
      sample <- sample[round(seq(1, length(sample), length.out = n_landmarks))]
    }
    c(sample - bandwidth, sample, sample + bandwidth)
  }
  grid <- sort(unique(c(seq(min(a1), max(a2), length.out = n_grid),
                        a0, a1, a2, landmarks(x, h1), landmarks(y, h2),
                        landmarks(z, h3))))
  grid[grid >= min(a1) & grid <= max(a2)]
}

threshold3_peaks <- function(points, scores, objective) {
  # Refine each detected peak in its neighbouring grid points. Scaling the
  # bracket gives a tolerance relative to the marker scale.
  if (length(points) > 1L && any(scores != scores[1L])) {
    left <- c(-Inf, head(scores, -1L))
    right <- c(tail(scores, -1L), -Inf)
    peaks <- which(scores >= left & scores >= right &
                     (scores > left | scores > right))
    refined <- vapply(peaks, function(i) {
      lower <- points[max(1L, i - 1L)]
      width <- points[min(length(points), i + 1L)] - lower
      fit <- stats::optimize(function(p) objective(lower + p * width),
                             c(0, 1), maximum = TRUE, tol = 1e-7)
      lower + fit$maximum * width
    }, numeric(1))
    points <- c(points, refined)
    scores <- c(scores, objective(refined))
  }
  sorted <- order(points)
  list(points = points[sorted], scores = scores[sorted])
}

threshold3_refine_pair <- function(thresholds, a1, a2, objective) {
  # Map a unit square to the feasible set. The lower bound for b depends
  # on a, so every trial point respects both the order and the class bounds.
  a_upper <- min(a2)
  a_span <- a_upper - a1[1L]
  decode <- function(p) {
    a <- min(a_upper, a1[1L] + p[1L] * a_span)
    b_lower <- max(a, a1[2L])
    b <- min(a2[2L], b_lower + p[2L] * (a2[2L] - b_lower))
    c(a, b)
  }
  b_lower <- max(thresholds[1L], a1[2L])
  b_span <- a2[2L] - b_lower
  start <- c(if (a_span > 0) (thresholds[1L] - a1[1L]) / a_span else 0,
             if (b_span > 0) (thresholds[2L] - b_lower) / b_span else 0)
  fit <- optim(start, function(p) {
    tau <- decode(p)
    objective(tau[1L], tau[2L])
  }, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1),
  control = list(fnscale = -1, ndeps = rep(1e-5, 2L)))
  list(thresholds = decode(fit$par), score = fit$value)
}

threshold3_ordered_pair <- function(x, y, z, h1, h2, h3, type_ker,
                                    a0, a1, a2, score = NULL) {
  bandwidths <- c(h1, h2, h3)
  if (any(!is.finite(bandwidths) | bandwidths <= 0)) {
    stop("Threshold selection requires three finite, positive bandwidths.",
         call. = FALSE)
  }
  # Without a coupled score, use GYI minus its additive constant.
  gyi <- is.null(score)
  cdf1 <- function(t) cdf_kernel(t, x, type_ker, h1)
  cdf2 <- function(t) cdf_kernel(t, y, type_ker, h2)
  cdf3 <- function(t) cdf_kernel(t, z, type_ker, h3)
  grid <- threshold3_grid(x, y, z, h1, h2, h3, a0, a1, a2,
                          n_grid = if (gyi) 129L else 65L,
                          n_landmarks = if (gyi) 65L else 17L)
  lower <- grid >= a1[1L] & grid <= a2[1L]
  upper <- grid >= a1[2L] & grid <= a2[2L]
  overlap <- lower & upper
  F1 <- cdf1(grid)
  F2 <- cdf2(grid)
  F3 <- cdf3(grid)

  if (gyi) {
    first <- function(t) cdf1(t) - cdf2(t)
    second <- function(t) cdf2(t) - cdf3(t)
    a <- threshold3_peaks(grid[lower], (F1 - F2)[lower], first)
    b <- threshold3_peaks(grid[upper], (F2 - F3)[upper], second)

    # Match each b to the best a at or below it using the prefix maxima.
    records <- which(!duplicated(cummax(a$scores)))
    best_a <- findInterval(b$points, a$points[records])
    feasible <- which(best_a > 0L)
    a_index <- records[best_a[feasible]]
    scores <- a$scores[a_index] + b$scores[feasible]
    best <- which.max(scores)
    thresholds <- c(a$points[a_index[best]], b$points[feasible[best]])
    best_score <- scores[best]
    diagonal <- function(t) cdf1(t) - cdf3(t)
    diagonal_scores <- (F1 - F3)[overlap]
  } else {
    objective <- function(a, b) score(cdf1(a), cdf2(b) - cdf2(a), 1 - cdf3(b))
    a <- grid[lower]
    b <- grid[upper]
    scores <- score(F1[lower], outer(F2[lower], F2[upper], function(u, v) v - u),
                    rep(1 - F3[upper], each = length(a)))
    scores[outer(a, b, ">")] <- -Inf
    best <- which.max(scores)
    index <- arrayInd(best, dim(scores))
    thresholds <- c(a[index[1L]], b[index[2L]])
    best_score <- scores[best]

    refined <- threshold3_refine_pair(thresholds, a1, a2, objective)
    if (refined$score > best_score) {
      thresholds <- refined$thresholds
      best_score <- refined$score
    }
    diagonal <- function(t) score(cdf1(t), 0, 1 - cdf3(t))
    diagonal_scores <- score(F1[overlap], 0, 1 - F3[overlap])
  }

  # Refine the boundary where the order constraint is active.
  if (any(overlap)) {
    joint <- threshold3_peaks(grid[overlap], diagonal_scores, diagonal)
    best_joint <- which.max(joint$scores)
    joint_score <- joint$scores[best_joint]
    if (joint_score > best_score || (gyi && joint_score == best_score)) {
      thresholds <- rep(joint$points[best_joint], 2L)
    }
  }
  thresholds
}

GYI_kern <- function(x, y, z, h1, h2, h3, type_ker, a0, a1, a2) {
  threshold3_ordered_pair(x, y, z, h1, h2, h3, type_ker, a0, a1, a2)
}

CtP3_kern <- function(x, y, z, h1, h2, h3, type_ker, a0, a1, a2) {
  score <- function(tcf1, tcf2, tcf3) {
    -((1 - tcf1)^2 + (1 - tcf2)^2 + (1 - tcf3)^2)
  }
  threshold3_ordered_pair(x, y, z, h1, h2, h3, type_ker, a0, a1, a2, score)
}

MV_kern <- function(x, y, z, h1, h2, h3, type_ker, a0, a1, a2) {
  score <- function(tcf1, tcf2, tcf3) tcf1 * tcf2 * tcf3
  threshold3_ordered_pair(x, y, z, h1, h2, h3, type_ker, a0, a1, a2, score)
}


## bootstrap function
bts_opt_tcfs <- function(data_bts, tcf_est, h1, h2, h3, a0, a1, a2,
                         type_ker, tau_fun){
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
#' @param assessment Optional named list with independent assessment vectors
#' \code{x}, \code{y}, and \code{z}. When supplied with \code{ci = TRUE},
#' inference uses these vectors at the thresholds selected from the original
#' samples. The data used to plot the ROC surface and region are also taken
#' from \code{assessment}. Bootstrap replicates hold the thresholds fixed.
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
#' \item{coincident_thresholds}{Whether both thresholds coincide.}
#' \item{tcf1_opt,tcf2_opt,tcf3_opt}{Kernel-based estimates of the three true
#' class fractions at \code{opt_thresh}, computed from the training data
#' even when \code{assessment} is supplied.}
#' \item{n}{Named vector of training group sizes, \code{n1}, \code{n2},
#' and \code{n3}.}
#' \item{call}{The matched call.}
#'
#' When \code{ci = TRUE}, the object additionally contains:
#'
#' \item{out_bts}{A three-row bootstrap matrix containing two estimated
#' thresholds and an EL statistic per replicate. Columns correspond to
#' bootstrap replicates. With \code{assessment}, the first two rows repeat
#' the selected thresholds.}
#' \item{coincident_thresholds_bts}{A logical vector marking bootstrap
#' replicates with coincident thresholds, in the column order of \code{out_bts}.}
#' \item{c_alp}{Bootstrap-calibrated cutoff for the joint EL region.
#' A nonfinite cutoff indicates that the region could not be calibrated.}
#' \item{ci_tau}{A two-by-two matrix with thresholds in rows and lower/upper
#' bootstrap percentile confidence limits in columns, available when
#' thresholds are re-estimated in the bootstrap.}
#'
#' For \code{method = "all"}, an object of class \code{"threshold3_all"}
#' is returned. This is a list containing the following components:
#'
#' \item{results}{Named list of \code{threshold3} objects in the order
#' \code{GYI}, \code{CtP}, and \code{MV}, each as described above.}
#' \item{summary}{Data frame with one row per criterion and columns
#' \code{method}, \code{tau1}, \code{tau2}, \code{tcf1_opt},
#' \code{tcf2_opt}, and \code{tcf3_opt}. With \code{ci = TRUE} and no
#' \code{assessment}, it also contains
#' \code{ci_tau1_lower}, \code{ci_tau1_upper},
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
#' @details The rule assigns marker values at or below \eqn{a} to class 1,
#' values in \eqn{(a,b]} to class 2, and values above \eqn{b} to class 3.
#' The corresponding fractions are \eqn{F_1(a)}, \eqn{F_2(b) - F_2(a)},
#' and \eqn{1 - F_3(b)}. All criteria enforce \eqn{a \le b} within
#' search bounds based on adjacent groups. GYI refines detected peaks of
#' the two CDF differences and matches each upper threshold to the best
#' eligible lower threshold. CtP and MV search ordered grid pairs and
#' refine the best pair with \code{L-BFGS-B} on a parameterization of
#' the feasible set. All three searches also consider the diagonal
#' \eqn{a = b}, where the middle-class fraction is zero.
#' With \code{method = "all"}, the criteria share bandwidths and bootstrap
#' draws, and each criterion has its own cutoff.
#' @section Joint region on an independent assessment sample:
#' Use \code{assessment} to estimate the population TCFs of the selected
#' classification rule on new observations. Conditional on the training
#' data, the selected thresholds are fixed. Use the same marker scale and
#' group order for the training and assessment samples. Assessment group
#' sizes may differ from training group sizes.
#'
#' If \eqn{k_j} of the \eqn{n_j} assessment observations in class \eqn{j}
#' are correctly classified, bootstrap counts are drawn from a binomial
#' distribution with probability \eqn{k_j/n_j}. At zero or one, the
#' generating probability is \eqn{(k_j + 0.5)/(n_j + 1)}. Each bootstrap
#' statistic uses the generated counts and evaluates the deviance at the
#' generating probabilities. The observed statistic uses the actual
#' assessment counts. Coincident thresholds give a region on the plane
#' \eqn{\mathrm{TCF}_2 = 0}, with the cutoff calibrated using the two outer
#' groups. The fixed-threshold bootstrap provides a cutoff for the region.
#' Percentile intervals for the thresholds are available in the refitting
#' workflow described below.
#' @section Joint region with bootstrap threshold refitting:
#' With \code{assessment = NULL}, the joint statistic uses the same data
#' used for threshold selection. The bootstrap resamples within each
#' training group, appends the original group extrema, and refits both
#' thresholds using the original bandwidths. Each replicate evaluates the
#' deviance at its refitted pair against the original kernel TCF estimates.
#' A replicate with coincident thresholds contributes an infinite statistic
#' if the original kernel estimate of TCF2 is positive. This can make the
#' cutoff nonfinite. The marginal threshold intervals use quantile type 8.
#' @section Evaluating and displaying the region:
#' The joint region contains the candidate triplets for which the sum of
#' the class-specific binomial deviances is at most \code{c_alp}.
#' Both bootstrap workflows use the default quantile type for the cutoff.
#' The binomial deviance uses the limits at observed fractions of zero
#' and one, and equals zero when an observed fraction and a candidate
#' coincide at the same endpoint. A candidate of zero or one that
#' contradicts the observed count gives infinite deviance.
#'
#' With \code{ci = TRUE} and \code{plot = TRUE}, the plot overlays the
#' region boundary and empirical operating point on the ROC surface of
#' the data used for inference. The axes are TCF1, TCF3, and TCF2.
#' See \code{\link{CR_tcfs}} for the chi-squared calibration at fixed
#' thresholds.
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
#' # Assess the selected rule on an independent sample.
#' assessment <- list(x = rnorm(60), y = rnorm(60, 1), z = rnorm(60, 2))
#' fit_assessment <- threshold3(x, y, z, method = "GYI",
#'                              assessment = assessment, B = 100)
#' fit_assessment$opt_thresh
#' fit_assessment$c_alp
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
                               ci_level = 0.95, B = 500, seed, plot = FALSE,
                               assessment = NULL, ...) {
  rlang::check_dots_empty()
  call <- match.call()
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_numeric(z, "z")
  if (!is.null(assessment)) {
    if (!is.list(assessment) || length(assessment) != 3L ||
        !setequal(names(assessment), c("x", "y", "z"))) {
      stop("'assessment' must be a list with elements x, y, and z.",
           call. = FALSE)
    }
    for (group in names(assessment)) {
      values <- assessment[[group]]
      .check_numeric(values, paste0("assessment$", group))
      if (any(!is.finite(values))) {
        stop("'assessment' must contain only finite values.", call. = FALSE)
      }
    }
  }
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
         n = c(n1 = n1, n2 = n2, n3 = n3), call = call,
         coincident_thresholds = out_thresh[1L] == out_thresh[2L])
  }), methods)
  if(ci){
    if (!is.numeric(B) || length(B) != 1L || !is.finite(B) ||
        B < 1 || B != floor(B)) {
      stop("'B' must be a positive integer.", call. = FALSE)
    }
    if (missing(seed)) seed <- 34
    set.seed(seed)
    if (is.null(assessment)) {
      data_bts <- lapply(seq_len(B), function(i) {
        list(x = c(x[sample.int(n1, n1, replace = TRUE)], range(x)),
             y = c(y[sample.int(n2, n2, replace = TRUE)], range(y)),
             z = c(z[sample.int(n3, n3, replace = TRUE)], range(z)))
      })
    } else {
      draws <- matrix(runif(3 * B), nrow = B, ncol = 3L)
    }
    region_data <- if (is.null(assessment)) list(x = x, y = y, z = z) else assessment
    if (plot) {
      do.call(ROC_emp_surface, region_data)
    }
    for (m in methods) {
      fit <- results[[m]]
      if (is.null(assessment)) {
        out_bts <- bts_opt_tcfs(
          data_bts = data_bts,
          tcf_est = c(fit$tcf1_opt, fit$tcf2_opt, fit$tcf3_opt),
          h1 = bwd[1], h2 = bwd[2], h3 = bwd[3], a0 = a0, a1 = a1, a2 = a2,
          type_ker = kernel_type, tau_fun = tau_functions[[m]])
        ci_tau_1 <- quantile(out_bts[1,], probs = (1 + c(-1, 1)*ci_level)/2,
                             type = 8, names = FALSE)
        ci_tau_2 <- quantile(out_bts[2,], probs = (1 + c(-1, 1)*ci_level)/2,
                             type = 8, names = FALSE)
        fit$ci_tau <- rbind(ci_tau_1, ci_tau_2)
      } else {
        obj_tcfs <- do.call(empi_llike3_prepare,
                            c(assessment, list(tau = fit$opt_thresh)))
        ll <- if (is.null(obj_tcfs)) rep(Inf, B) else bts_fixed_tcfs(obj_tcfs, draws)
        out_bts <- rbind(tau1 = rep(fit$opt_thresh[1L], B),
                         tau2 = rep(fit$opt_thresh[2L], B), ll = ll)
      }
      fit$coincident_thresholds_bts <- out_bts[1L, ] == out_bts[2L, ]
      c_alp <- quantile(out_bts[3,], probs = ci_level, names = FALSE,
                        na.rm = TRUE)
      if (plot) {
        if (is.null(assessment)) {
          obj_tcfs <- do.call(empi_llike3_prepare,
                              c(region_data, list(tau = fit$opt_thresh)))
        }
        if (is.null(obj_tcfs) || !is.finite(c_alp)) {
          warning(sprintf("%s EL contour unavailable for invalid thresholds or a nonfinite cutoff.", m),
                  call. = FALSE)
        } else {
          .plot_tcf_region(obj_tcfs, c_alp, color = "blue", alpha = 0.5,
                           fill = FALSE)
          tcf_orgi <- do.call(tcfs_emp, c(region_data, list(tau = fit$opt_thresh)))
          plot3d(tcf_orgi[1], tcf_orgi[3], tcf_orgi[2], type = "s", col = "red",
                 radius = 0.01, add = TRUE)
        }
      }
      fit$out_bts <- out_bts
      fit$c_alp <- c_alp
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
      if (!is.null(fit$ci_tau)) {
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

