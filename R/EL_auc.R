# bootstrap procedure for EL vus ----
bts_auc <- function(x, y, n1, n2, n, auc_est, B) {
  empi_bts <- sapply(1:B, function(i){
    x.b <- sample(x, n1, replace = TRUE)
    y.b <- sample(y, n2, replace = TRUE)
    auc_est_bts <- auc_core(x.b, y.b)
    if (auc_est_bts == 1) {
      auc_est_bts <- auc_est_bts/(1 + 0.5 / n1 / n2)
    }
    res <- ll_prob(theta = auc_est, theta_est = auc_est_bts, n = n)
    return(res)
  })
  return(empi_bts)
}

# function for plotting the likelihood ratio and confidence interval
plot_auc <- function(auc_est, r_est, ci_level, n, ci) {
  xgrid <- seq(0, 1, by = 0.001)
  ll <- sapply(xgrid, function(x) {
    ll_prob_adj(x, theta_est = auc_est, r_adj = r_est, qc = qchisq(ci_level, 1),
                n = n) + qchisq(ci_level, 1)
  })
  df <- data.frame(auc = xgrid, elr = exp(-0.5 * ll))
  df$inside_ci <- df$auc >= ci[1] & df$auc <= ci[2]
  cutoff <- exp(-0.5 * qchisq(ci_level, 1))
  p <- ggplot(data = df, mapping = aes(x = .data$auc, y = .data$elr)) +
    geom_line(linewidth = 0.75) +
    geom_ribbon(data = df[df$inside_ci, ], mapping = aes(ymin = 0, ymax = .data$elr),
                alpha = 0.2) +
    geom_vline(xintercept = ci, linetype = "dashed") +
    geom_vline(xintercept = auc_est, color = "blue", linewidth = 0.75) +
    geom_hline(yintercept = cutoff, linetype = "dotted") +
    labs(x = "AUC", y = "Empirical likelihood ratio") +
    theme_bw()
  return(p)
}

# Empirical Likelihood Inference for auc ----
#' @name auc
#' 
#' @title Area Under ROC curve (AUC) estimation and Empirical Likelihood Inference for AUC
#' @aliases auc
#' @aliases auc.default
#' @aliases auc.formula
#' @description This function estimates the AUC of a continuous diagnostic test (biomarker) 
#' in two-class setting, together with a confidence interval.
#' 
#' 
#' @details Supply independent samples ordered by increasing marker values,
#' with missing values handled before calling the default method. The formula
#' interface uses \code{marker ~ group} with exactly two groups.
#' The empirical estimate accounts for ties. Bootstrap resampling is performed
#' independently within groups. The likelihood statistic is scaled by
#' \code{qchisq(0.5, 1) / median(r.bts)}, and the confidence limits are found
#' by inversion using a chi-squared cutoff with one degree of freedom.
#' If the empirical estimate is zero or one, the function returns early without
#' inference; an estimate of one is adjusted to \code{1 / (1 + 0.5 / n1 / n2)}.
#' An unbracketed confidence limit is returned as \code{NA}.
#'
#' @section P-value calculation:
#' Let \eqn{\theta}{theta} be the AUC, \eqn{\hat{\theta}}{theta_hat}
#' its empirical estimate, and \eqn{\theta_0}{theta0} the null value
#' (\code{auc0}). For total sample size \eqn{n}, the empirical likelihood (EL)
#' statistic at the null is returned as \code{ll.value} and equals
#' \deqn{D_0 = 2n \left[\hat{\theta}\log\frac{\hat{\theta}}{\theta_0}
#'   + (1-\hat{\theta})\log\frac{1-\hat{\theta}}{1-\theta_0}\right].}{
#'   D0 = 2*n*[theta_hat*log(theta_hat/theta0) +
#'   (1-theta_hat)*log((1-theta_hat)/(1-theta0))].}
#' The test uses the signed root of the bootstrap-adjusted EL statistic
#' \deqn{R = \mathrm{sign}(\hat{\theta}-\theta_0)\sqrt{rD_0},}{
#'   R = sign(theta_hat - theta0) * sqrt(r * D0),}
#' where \eqn{r} is the bootstrap scale factor returned as \code{r.adj}.
#' The function sets \eqn{R=0}{R = 0} at equality and truncates negative
#' values of \eqn{rD_0}{r * D0} caused by rounding to zero before taking the
#' square root.
#'
#' Under \eqn{H_0:\theta=\theta_0}{H0: theta = theta0}, the test uses
#' the standard normal approximation for the signed root (DiCiccio and
#' Romano, 1989). With \eqn{\Phi}{Phi} denoting the standard normal
#' cumulative distribution function, \code{p.value} is computed as follows.
#' \describe{
#'   \item{\code{alternative = "greater"}}{\eqn{1-\Phi(R)}{1 - Phi(R)}
#'     for \eqn{\theta>\theta_0}{theta > theta0}.}
#'   \item{\code{alternative = "less"}}{\eqn{\Phi(R)}{Phi(R)}
#'     for \eqn{\theta<\theta_0}{theta < theta0}.}
#'   \item{\code{alternative = "two.sided"}}{\eqn{2\Phi(-|R|)}{2 * Phi(-abs(R))}
#'     for \eqn{\theta\ne\theta_0}{theta != theta0}. This equals the upper
#'     tail probability of a chi-squared distribution with one degree of
#'     freedom evaluated at \eqn{R^2}{R^2}.}
#' }
#' @return \code{auc} returns an object of class "auc" which is a list
#' containing the following components:
#'
#' \item{call}{the matched call.}
#' \item{estimate}{the estimated AUC.}
#' \item{n}{Named vector of group sizes, \code{n1} and \code{n2}.}
#' \item{alternative}{the alternative hypothesis used for the test.}
#'
#' For interior estimates, the list also contains:
#'
#' \item{conf.int}{a two-sided confidence interval for the AUC.}
#' \item{p.value}{the p-value for testing AUC equal to \code{auc0}
#' against the specified alternative.}
#' \item{ll.value}{Unadjusted empirical likelihood statistic at \code{auc0}.}
#' \item{r.bts}{Bootstrap empirical likelihood statistics.}
#' \item{r.adj}{the value of bootstrap adjustment factor.}
#' \item{ci_level}{confidence level of the interval.}
#'
#' Inference components are absent for boundary estimates. The formula method
#' additionally returns:
#'
#' \item{formula}{The supplied formula.}
#'
#' Group-label metadata are currently not retained. The \code{print} method
#' displays the results and returns the object invisibly.
#' 
#' 
#' @references
#' DiCiccio, T. J., & Romano, J. P. (1989).
#' On adjustments based on the signed root of the empirical likelihood ratio
#' statistic. \emph{Biometrika}, \bold{76}(3), 447--456.
#' \doi{10.1093/biomet/76.3.447}.
#'
#' To, D. K., Adimari, G., & Chiogna, M. (2024). 
#' Interval estimation in three-class receiver operating characteristic analysis: 
#' A fairly general approach based on the empirical likelihood. 
#' \emph{Statistical Methods in Medical Research}, \bold{33}, 5, 875-893.
#' 
#' @examples
#' set.seed(123)
#' x <- rnorm(60)
#' y <- rnorm(60, 1)
#' fit <- auc(x, y, B = 100, seed = 34)
#' summary(fit)
#'@export
auc <- function(x, ...) {
  UseMethod("auc")
}

#' @rdname auc
#' @param x,y numeric vectors (default method) contains values of diagnostic test (biomarker)
#' corresponding to the first and second class of disease. The ordering is intended 
#' to be ``increasing'' with respect to the disease severity.
#' @param auc0 the null-hypothesis value of the AUC, defaulting to \code{1/2}.
#' @param ci_level a confidence level to be used for constructing the confidence interval; 
#' default is 0.95.
#' @param B the number of bootstrap replicates (default: 500).
#' @param seed Integer passed to \code{set.seed}; defaults to 34 when omitted.
#' This changes the global random-number generator state.
#' @param plot a logical indicating whether you want to plot empirical likelihood ratio
#' and confidence interval for AUC.
#' @param alternative The alternative hypothesis. Use \code{"greater"} (default)
#' to test whether AUC is above \code{auc0}, \code{"less"} to test whether
#' it is below, or \code{"two.sided"} to test whether it differs.
#' @exportS3Method
auc.default <- function(x, y, auc0 = 1/2, ci_level = 0.95, B = 500, seed,
                        plot = FALSE, alternative = c("greater", "less", "two.sided"),
                        ...) {
  rlang::check_dots_empty()
  call <- match.call()
  alternative <- match.arg(alternative)
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_ci(ci_level)
  # m1 <- mean(x)
  # m2 <- mean(y)
  # if (m1 > m2) warning("the orders of groups may not hold")
  n1 <- length(x)
  n2 <- length(y)
  n  <- n1 + n2
  auc_est <- auc_core(x, y)
  out <- list(estimate = auc_est, n = c(n1 = n1, n2 = n2), call = call,
              alternative = alternative)
  if (auc_est == 1) {
    out$estimate <- auc_est / (1 + 0.5 / n1 / n2)
    class(out) <- "auc"
    return(out)
  }
  if (auc_est == 0) {
    out$estimate <- 0
    class(out) <- "auc"
    return(out)
  }
  if (missing(seed)) seed <- 34
  set.seed(seed)
  r_bts <- bts_auc(x = x, y = y, n1 = n1, n2 = n2, n = n, auc_est = auc_est,
                   B = B)
  r_est <- qchisq(0.5, 1) / median(r_bts)
  qc <- qchisq(ci_level, 1)
  eps <- .Machine$double.eps^0.5
  LI <- .safe_uniroot(interval = c(eps, auc_est), theta_est = auc_est,
                      qc = qc, r_adj = r_est, n = n)
  UI <- .safe_uniroot(interval = c(auc_est, 1 - eps), theta_est = auc_est,
                      qc = qc, r_adj = r_est, n = n)
  ci <- c(LI, UI)
  if (plot) {
    pl <- plot_auc(auc_est, r_est, ci_level, n, ci)
    print(pl)
  }
  ## p-value
  ll_0 <- ll_prob(theta = auc0, theta_est = auc_est, n = n)
  p_val <- .el_pvalue(auc0, auc_est, r_est * ll_0, alternative)
  ##
  out$conf.int <- ci
  out$p.value <- p_val
  out$ll.value <- ll_0
  out$r.bts <- r_bts
  out$r.adj <- r_est
  out$ci_level <- ci_level
  class(out) <- "auc"
  return(out)
}

#' @rdname auc
#' @param formula an object of class "\code{\link[stats]{formula}}" (or one that can be coerced 
#' to that class): a symbolic description of the model to be fitted. 
#' The details of model specification are given under 'Details'.
#' @param data a data frame containing the variables in the formula.
#' @param diag_levels a vector (of strings) containing the ordered name chosen for 
#' the disease classes. The ordering is intended to be ``increasing'' with respect to the 
#' disease severity. If \code{diag_levels = NULL} (default), the elements of the vector 
#' follow the factor levels of the grouping variable (or the levels produced by
#' coercing it to a factor). Groups are not reordered by their sample means.
#' @param subset an optional expression indicating the subset of the rows of data 
#' that should be used in the fit. This can be a logical vector, or a numeric vector
#' indicating which observation numbers are to be included, or a character vector of 
#' the row names to be included. All observations are included by default.
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s. 
#' The default is set by the \code{na.action} setting of \code{\link[base]{options}}, and is 
#' \code{\link[stats]{na.fail}} if that is unset. The 'factory-fresh' default is 
#' \code{\link[stats]{na.omit}}. Another possible value is \code{NULL}, no action. 
#' Value \code{\link[stats]{na.exclude}} can be useful.
#' @param ... for formula method: additional arguments to be passed to the \code{\link[emplikDTA]{auc}}
#' such as \code{auc0}, \code{ci_level}, \code{B}, \code{plot}, and \code{alternative}.
#' @exportS3Method
auc.formula <- function(formula, data, diag_levels = NULL, subset, 
                        na.action, ...) {
  call <- match.call()
  # if (missing(data)) {
  #   stop("'data' must be provided for formula method", call. = FALSE)
  # }
  # mf <- match.call(expand.dots = FALSE)
  # mf$diag_levels <- NULL
  # mf$... <- NULL
  # mf[[1]] <- quote(model.frame)
  # mf <- eval(mf, parent.frame())
  # response <- model.response(mf)
  # group <- mf[[2]]
  # if (!is.factor(group)) {
  #   group <- factor(group)
  # }
  # if (nlevels(group) != 2) {
  #   stop("diagnostic group must have exactly 2 levels", call. = FALSE)
  # }
  # mean_temp <- aggregate(formula, FUN = mean, data = data)
  # temp_levl <- mean_temp[order(mean_temp[, 2]), 1]
  # out_check_levl <- .check_levl_class(trace = TRUE, diag_levels, temp_levl,
  #                                     n_class = 2)
  # levl_class <- out_check_levl$levl_class
  # x <- response[group == levl_class[1]]
  # y <- response[group == levl_class[2]]
  dat <- .extract_formula_data(formula = formula, data = data,
                               diag_levels = diag_levels, subset = subset,
                               na.action = na.action, n_class = 2)
  res <- auc.default(dat$split[[1]], dat$split[[2]], ...)
  res$formula <- formula
  res$group.levels <- dat$levl_class
  res$call <- call
  return(res)
}

#' @rdname auc
#' @export
print.auc <- function(x, ...) {
  cat("AUC estimate:\n")
  print(x$estimate)
  if (!is.null(x$conf.int)) {
    cat(paste0("\n", x$ci_level * 100, "% Confidence interval:\n"))
    print(x$conf.int)
  }
  if (!is.null(x$p.value)) {
    label <- if (is.null(x$alternative)) "p-value" else paste0("p-value (", x$alternative, ")")
    cat("\n", label, ": ", x$p.value, "\n", sep = "")
  }
  invisible(x)
}

#' @rdname auc
#' @param object An object of class \code{auc} to summarise.
#' @export
summary.auc <- function(object, ...) {
  out <- list(estimate = object$estimate, conf.int = object$conf.int,
              p.value = object$p.value, n = object$n)
  out$alternative <- object$alternative
  class(out) <- "summary.auc"
  out
}
