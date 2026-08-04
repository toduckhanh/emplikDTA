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
  p <- ggplot(data = df, mapping = aes(x = auc, y = elr)) +
    geom_line(linewidth = 0.75) +
    geom_ribbon(data = df[df$inside_ci, ], mapping = aes(ymin = 0, ymax = elr),
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
#' @details ....
#' 
#' 
#' @return \code{auc} returns an object of class "auc" which is a list containing at least the following components:
#'
#' \item{call}{the matched call.}
#' \item{estimate}{the estimated VUS.}
#' \item{conf.int}{a confidence interval for the AUC.}
#' \item{p.value}{the p-value for the test.}
#' \item{r.adj}{the value of adjusted pamarameter.}
#' \item{ci_level}{confidence level of the interval.}
#'
#' Generic functions such as \code{print} is also used to show the results.
#' 
#' 
#' @references
#' To, D. K., Adimari, G., & Chiogna, M. (2024). 
#' Interval estimation in three-class receiver operating characteristic analysis: 
#' A fairly general approach based on the empirical likelihood. 
#' \emph{Statistical Methods in Medical Research}, \bold{33}, 5, 875-893.
#' 
#'@export
auc <- function(x, ...) {
  UseMethod("auc")
}

#' @rdname auc
#' @param x,y numeric vectors (default method) contains values of diagnostic test (biomarker)
#' corresponding to the first and second class of disease. The ordering is intended 
#' to be ``increasing'' with respect to the disease severity.
#' @param auc0 a number indicating the true value of the AUC.
#' @param ci_level a confidence level to be used for constructing the confidence interval; 
#' default is 0.95.
#' @param B the number of bootstrap replicates (default: 500).
#' @param seed the value of \code{.Random.seed} when bootstrap started work.
#' @param plot a logical indicating whether you want to plot empirical likelihood ratio
#' and confidence interval for AUC.
#' @exportS3Method
auc.default <- function(x, y, auc0 = 1/2, ci_level = 0.95, B = 500, seed,
                        plot = FALSE) {
  call <- match.call()
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
  out <- list(estimate = auc_est, n = c(n1 = n1, n2 = n2), call = call)
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
  p_val <- pchisq(r_est * ll_0, df = 1, lower.tail = FALSE)
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
#' The details of model specification are given under ‘Details’.
#' @param data a data frame containing the variables in the formula.
#' @param diag_levels a vector (of strings) containing the ordered name chosen for 
#' the disease classes. The ordering is intended to be ``increasing'' with respect to the 
#' disease severity. If \code{diag_levels = NULL} (default), the elements of the vector 
#' will be automatically determined from data, by considering the order of the means 
#' of the test values for each disease class (diagnostic group).
#' @param subset an optional expression indicating the subset of the rows of data 
#' that should be used in the fit. This can be a logical vector, or a numeric vector
#' indicating which observation numbers are to be included, or a character vector of 
#' the row names to be included. All observations are included by default.
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s. 
#' The default is set by the \code{na.action} setting of \code{\link[base]{options}}, and is 
#' \code{\link[stats]{na.fail}} if that is unset. The ‘factory-fresh’ default is 
#' \code{\link[stats]{na.omit}}. Another possible value is \code{NULL}, no action. 
#' Value \code{\link[stats]{na.exclude}} can be useful.
#' @param ... for formula method: additional arguments to be passed to the \code{\link[emplikDTA]{auc}}
#' i.e., \code{auc0}, \code{ci_level}, \code{B}, \code{plot}.
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
    cat("\n p-value:", x$p.value, "\n")
  }
  invisible(x)
}

#' @rdname auc
#' @export
summary.auc <- function(object, ...) {
  out <- list(estimate = object$estimate, conf.int = object$conf.int,
              p.value = object$p.value, n = object$n)
  class(out) <- "summary.auc"
  out
}
