# Utilities functions ----
sp_se_emp <- function(x, y, tau) {
  c(sp = mean(x <= tau), se = mean(y > tau))
}

.check_roc_curve_data <- function(x, y, ncp) {
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  if (any(!is.finite(x))) {
    stop("'x' must contain only finite values", call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop("'y' must contain only finite values", call. = FALSE)
  }
  if (!is.numeric(ncp) || length(ncp) != 1L || is.na(ncp) ||
      ncp < 3 || ncp != floor(ncp)) {
    stop("'ncp' must be a single integer greater than or equal to 3",
         call. = FALSE)
  }
  as.integer(ncp)
}

.roc_curve_cutpoints <- function(x, y, ncp) {
  values <- c(x, y)
  c(-Inf, seq(min(values), max(values), length.out = ncp - 2), Inf)
}

.roc_curve_points <- function(x, y, cpoint) {
  ROCpoint <- t(vapply(seq_len(length(cpoint)), function(i) {
    sp_se_emp(x = x, y = y, tau = cpoint[i])
  }, numeric(2)))
  colnames(ROCpoint) <- c("Spe", "Sen")
  ROCpoint
}

.roc_curve_df <- function(x) {
  data.frame(FPR = 1 - x$vals[, "Spe"], TPR = x$vals[, "Sen"])
}

.draw_roc_curve <- function(x, color = "steelblue", linewidth = 1) {
  df <- .roc_curve_df(x)
  ggplot(data = df, mapping = aes(x = FPR, y = TPR)) +
    geom_path(colour = color, linewidth = linewidth) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, colour = "grey60") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    labs(x = "1 - Specificity", y = "Sensitivity") +
    theme_bw()
}

.new_ROC_emp_curve <- function(x, y, ncp = 150, call = NULL) {
  ncp <- .check_roc_curve_data(x, y, ncp)
  cutpoints <- .roc_curve_cutpoints(x, y, ncp)
  ROCpoint <- .roc_curve_points(x, y, cutpoints)
  structure(c(list(x = x, y = y, vals = ROCpoint, cutpoints = cutpoints, 
                   ncp = ncp, n = c(x = length(x), y = length(y)), 
                   call = call)),
            class = c("ROC_emp_curve", "roc_emp_curve"))
}

# Empirical Likelihood Inference for ROC curve ----
#' @name ROC_emp_curve
#' 
#' @title Empirical ROC curve estimation and Empirical Likelihood Inference for ROC curve
#' @aliases ROC_emp_curve
#' @aliases ROC_emp_curve.default
#' @aliases ROC_emp_curve.formula
#' @description This function estimates the ROC curve of a continuous diagnostic test (biomarker) 
#' in two-class setting.
#' 
#' 
#' @details ....
#' 
#' 
#' @return \code{ROC_emp_curve} returns an object of class "ROC_emp_curve" which 
#' is a list containing at least the following components:
#'
#' \item{call}{the matched call.}
#' \item{ROCpoint}{the estimated ROC curve: a matrix with two columns named Specificity and
#' Sensitivity.}
#' \item{cutpoints}{a vector of cutpoints.}
#'
#' Generic functions such as \code{plot} is also used to make a plot of ROC surface
#'  with \code{ggplot2} style.
#' 
#' 
#' @references
#' To, D. K., Adimari, G., & Chiogna, M. (2024). 
#' Interval estimation in three-class receiver operating characteristic analysis: 
#' A fairly general approach based on the empirical likelihood. 
#' \emph{Statistical Methods in Medical Research}, \bold{33}, 5, 875-893.
#' 
#' 
#' @export
ROC_emp_curve <- function(x, ...) {
  UseMethod("ROC_emp_curve")
}

#' @rdname ROC_emp_curve
#' @param x,y numeric vectors (default method) contains values of diagnostic test (biomarker)
#' corresponding to the first and second class of disease. The ordering is intended 
#' to be ``increasing'' with respect to the disease severity.
#' @param ncp an integer specifying the number of cut-points used to construct
#'   the empirical ROC curve. Larger values produce a smoother approximation at
#'   the expense of additional computation.
#' @param main a character string giving the title of the plot.
#' @param color a character string specifying the color of the ROC curve.
#' @param plot a logical indicating whether the empirical ROC curve should be
#'   plotted. If `FALSE`, the computed ROC curve object is returned without
#'   producing a graphical display.
#' @exportS3Method
ROC_emp_curve.default <- function(x, y, ncp = 150, main, color = "steelblue",
                          plot = TRUE) {
  object <- .new_ROC_emp_curve(x = x, y = y, ncp = ncp, call = match.call())
  if (plot) {
    main <- if (missing(main)) "Empirical ROC curve" else main
    plot(object, type = "curve", main = main, color = color, alpha = alpha)
  }
  invisible(object)
}

#' @rdname ROC_emp_curve
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
#' @param ... for formula method: additional arguments to be passed to the
#' \code{\link[emplikDTA]{ROC_emp_curve}} i.e., \code{ncp}, \code{main}, \code{color}, 
#' and \code{plot}.
#' @exportS3Method
ROC_emp_curve.formula <- function(formula, data, diag_levels = NULL, subset, 
                                  na.action, ...) {
  call <- match.call()
  dat <- .extract_formula_data(formula = formula, data = data,
                               diag_levels = diag_levels, subset = subset,
                               na.action = na.action, n_class = 2)
  object <- ROC_emp_curve(x = dat$split[[1]], y = dat$split[[2]], ...)
  invisible(object)
}

#' @export
plot.ROC_emp_curve <- function(x, color = "steelblue", linewidth = 1, ...) {
  p <- .draw_roc_curve(x, color = color, linewidth = linewidth)
  invisible(p)
}

.cr_emp_data <- function(x, y, cpt, n = 51) {
  obj <- .empi_llike_prepare(x, y, cpt, type_F = "empi")
  z1 <- seq(0, 1, length.out = n)
  z2 <- seq(0, 1, length.out = n)
  grid <- expand.grid(Spe = z1, Sen = z2, KEEP.OUT.ATTRS = FALSE)
  grid$z <- empi_llike_2C(obj, spe = grid$Spe, sen = grid$Sen)
  return(grid)
}

#' @rdname geom_ROC
#' @title Add a Confidence Region to an Empirical ROC Curve
#' @description
#' Add a Confidence Region to an Empirical ROC Curve
#'
#' Functions for adding graphical layers to an empirical ROC curve produced by
#' \code{\link[emplikDTA]{plot.ROC_emp_curve()}}. These functions return standard
#'  \code{ggplot2} layers and
#' are intended to be combined with the output of `plot()` using the `+` operator.
#'
#' \code{geom_confidence_region} adds the confidence region for the pair of
#' specificity and sensitivity corresponding to a specified cut-point.
#'
#' \code{geom_operating_point()} adds the empirical operating point
#' (false positive rate, true positive rate) corresponding to a specified
#' cut-point.
#' 
#' @param object an object of class \code{ROC_emp_curve}.
#' @param cpt a numeric value specifying the cut-point at which the operating
#'   point and confidence region are computed.
#' @param ci_level the confidence level of the confidence region.
#' @param colour a character string specifying the colour of the graphical
#'   element.
#' @param linewidth a numeric value specifying the line width of the confidence
#'   region boundary.
#' @param size a numeric value specifying the point size.
#'
#' @return
#' \code{geom_confidence_region()} returns a \code{ggplot2::geom_contour()} layer.
#'
#' \code{geom_operating_point()} returns a \code{ggplot2::geom_point()} layer.
#'
#' @seealso
#' \code{\link[emplikDTA]{ROC_emp_curve()}} and \code{\link[emplikDTA]{plot.ROC_emp_curve()}}
#' 
#' @export
geom_confidence_region <- function(object, cpt, ci_level = 0.95, colour = "blue",
                                   linewidth = 0.8) {
  df <- .cr_emp_data(x = object$x, y = object$y, cpt = cpt)
  df <- df[is.finite(df$z), ]
  geom_contour(data = df, mapping = aes(x = 1 - Spe, y = Sen, z = z),
               breaks = qchisq(ci_level, 2), colour = colour, 
               linewidth = linewidth)
}

#' @rdname geom_ROC
#' @export
geom_operating_point <- function(object, cpt, colour = "red", size = 2.5) {
  est <- sp_se_emp(x = object$x, y = object$y, tau = cpt)
  geom_point(data = data.frame(FPR = 1 - est["sp"], TPR = est["se"]),
             mapping = aes(FPR, TPR), colour = colour, size = size)
}

