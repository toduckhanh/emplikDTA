## ---- plot ROC surface and ellipse confidence region for TCFs ----
#' @import rgl
#' @import misc3d

tcfs_emp <- function(x, y, z, tau) {
  c(TCF1 = mean(x <= tau[1]), TCF2 = mean(y <= tau[2] & y > tau[1]), 
    TCF3 = mean(z > tau[2]))
}

.check_roc_surface_data <- function(x, y, z, ncp) {
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_numeric(z, "z")
  if (any(!is.finite(x))) {
    stop("'x' must contain only finite values", call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop("'y' must contain only finite values", call. = FALSE)
  }
  if (any(!is.finite(z))) {
    stop("'z' must contain only finite values", call. = FALSE)
  }
  if (!is.numeric(ncp) || length(ncp) != 1L || is.na(ncp) ||
      ncp < 3 || ncp != floor(ncp)) {
    stop("'ncp' must be a single integer greater than or equal to 3",
         call. = FALSE)
  }
  as.integer(ncp)
}

.roc_surface_cutpoints <- function(x, y, z, ncp) {
  values <- c(x, y, z)
  c(-Inf, seq(min(values), max(values), length.out = ncp - 2), Inf)
}

.roc_surface_threshold_pairs <- function(cutpoints) {
  ncp <- length(cutpoints)
  cp1 <- rep(cutpoints, seq(ncp - 1, 0, by = -1))
  cp2 <- unlist(lapply(seq_len(ncp - 1), function(i) cutpoints[-seq_len(i)]),
                use.names = FALSE)
  cbind(cp1, cp2)
}

.roc_surface_points <- function(x, y, z, cpoint) {
  ROCpoint <- t(vapply(seq_len(nrow(cpoint)), function(i) {
    tcfs_emp(x = x, y = y, z = z, tau = cpoint[i, ])
  }, numeric(3)))
  colnames(ROCpoint) <- c("TCF1", "TCF2", "TCF3")
  rownames(ROCpoint) <- paste(
    "(", round(cpoint[, 1], 3), ", ", round(cpoint[, 2], 3), ")",
    sep = "")
  ROCpoint
}

.roc_surface_grids <- function(ROCpoint, ncp) {
  n_grid <- ncp - 1L
  ct1 <- vapply(seq_len(n_grid), function(i) {
    i * ncp - i * (i + 1) / 2
  }, numeric(1))
  tcf1 <- matrix(ROCpoint[ct1, 1], n_grid, n_grid, byrow = FALSE)
  tcf3 <- matrix(ROCpoint[seq_len(n_grid), 3], n_grid, n_grid, byrow = TRUE)
  tcf2 <- matrix(0, nrow = n_grid, ncol = n_grid)
  tcf2[lower.tri(tcf2, diag = TRUE)] <- ROCpoint[, 2]
  list(tcf1 = tcf1, tcf2 = t(tcf2), tcf3 = tcf3)
}

.new_ROC_emp_surface <- function(x, y, z, ncp = 150, call = NULL) {
  ncp <- .check_roc_surface_data(x, y, z, ncp)
  cutpoints <- .roc_surface_cutpoints(x, y, z, ncp)
  cpoint <- .roc_surface_threshold_pairs(cutpoints)
  ROCpoint <- .roc_surface_points(x, y, z, cpoint)
  grids <- .roc_surface_grids(ROCpoint, ncp)
  structure(c(list(vals = ROCpoint, cpoint = cpoint, cutpoints = cutpoints,
                   ncp = ncp,
                   n = c(x = length(x), y = length(y), z = length(z)),
                   call = call, x = x, y = y, z = z), grids),
            class = c("ROC_emp_surface", "roc_emp_surface"))
}

.roc_surface_default_matrix <- function() {
  rbind(c(-0.8370321, -0.5446390, -0.0523976, 0), 
        c(0.1272045, -0.2868422, 0.9494949, 0),
        c(-0.5321618, 0.7880925, 0.3093767, 0),
        c(0, 0, 0, 1))
}

.open_roc_surface_scene <- function() {
  open3d(antialias = 8)
  par3d(windowRect = 50 + c(0, 0, 1250, 1250),
        userMatrix = .roc_surface_default_matrix())
}

.draw_roc_surface_frame <- function() {
  plot3d(0, 0, 0, type = "n", box = FALSE, xlab = "", ylab = "", zlab = "",
    xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1), axes = FALSE)
  axes3d(edges = c("x--", "y--", "z--"), cex = 1.4, lwd = 2)
  mtext3d("TCF 1", "x--", line = 2, at = 0.35, cex = 1.5)
  mtext3d("TCF 2", "z--", line = 4, at = 0.55, cex = 1.5)
  mtext3d("TCF 3", "y--", line = 4, at = 0.15, level = 2, cex = 1.5)
}

.draw_roc_surface <- function(x, color = "steelblue", alpha = 0.5) {
  surface3d(x$tcf1, x$tcf3, x$tcf2, col = color, alpha = alpha)
}


# Main function for ROC surface ----
# Empirical Likelihood Inference for ROC surface ----
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
#' 
#' @return \code{ROC_emp_surface} returns an object of class "ROC_emp_surface" which 
#' is a list containing at least the following components:
#'
#' \item{call}{the matched call.}
#' \item{ROCpoint}{the estimated ROC surface: a matrix with three columns named TCF1, TCF2 and TCF3.}
#' \item{cutpoints}{a matrix of the pair of cutpoints.}
#'
#' Generic functions such as \code{plot} is also used to make a 3D plot of ROC surface
#'  with \code{rgl} style.
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
ROC_emp_surface <- function(x, ...) {
  UseMethod("ROC_emp_surface")
}


#' @rdname ROC_emp_surface
#' @param x,y,z numeric vectors (default method) contains values of diagnostic tests (biomarkers)
#' corresponding to the first, second and third class of disease. The ordering is intended 
#' to be ``increasing'' with respect to the disease severity.
#' @param ncp an integer specifying the number of cut-points used to construct
#'   the empirical ROC surface. Larger values produce a smoother approximation at
#'   the expense of additional computation.
#' @param main a character string giving the title of the plot.
#' @param color a character string specifying the color of the ROC surface.
#' @param alpha vector of alpha values between 0.0 (fully transparent) and 1.0 (opaque).
#' @param plot a logical indicating whether the empirical ROC surface should be
#'   plotted. If `FALSE`, the computed ROC surface object is returned without
#'   producing a graphical display.
#' @exportS3Method
ROC_emp_surface.default <- function(x, y, z, ncp = 150, main, 
                                    color = "steelblue", alpha = 0.5,
                                    plot = TRUE, ...) {
  object <- .new_ROC_emp_surface(x = x, y = y, z = z, ncp = ncp,
                                 call = match.call())
  if (plot) {
    main <- if (missing(main)) "Empirical ROC surface" else main
    plot(object, type = "surface", main = main, color = color, alpha = alpha)
  }
  invisible(object)
}

#' #' @export
#' ROC_emp_surface <- function(x, y, z, ncp = 150, main, color = "steelblue",
#'                             alpha = 0.5, plot = TRUE) {
#'   object <- .new_ROC_emp_surface(x = x, y = y, z = z,
#'                                  ncp = ncp, call = match.call())
#'   if (plot) {
#'     main <- if (missing(main)) "Empirical ROC surface" else main
#'     plot(object, type = "surface", main = main, color = color, alpha = alpha)
#'   }
#'   invisible(object)
#' }


#' @rdname ROC_emp_surface 
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
#' \code{\link[emplikDTA]{ROC_emp_surface}} i.e., \code{ncp}, \code{main}, \code{color}, \code{alpha}
#' and \code{plot}.
#' @exportS3Method
ROC_emp_surface.formula <- function(formula, data, diag_levels = NULL, subset, 
                                    na.action, ...){
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
  # if (nlevels(group) != 3) {
  #   stop("diagnostic group must have exactly 3 levels", call. = FALSE)
  # }
  # mean_temp <- aggregate(formula, FUN = mean, data = data)
  # temp_levl <- mean_temp[order(mean_temp[, 2]), 1]
  # out_check_levl <- .check_levl_class(trace = TRUE, diag_levels, temp_levl, 
  #                                     n_class = 3)
  # levl_class <- out_check_levl$levl_class
  # x <- response[group == levl_class[1]]
  # y <- response[group == levl_class[2]]
  # z <- response[group == levl_class[3]]
  dat <- .extract_formula_data(formula = formula, data = data,
                               diag_levels = diag_levels, subset = subset,
                               na.action = na.action)
  object <- ROC_emp_surface(x = dat$split[[1]], y = dat$split[[2]], 
                            z = dat$split[[3]], ...)
  invisible(object)
}


#' @export
plot.ROC_emp_surface <- function(x, main = "Empirical ROC surface",
                                 color = "steelblue", alpha = 0.5,
                                 new_window = TRUE, add = FALSE, ...) {
  if (new_window && !add) {
    .open_roc_surface_scene()
  }
  if (!add) {
    .draw_roc_surface_frame()
  }
  .draw_roc_surface(x, color = color, alpha = alpha)
  light3d()
  invisible(x)
}

#' @export
print.ROC_emp_surface <- function(x, ...) {
  cat("Empirical ROC surface\n")
  cat("Cut points:", x$ncp, "\n")
  cat("Threshold pairs:", nrow(x$cpoint), "\n")
  cat("Sample sizes:", paste(names(x$n), x$n, sep = " = ", collapse = ", "),
      "\n")
  invisible(x)
}

#' @export
CR_tcfs <- function(x, ...) {
  UseMethod("CR_tcfs")
}

#' @exportS3Method
CR_tcfs.ROC_emp_surface <- function(x, cpts = NULL, ci_level = 0.95,
                                    color1 = "red", color2 = "blue", smooth = 0,
                                    alpha = 0.5, fill = FALSE) {
  if (is.null(cpts)) {
    stop("Need to specified pair of thresholds to plot the confidence region.")
  } else {
    X1 <- x$x
    X2 <- x$y
    X3 <- x$z
    z1 <- seq(0, 1, length.out = 51)
    z2 <- seq(0, 1, length.out = 51)
    z3 <- seq(0, 1, length.out = 51)
    contour3d(f = function(x, y, z){
      empi_llike_3C(x = X1, y = X2, z = X3, n1 = length(X1), n2 = length(X2), 
                    n3 = length(X3), tcf1 = x, tcf2 = z, tcf3 = y, 
                    tau = cpts, type_F = "empi")
    }, level = qchisq(ci_level, 3), x = z1, y = z3, z = z2, draw = TRUE,
    add = TRUE, color2 = color2, smooth = smooth, alpha = alpha, fill = fill)
    tcf_orgi <- tcfs_emp(x = X1, y = X2, z = X3, tau = cpts)
    plot3d(tcf_orgi[1], tcf_orgi[3], tcf_orgi[2], type = "s", col = color1,
           radius = 0.01, add = TRUE)
  }
  invisible(x)
}


