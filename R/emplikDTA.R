#' Empirical likelihood inference for diagnostic test accuracy
#'
#' @description Estimation and empirical likelihood inference for continuous
#' markers in two or three independent ordered diagnostic groups.
#' @details Use \code{\link{auc}} and \code{\link{ROC_emp_curve}} for two groups;
#' \code{\link{vus}}, \code{\link{tcf2}} and \code{\link{ROC_emp_surface}} for
#' three groups. \code{\link{threshold2}} and \code{\link{threshold3}} select
#' thresholds using kernel CDFs. Formula interfaces use marker ~ group.
#' Larger marker values indicate higher diagnostic classes. Set diag_levels
#' explicitly when the factor order is not the intended diagnostic order.
#' These interfaces do not accept cluster identifiers or covariate models.
#' @name emplikDTA-package
#' @aliases emplikDTA
"_PACKAGE"

#' @importFrom Rcpp evalCpp
#' @importFrom rlang .data
#' @useDynLib emplikDTA, .registration = TRUE
