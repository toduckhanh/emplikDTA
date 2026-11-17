#' Projected two-sample empirical likelihood statistic
#'
#' @description Low-level calculation of a two-sample minus twice log
#' empirical likelihood ratio at a fixed threshold.
#' @param X1,X2 Numeric observations from two independent ordered groups.
#' @param n1,n2 Sample sizes associated with X1 and X2.
#' @param tcf1,tcf2 Proposed probabilities P(X1 <= tau) and P(X2 > tau).
#' @param tau Single threshold on the marker scale.
#' @param type_F CDF estimator: "empi" uses empirical proportions;
#' "Adi_ties" uses the interpolated tie-aware CDF and "Adi" interpolates
#' between order statistics for untied data.
#' @details The binomial deviance uses limits at zero and one. A candidate
#' endpoint incompatible with the observed proportion contributes infinity.
#' With empirical proportions a threshold outside either group's observed
#' range gives a boundary proportion.
#' @return Numeric likelihood statistic.
#' @examples
#' empi_llike_proj(1:5, 2:6, 5, 5, tcf1 = 0.5, tcf2 = 0.5, tau = 3)
#' @export
empi_llike_proj <- function(X1, X2, n1, n2, tcf1, tcf2, tau,
                            type_F = c("empi", "Adi", "Adi_ties")) {
  type_F <- match.arg(type_F)
  obj <- empi_llike_prepare(X1, X2, tau, type_F)
  obj$n1 <- n1
  obj$n2 <- n2
  empi_llike_2C(obj, spe = tcf1, sen = tcf2)
}
