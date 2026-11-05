#' Projected two-sample empirical likelihood statistic
#'
#' @description Low-level calculation of a two-sample minus twice log
#' empirical likelihood ratio at a fixed threshold.
#' @param X1,X2 Numeric observations from two independent ordered groups.
#' @param n1,n2 Sample sizes associated with X1 and X2.
#' @param tcf1,tcf2 Proposed probabilities P(X1 <= tau) and P(X2 > tau).
#' Use interior probabilities for finite calculations.
#' @param tau Single threshold in the overlap of the observed group ranges.
#' @param type_F CDF estimator: "empi" uses empirical proportions;
#' "Adi_ties" uses the interpolated tie-aware CDF. "Adi" requires an Fs
#' helper that is not currently defined in the package and is unsupported.
#' @details This helper does not validate sample sizes or handle zero log
#' terms by limits. Boundary probabilities can produce NaN. Prefer the
#' high-level inference functions for ordinary analyses.
#' @return Numeric likelihood statistic, Inf outside overlapping ranges,
#' or NaN for undefined boundary arithmetic.
#' @examples
#' empi_llike_proj(1:5, 2:6, 5, 5, tcf1 = 0.5, tcf2 = 0.5, tau = 3)
#' @export
empi_llike_proj <- function(X1, X2, n1, n2, tcf1, tcf2, tau,
                            type_F = c("empi", "Adi", "Adi_ties")) {
  ll <- Inf
  a1 <- max(min(X1), min(X2))
  a2 <- min(max(X1), max(X2))
  if(tau >= a1 & tau <= a2){
    type_F <- match.arg(type_F)
    F1_tau <- switch(type_F,
                     empi = mean(X1 <= tau),
                     Adi = Fs(X1, tau),
                     Adi_ties = Fs_ties(X1, tau)
    )
    F2_tau <- switch(type_F,
                     empi = mean(X2 <= tau),
                     Adi = Fs(X2, tau),
                     Adi_ties = Fs_ties(X2, tau)
    )
    ll1 <- 2 * n1 * (F1_tau * log(F1_tau / tcf1) +
                       (1 - F1_tau) * log((1 - F1_tau) / (1 - tcf1)))
    ll2 <- 2 * n2 * ((1 - F2_tau) * log((1 - F2_tau) / tcf2) +
                       F2_tau * log(F2_tau / (1 - tcf2)))
    ll <- ll1 + ll2
  }
  return(ll)
}
