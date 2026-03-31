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
