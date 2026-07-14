# empirical likelihood ratio function for (TCF1, TCF2, TCF3) ----
empi_llike_3C <- function(x, y, z, n1, n2, n3, tcf1, tcf2, tcf3, tau,
                          type_F = c("empi", "Adi", "Adi_ties")) {
  ll <- Inf
  r1 <- range(x)
  r2 <- range(y)
  r3 <- range(z)
  ckt11 <- as.numeric(tau[1] >= r1[1] & tau[1] <= r1[2])
  ckt12 <- as.numeric(tau[1] >= r2[1] & tau[1] <= r2[2])
  ckt21 <- as.numeric(tau[2] >= r2[1] & tau[2] <= r2[2])
  ckt22 <- as.numeric(tau[2] >= r3[1] & tau[2] <= r3[2])
  if(ckt11 & (ckt12 | ckt21) & ckt22){
    type_F <- match.arg(type_F)
    F1_tau1 <- switch(type_F,
                      empi = mean(x <= tau[1]),
                      Adi = Fs(x, tau[1]),
                      Adi_ties = Fs_ties(x, tau[1])
    )
    F2_tau12 <- switch(type_F,
                       empi = mean(y <= tau[2]) - mean(y <= tau[1]),
                       Adi = Fs(y, tau[2]) - Fs(y, tau[1]),
                       Adi_ties = Fs_ties(y, tau[2]) - Fs_ties(y, tau[1])
    )
    F3_tau2 <- switch(type_F,
                      empi = mean(z <= tau[2]),
                      Adi = Fs(z, tau[2]),
                      Adi_ties = Fs_ties(z, tau[2])
    )
    ll1 <- 2 * n1 * (F1_tau1 * log(F1_tau1 / tcf1) +
                       (1 - F1_tau1) * log((1 - F1_tau1) / (1 - tcf1)))
    if (F2_tau12 == 0) {
      ll2 <- Inf
    } else {
      ll2 <- 2 * n2 * (F2_tau12 * log(F2_tau12 / tcf2) +
                         (1 - F2_tau12) * log((1 - F2_tau12) / (1 - tcf2)))
    }
    ll3 <- 2 * n3 * (F3_tau2 * log(F3_tau2 / (1 - tcf3)) +
                       (1 - F3_tau2) * log((1 - F3_tau2) / tcf3))
    ll <- ll1 + ll2 + ll3
  }
  return(ll)
}
