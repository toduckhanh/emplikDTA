# empirical likelihood ratio function for (TCF1, TCF2, TCF3) ----
empi_llike3_prepare <- function(x, y, z, tau,
                                type_F = c("empi", "Adi", "Adi_ties")){
  type_F <- match.arg(type_F)
  r1 <- range(x)
  r2 <- range(y)
  r3 <- range(z)
  ckt11 <- as.numeric(tau[1] >= r1[1] & tau[1] <= r1[2])
  ckt12 <- as.numeric(tau[1] >= r2[1] & tau[1] <= r2[2])
  ckt21 <- as.numeric(tau[2] >= r2[1] & tau[2] <= r2[2])
  ckt22 <- as.numeric(tau[2] >= r3[1] & tau[2] <= r3[2])
  inside <- ckt11 & (ckt12 | ckt21) & ckt22
  if (!inside) return(NULL)
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
  return(list(n1 = length(x), n2 = length(y), n3 = length(z), 
              F1 = F1_tau1, F2 = F2_tau12, F3 = F3_tau2))
}


empi_llike_3C <- function(obj, tcf1, tcf2, tcf3) {
  if(is.null(obj)) ll <- Inf
  else {
    ll1 <- 2 * obj$n1 * (obj$F1 * log(obj$F1 / tcf1) +
                       (1 - obj$F1) * log((1 - obj$F1) / (1 - tcf1)))
    if (obj$F2 == 0) {
      ll2 <- Inf
    } else {
      ll2 <- 2 * obj$n2 * (obj$F2 * log(obj$F2 / tcf2) +
                         (1 - obj$F2) * log((1 - obj$F2) / (1 - tcf2)))
    }
    ll3 <- 2 * obj$n3 * (obj$F3 * log(obj$F3 / (1 - tcf3)) +
                       (1 - obj$F3) * log((1 - obj$F3) / tcf3))
    ll <- ll1 + ll2 + ll3
  }
  return(ll)
}
