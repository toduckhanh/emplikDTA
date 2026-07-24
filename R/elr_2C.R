# empirical likelihood ratio function for (TCF1, TCF2, TCF3) ----
.empi_llike_prepare <- function(x, y, tau,
                                type_F = c("empi", "Adi", "Adi_ties")) {
  type_F <- match.arg(type_F)
  r1 <- range(x)
  r2 <- range(y)
  inside <- tau >= r1[1] && tau <= r1[2] && tau >= r2[1] && tau <= r2[2]
  if (!inside) return(NULL)
  F1_tau <- switch(type_F,
                   empi = mean(x <= tau),
                   Adi = Fs(x, tau),
                   Adi_ties = Fs_ties(x, tau))
  F2_tau <- switch(type_F,
                   empi = mean(y <= tau),
                   Adi = Fs(y, tau),
                   Adi_ties = Fs_ties(y, tau))
  return(list(n1 = length(x), n2 = length(y), F1 = F1_tau, F2 = F2_tau))
}

empi_llike_2C <- function(obj, spe, sen) {
  if(is.null(obj)) ll <- Inf
  else {
    ll1 <- 2 * obj$n1 * (obj$F1 * log(obj$F1 / spe) +
                       (1 - obj$F1) * log((1 - obj$F1) / (1 - spe)))
    ll2 <- 2 * obj$n2 * (obj$F2 * log(obj$F2 / (1 - sen)) +
                       (1 - obj$F2) * log((1 - obj$F2) / sen))
    ll <- ll1 + ll2
  }
  return(ll)
}

# empi_llike_2C <- function(x, y, n1, n2, spe, sen, tau,
#                           type_F = c("empi", "Adi", "Adi_ties")) {
#   ll <- Inf
#   r1 <- range(x)
#   r2 <- range(y)
#   ckt11 <- as.numeric(tau >= r1[1] & tau <= r1[2])
#   ckt12 <- as.numeric(tau >= r2[1] & tau <= r2[2])
#   if(ckt11 & ckt12){
#     type_F <- match.arg(type_F)
#     F1_tau <- switch(type_F,
#                      empi = mean(x <= tau),
#                      Adi = Fs(x, tau),
#                      Adi_ties = Fs_ties(x, tau)
#                      )
#     F2_tau <- switch(type_F,
#                      empi = mean(y <= tau),
#                      Adi = Fs(y, tau),
#                      Adi_ties = Fs_ties(y, tau)
#                      )
#     
#   }
#   return(ll)
# }
