empi_llike3_prepare <- function(x, y, z, tau,
                                type_F = c("empi", "Adi", "Adi_ties")){
  type_F <- match.arg(type_F)
  if (tau[1] > tau[2]) return(NULL)
  coincident <- tau[1] == tau[2]
  F1_tau1 <- switch(type_F,
                    empi = mean(x <= tau[1]),
                    Adi = Fs(x, tau[1]),
                    Adi_ties = Fs_ties(x, tau[1])
  )
  F2_tau12 <- if (coincident) 0 else switch(
    type_F,
    empi = mean(y > tau[1] & y <= tau[2]),
    Adi = Fs(y, tau[2]) - Fs(y, tau[1]),
    Adi_ties = Fs_ties(y, tau[2]) - Fs_ties(y, tau[1])
  )
  F3_tau2 <- switch(type_F,
                    empi = mean(z <= tau[2]),
                    Adi = Fs(z, tau[2]),
                    Adi_ties = Fs_ties(z, tau[2])
  )
  return(list(n1 = length(x), n2 = length(y), n3 = length(z),
              F1 = F1_tau1, F2 = F2_tau12, F3 = F3_tau2,
              coincident = coincident))
}


empi_llike_3C <- function(obj, tcf1, tcf2, tcf3) {
  if (is.null(obj)) return(Inf)
  ll1 <- .binomial_deviance(rep_len(obj$F1, length(tcf1)), tcf1, obj$n1)
  ll3 <- .binomial_deviance(rep_len(1 - obj$F3, length(tcf3)), tcf3, obj$n3)
  if (isTRUE(obj$coincident)) {
    ll2 <- ifelse(tcf2 == 0, 0, Inf)
  } else {
    ll2 <- .binomial_deviance(rep_len(obj$F2, length(tcf2)), tcf2, obj$n2)
  }
  invalid <- tcf1 < 0 | tcf1 > 1 | tcf2 < 0 | tcf2 > 1 | tcf3 < 0 | tcf3 > 1
  ifelse(invalid, Inf, ll1 + ll2 + ll3)
}

# At fixed thresholds, resampling the indicators is equivalent to drawing
# binomial counts. Half counts are used only in the generating probabilities.
bts_fixed_tcfs <- function(obj, draws) {
  ll <- .bootstrap_binomial_deviance(obj$F1, obj$n1, draws[, 1L]) +
    .bootstrap_binomial_deviance(1 - obj$F3, obj$n3, draws[, 3L])
  if (!obj$coincident) {
    ll <- ll + .bootstrap_binomial_deviance(obj$F2, obj$n2, draws[, 2L])
  }
  ll
}
