# empirical likelihood ratio function for (Se, Sp) ----
empi_llike_prepare <- function(x, y, tau,
                               type_F = c("empi", "Adi", "Adi_ties")) {
  type_F <- match.arg(type_F)
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
  if (is.null(obj)) return(Inf)
  ll1 <- .binomial_deviance(rep_len(obj$F1, length(spe)), spe, obj$n1)
  ll2 <- .binomial_deviance(rep_len(1 - obj$F2, length(sen)), sen, obj$n2)
  invalid <- spe < 0 | spe > 1 | sen < 0 | sen > 1
  ifelse(invalid, Inf, ll1 + ll2)
}

bts_fixed_sen_spe <- function(obj, draws) {
  .bootstrap_binomial_deviance(obj$F1, obj$n1, draws[, 1L]) +
    .bootstrap_binomial_deviance(1 - obj$F2, obj$n2, draws[, 2L])
}
