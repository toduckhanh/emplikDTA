####========================================================================####
## This file contains functions for estimating the empirical likelihood ratio ##
## for three TCFs at given pair of thresholds                                 ##
####========================================================================####

#' @import stats
#' @import utils

## ---- bootstrap procedure to compute w for EL for TCF2 ----
bts_func_3C <- function(x, y, z, n1, n2, n3, tcf1, tcf2, tcf3,
                        enlarged = TRUE, B, type_F) {
  empi_bts <- sapply(1:B, function(i){
    # flag <- 0
    # while(flag == 0){
      x.b <- sample(x, n1, replace = TRUE)
      y.b <- sample(y, n2, replace = TRUE)
      z.b <- sample(z, n3, replace = TRUE)
      if (enlarged) {
        x.b <- c(x.b, range(x))
        y.b <- c(y.b, range(y))
        z.b <- c(z.b, range(z))
        n1 <- n1 + 2
        n2 <- n2 + 2
        n3 <- n3 + 2
      }
      # flag <- as.numeric((mean(x.b) < mean(y.b)) * (mean(y.b) < mean(z.b)))
    # }
    tau1_est <- quantile(x.b, tcf1, names = FALSE)
    tau2_est <- quantile(z.b, 1 - tcf3, names = FALSE)
    if (tau1_est > tau2_est) {
      temp <- tau2_est
      tau2_est <- tau1_est
      tau1_est <- temp
    }
    res <- empi_llike_3C(x = x.b, y = y.b, z = z.b, n1 = n1, n2 = n2, n3 = n3, 
                         tcf1 = tcf1, tcf2 = tcf2, tcf3 = tcf3, 
                         tau = c(tau1_est, tau2_est), type_F = type_F)
    return(res)
  })
  empi_bts[is.na(empi_bts)] <- Inf
  r_est <- qchisq(0.5, 1) / median(empi_bts)
  return(r_est)
}

#' @export
EL_ci_tcf2 <- function(x, y, z, n1, n2, n3, tcf10, tcf30, ci_level = 0.95,
                       enlarged = TRUE, B = 500, seed, plot = TRUE) {
  tau1_est <- quantile(x, probs = tcf10)
  tau2_est <- quantile(z, probs = 1 - tcf30)
  tcf2_emp <- mean(y <= tau2_est) - mean(y <= tau1_est)
  if(missing(seed)) seed <- 32
  set.seed(seed)
  r_est <- bts_func_3C(x = x, y = y, z = z, n1 = n1, n2 = n2, n3 = n3,
                       tcf1 = tcf10, tcf2 = tcf2_emp, tcf3 = tcf30,
                       enlarged = enlarged, B = B, type_F = "Adi_ties")
  ##
  myfun <- function(theta, r_adj, qc) {
    ll_est <- empi_llike_3C(x = x, y = y, z = z, n1 = n1, n2 = n2,
                            n3 = n3, tcf1 = tcf10, tcf2 = theta, tcf3 = tcf30,
                            tau = c(tau1_est, tau2_est), type_F = "empi")
    if(is.na(ll_est)) ll_est <- Inf
    ll_est_adj <- ll_est
    if(!is.infinite(ll_est)){
      ll_est_adj <- r_adj * ll_est_adj
    }
    return(ll_est_adj - qc)
  }
  LI_eng <- uniroot(f = myfun, interval = c(0, tcf2_emp),
                    qc = qchisq(ci_level, 1), r_adj = r_est)$root
  UI_eng <- uniroot(f = myfun, interval = c(tcf2_emp, 1),
                    qc = qchisq(ci_level, 1), r_adj = r_est)$root
  ci_tcf2 <- c(LI_eng, UI_eng)
  ##
  if (plot) {
    x22 <- seq(0, 1, length.out = 101)
    ll2 <- sapply(x22, function(x) {
      myfun(x, qc = qchisq(ci_level, 1), r_adj = r_est) + qchisq(ci_level, 1)
    })
    plot(x22, exp(-0.5*ll2), type = "l", xaxs = "i", yaxs = "i", xlim = c(0, 1),
         ylim = c(0, 1), xlab = "TCF 2", ylab = "Emprical likelihood ratio")
    abline(h = exp(-0.5*qchisq(ci_level, 1)), lty = 2)
    abline(v = c(LI_eng, UI_eng), lty = 2)
    points(x = tcf2_emp, y = 0, pch = 16)
    abline(v = tcf2_emp, lty = 2, col = "blue")
  }
  return(list(tcf2_emp = tcf2_emp, ci_tcf2 = ci_tcf2))
}

