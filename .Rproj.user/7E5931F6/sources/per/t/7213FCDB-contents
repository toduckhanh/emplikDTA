####========================================================================####
## This file contains functions for estimating the empirical likelihood ratio ##
## for three TCFs at given pair of thresholds                                 ##
####========================================================================####

#' @import stats
#' @import utils

#' @export
empi_llike_3C <- function(X1, X2, X3, n1, n2, n3, tcf1, tcf2, tcf3, tau,
                          type_F = c("empi", "Adi", "Adi_ties")) {
  ll <- Inf
  r1 <- range(X1)
  r2 <- range(X2)
  r3 <- range(X3)
  ckt11 <- as.numeric(tau[1] >= r1[1] & tau[1] <= r1[2])
  ckt12 <- as.numeric(tau[1] >= r2[1] & tau[1] <= r2[2])
  ckt21 <- as.numeric(tau[2] >= r2[1] & tau[2] <= r2[2])
  ckt22 <- as.numeric(tau[2] >= r3[1] & tau[2] <= r3[2])
  if(ckt11 & (ckt12 | ckt21) & ckt22){
    type_F <- match.arg(type_F)
    F1_tau1 <- switch(type_F,
                      empi = mean(X1 <= tau[1]),
                      Adi = Fs(X1, tau[1]),
                      Adi_ties = Fs_ties(X1, tau[1])
    )
    F2_tau12 <- switch(type_F,
                       empi = mean(X2 <= tau[2]) - mean(X2 <= tau[1]),
                       Adi = Fs(X2, tau[2]) - Fs(X2, tau[1]),
                       Adi_ties = Fs_ties(X2, tau[2]) - Fs_ties(X2, tau[1])
    )
    F3_tau2 <- switch(type_F,
                      empi = mean(X3 <= tau[2]),
                      Adi = Fs(X3, tau[2]),
                      Adi_ties = Fs_ties(X3, tau[2])
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

## ---- bootstrap procedure to compute w for EL for TCF2 ----
#' @export
bts_func_3C <- function(X1, X2, X3, n1, n2, n3, tcf1, tcf2, tcf3,
                        enlarged = TRUE, B, type_F) {
  empi_bts <- sapply(1:B, function(i){
    flag <- 0
    while(flag == 0){
      X1.b <- sample(X1, n1, replace = TRUE)
      X2.b <- sample(X2, n2, replace = TRUE)
      X3.b <- sample(X3, n3, replace = TRUE)
      if (enlarged) {
        X1.b <- c(X1.b, min(X1), max(X1))
        X2.b <- c(X2.b, min(X2), max(X2))
        X3.b <- c(X3.b, min(X3), max(X3))
      }
      flag <- as.numeric((mean(X1.b) < mean(X2.b)) * (mean(X2.b) < mean(X3.b)))
    }
    tau1_est <- quantile(X1.b, tcf1, names = FALSE)
    tau2_est <- quantile(X3.b, 1 - tcf3, names = FALSE)
    if (tau1_est > tau2_est) {
      temp <- tau2_est
      tau2_est <- tau1_est
      tau1_est <- temp
    }
    res <- empi_llike_3C(X1 = X1.b, X2 = X2.b, X3 = X3.b, n1 = length(X1.b),
                         n2 = length(X2.b), n3 = length(X3.b), tcf1 = tcf1,
                         tcf2 = tcf2, tcf3 = tcf3, tau = c(tau1_est, tau2_est),
                         type_F = type_F)
    return(res)
  })
  empi_bts[is.na(empi_bts)] <- Inf
  r_est <- qchisq(0.5, 1) / median(empi_bts)
  return(r_est)
}

bts_func_3C_2 <- function(X1, X2, X3, n1, n2, n3, tcf1, tcf2, tcf3,
                          enlarged = TRUE, B, type_F) {
  empi_bts <- sapply(1:B, function(i){
    flag <- 0
    while(flag == 0){
      X1.b <- sample(X1, n1, replace = TRUE)
      X2.b <- sample(X2, n2, replace = TRUE)
      X3.b <- sample(X3, n3, replace = TRUE)
      #
      tau1_est <- quantile(X1.b, tcf1, names = FALSE)
      tau2_est <- quantile(X3.b, 1 - tcf3, names = FALSE)
      if (tau1_est > tau2_est) {
        temp <- tau2_est
        tau2_est <- tau1_est
        tau1_est <- temp
      }
      if (enlarged) {
        X1.b <- c(X1.b, min(X1), max(X1))
        X2.b <- c(X2.b, min(X2), max(X2))
        X3.b <- c(X3.b, min(X3), max(X3))
      }
      flag <- as.numeric((mean(X1.b) < mean(X2.b)) * (mean(X2.b) < mean(X3.b)))
    }
    res <- empi_llike_3C(X1 = X1.b, X2 = X2.b, X3 = X3.b, n1 = length(X1.b),
                         n2 = length(X2.b), n3 = length(X3.b), tcf1 = tcf1,
                         tcf2 = tcf2, tcf3 = tcf3, tau = c(tau1_est, tau2_est),
                         type_F = type_F)
    return(res)
  })
  empi_bts[is.na(empi_bts)] <- Inf
  r_est <- qchisq(0.5, 1) / median(empi_bts)
  return(r_est)
}

#' @export
empi_llike_C2 <- function(X2, n2, tcf2, tau,
                          type_F = c("empi", "Adi", "Adi_ties")) {
  ll <- Inf
  r2 <- range(X2)
  ckt12 <- as.numeric(tau[1] >= r2[1] & tau[1] <= r2[2])
  ckt21 <- as.numeric(tau[2] >= r2[1] & tau[2] <= r2[2])
  if(ckt12 | ckt21) {
    type_F <- match.arg(type_F)
    F2_tau12 <- switch(type_F,
                       empi = mean(X2 <= tau[2]) - mean(X2 <= tau[1]),
                       Adi = Fs(X2, tau[2]) - Fs(X2, tau[1]),
                       Adi_ties = Fs_ties(X2, tau[2]) - Fs_ties(X2, tau[1])
    )
    if (F2_tau12 == 0) {
      ll <- Inf
    } else {
      ll <- 2 * n2 * (F2_tau12 * log(F2_tau12 / tcf2) +
                        (1 - F2_tau12) * log((1 - F2_tau12) / (1 - tcf2)))
    }
  }
  return(ll)
}

#' @export
empi_llike_C3 <- function(X3, n3, tcf3, tau,
                          type_F = c("empi", "Adi", "Adi_ties")) {
  ll <- Inf
  r3 <- range(X3)
  ckt22 <- as.numeric(tau[2] >= r3[1] & tau[2] <= r3[2])
  if(ckt22) {
    type_F <- match.arg(type_F)
    F3_tau2 <- switch(type_F,
                      empi = mean(X3 <= tau[2]),
                      Adi = Fs(X3, tau[2]),
                      Adi_ties = Fs_ties(X3, tau[2])
    )
    ll <- 2 * n3 * (F3_tau2 * log(F3_tau2 / (1 - tcf3)) +
                      (1 - F3_tau2) * log((1 - F3_tau2) / tcf3))
  }
  return(ll)
}

#' @export
empi_llike_C1 <- function(X1, n1, tcf1, tau,
                          type_F = c("empi", "Adi", "Adi_ties")) {
  ll <- Inf
  r1 <- range(X1)
  ckt11 <- as.numeric(tau[1] >= r1[1] & tau[1] <= r1[2])
  if(ckt11) {
    type_F <- match.arg(type_F)
    F1_tau1 <- switch(type_F,
                      empi = mean(X1 <= tau[1]),
                      Adi = Fs(X1, tau[1]),
                      Adi_ties = Fs_ties(X1, tau[1])
    )
    ll <- 2 * n1 * (F1_tau1 * log(F1_tau1 / tcf1) +
                       (1 - F1_tau1) * log((1 - F1_tau1) / (1 - tcf1)))
  }
  return(ll)
}

#' @export
bts_func_C2_1 <- function(X1, X2, n1, n2, tcf1, tcf2, t2, enlarged = TRUE, B,
                          type_F) {
  empi_bts <- sapply(1:B, function(i){
    flag <- 0
    while(flag == 0){
      X1.b <- sample(X1, n1, replace = TRUE)
      X2.b <- sample(X2, n2, replace = TRUE)
      if (enlarged) {
        X1.b <- c(X1.b, min(X1), max(X1))
        X2.b <- c(X2.b, min(X2), max(X2))
      }
      tau1_est <- quantile(X1.b, tcf1, names = FALSE)
      flag <- as.numeric((mean(X1.b) < mean(X2.b)) * (tau1_est < t2))
    }
    ll2_est <- empi_llike_C2(X2 = X2.b, n2 = length(X2.b), tcf2 = tcf2,
                             tau = c(tau1_est, t2), type_F = type_F)
    return(ll2_est)
  })
  empi_bts[is.na(empi_bts)] <- Inf
  r_est <- qchisq(0.5, 1) / median(empi_bts)
  return(r_est)
}

#' @export
bts_func_C2_2 <- function(X2, X3, n2, n3, tcf2, tcf3, t1, enlarged = TRUE, B,
                          type_F) {
  empi_bts <- sapply(1:B, function(i){
    flag <- 0
    while(flag == 0){
      X2.b <- sample(X2, n2, replace = TRUE)
      X3.b <- sample(X3, n3, replace = TRUE)
      #
      if (enlarged) {
        X2.b <- c(X2.b, min(X2), max(X2))
        X3.b <- c(X3.b, min(X3), max(X3))
      }
      tau2_est <- quantile(X3.b, 1 - tcf3, names = FALSE)
      flag <- as.numeric((mean(X1.b) < mean(X2.b)) * (tau2_est > t1))
    }
    ll2_est <- empi_llike_C2(X2 = X2.b, n2 = length(X2.b), tcf2 = tcf2,
                             tau = c(t1, tau2_est), type_F = type_F)
    return(ll2_est)
  })
  empi_bts[is.na(empi_bts)] <- Inf
  r_est <- qchisq(0.5, 1) / median(empi_bts)
  return(r_est)
}

## ---- main function ----

#' @export
EL3C <- function(X1, X2, X3, n1, n2, n3, tcf1, tcf2, tcf3, tau,
                 type_F = c("empi", "Adi", "Adi_ties"), enlarged = TRUE,
                 B = 200, type_F_B) {
  ll_est <- empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                          tcf1 = tcf1, tcf2 = tcf2, tcf3 = tcf3,
                          tau = tau, type_F = type_F)
  if(is.na(ll_est)) ll_est <- Inf
  ll_est_adj <- ll_est
  if(!is.infinite(ll_est)){
    # tcf2_est <- Fs(X2, tau[2]) - Fs(X2, tau[1])
    type_F <- match.arg(type_F)
    tcf2_est <- switch(type_F,
                       empi = mean(X2 <= tau[2]) - mean(X2 <= tau[1]),
                       Adi = Fs(X2, tau[2]) - Fs(X2, tau[1]),
                       Adi_ties = Fs_ties(X2, tau[2]) - Fs_ties(X2, tau[1])
    )
    r_est <- bts_func_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                         tcf1 = tcf1, tcf2 = tcf2_est, tcf3 = tcf3,
                         enlarged = enlarged, B = B, type_F = type_F_B)
    ll_est_adj <- r_est * ll_est_adj
  }
  return(ll_est_adj)
}

#' @export
EL_ci_tcf2 <- function(X1, X2, X3, n1, n2, n3, tcf10, tcf30, ci_level = 0.95,
                       enlarged = TRUE, B = 200, seed, plot = TRUE) {
  ###
  tau1_est <- quantile(X1, probs = tcf10)
  tau2_est <- quantile(X3, probs = 1 - tcf30)
  tcf2_emp <- mean(X2 <= tau2_est) - mean(X2 <= tau1_est)
  if(missing(seed)) seed <- 32
  set.seed(seed)
  r_est <- bts_func_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                       tcf1 = tcf10, tcf2 = tcf2_emp, tcf3 = tcf30,
                       enlarged = enlarged, B = B, type_F = "Adi_ties")
  ##
  myfun <- function(theta, r_adj, qc) {
    ll_est <- empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2,
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

#' @export
EL_ci_tcf23 <- function(X1, X2, X3, n1, n2, n3, tcf10, t2, ci_level = 0.95,
                        enlarged, B = 200, seed, col, ...){
  tau1_est <- quantile(X1, probs = tcf10)
  tcf2_emp <- mean(X2 <= t2) - mean(X2 <= tau1_est)
  tcf3_emp <- 1 - mean(X3 <= t2)
  if(missing(seed)) seed <- 32
  set.seed(seed)
  r_est <- bts_func_C2_1(X1 = X1, X2 = X2, n1 = n1, n2 = n2, tcf1 = tcf10,
                         tcf2 = tcf2_emp, t2 = t2, enlarged = TRUE, B = 200,
                         type_F = "Adi_ties")
  WW <- r_est*rchisq(1000, 1) + rchisq(1000, 1)
  x <- seq(0, 1, length.out = 300)
  y <- seq(0, 1, length.out = 300)
  llR_23 <- outer(x, y, FUN = function(x, y){
    ll_est <- empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2,
                            n3 = n3, tcf1 = tcf10, tcf2 = x, tcf3 = y,
                            tau = c(tau1_est, t2), type_F = "empi")
    return(ll_est)
  })
  if (missing(col)) col = "black"
  contour(x, y, llR_23, levels = quantile(WW, probs = c(0.90, 0.95, 0.99)),
          xlab = "TCF 2", ylab = "TCF 3", labels = c("0.90", "0.95", "0.99"),
          col = col, ...)
  points(tcf2_emp, tcf3_emp, pch = 16, col = col)
  return(c(tcf2_emp, tcf3_emp))
}


