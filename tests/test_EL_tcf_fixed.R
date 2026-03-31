
simu_TCF23_norm <- function(n1, n2, n3, mu_true, sigma_true, tcf_10, tcf_20,
                           tcf_30, t2, type_F){
  # generate data
  flag_data <- 0
  while (flag_data == 0) {
    X1 <- rnorm(n1, mu_true[1], sigma_true[1])
    X2 <- rnorm(n2, mu_true[2], sigma_true[2])
    X3 <- rnorm(n3, mu_true[3], sigma_true[3])
    flag_data <- as.numeric((mean(X1) < mean(X2)) * (mean(X2) < mean(X3)))
  }
  tau1_est <- quantile(X1, probs = tcf_10)
  tau2_est <- quantile(X3, probs = 1 - tcf_30)
  # tau2_est <- t2
  # our method
  ll_est <- empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                          tcf1 = tcf_10, tcf2 = tcf_20, tcf3 = tcf_30,
                          tau = c(tau1_est, tau2_est), type_F = type_F)
  return(ll_est)
}

tau0_1 <- c(qnorm(0.8, 0, 1), qnorm(1 - 0.8, 3.69, 1.2))
tcf_10_1 <- pnorm(tau0_1[1], 0, 1)
tcf_20_1 <- pnorm(tau0_1[2], 2.5, 1.1) - pnorm(tau0_1[1], 2.5, 1.1)
tcf_30_1 <- pnorm(tau0_1[2], 3.69, 1.2, lower.tail = FALSE)

out_norm_1_30 <- sapply(1:10000, function(i){
  simu_TCF23_norm(n1 = 30, n2 = 30, n3 = 30, mu_true = c(0, 2.5, 3.69),
                  sigma_true = c(1, 1.1, 1.2), tcf_10 = tcf_10_1,
                  tcf_20 = tcf_20_1, tcf_30 = tcf_30_1, t2 = tau0_1[2],
                  type_F = "empi")
})

ll_adj_est <- 2*(8/9)^3*out_norm_1_30/median(out_norm_1_30) # (7/9)^3 # 2*log(2)
mean(out_norm_1_30 <= qchisq(0.9, df = 2))
mean(ll_adj_est <= qchisq(0.9, df = 2))

mean(out_norm_1_30 <= qchisq(0.95, df = 2))
mean(ll_adj_est <= qchisq(0.95, df = 2))

mean(out_norm_1_30 <= qchisq(0.99, df = 2))
mean(ll_adj_est <= qchisq(0.99, df = 2))


xx <- seq(0, 1, by = 0.001)
ll_qq_est <- sapply(xx, function(x) quantile(out_norm_1_30, probs = x))
ll_adj_qq_est <- sapply(xx, function(x) quantile(ll_adj_est, probs = x))
Chi2_2_qq <- sapply(xx, function(x) qchisq(p = x, df = 2))

# pdf(file = "simulations_3classes/qq_chisq_Norm_1_50.pdf", width = 5, height = 5)
plot(Chi2_2_qq, ll_qq_est, cex = 0.6, pch = 16, xlim = c(0, 15),
     ylim = c(0, 15),
     xlab = expression(paste("Quantile of ", chi[2]^2, " distribution")),
     ylab = "Monte Carlo quantile of statistic")
points(Chi2_2_qq, ll_adj_qq_est, cex = 0.6, pch = 16, col = "blue")
points(qchisq(p = c(0.9, 0.95, 0.99), df = 2),
       qchisq(p = c(0.9, 0.95, 0.99), df = 2), cex = 1.2, pch = 19)
abline(0, 1, col = "gray30", lty = 2)



tau0_2 <- c(qnorm(0.8, 0, 1), qnorm(1 - 0.8, 5.5, 1.2))
tcf_10_2 <- pnorm(tau0_2[1], 0, 1)
tcf_20_2 <- pnorm(tau0_2[2], 3.5, 1.1) - pnorm(tau0_2[1], 3.5, 1.1)
tcf_30_2 <- pnorm(tau0_2[2], 5.5, 1.2, lower.tail = FALSE)

out_norm_2_30 <- sapply(1:10000, function(i){
  simu_TCF23_norm(n1 = 25, n2 = 25, n3 = 25, mu_true = c(0, 3.5, 5.5),
                  sigma_true = c(1, 1.1, 1.2), tcf_10 = tcf_10_2,
                  tcf_20 = tcf_20_2, tcf_30 = tcf_30_2, t2 = tau0_2[2],
                  type_F = "empi")
})

ll_adj_est_2 <- 2*log(2)*out_norm_2_30/median(out_norm_2_30)
mean(out_norm_2_30 <= qchisq(0.9, df = 2))
mean(ll_adj_est_2 <= qchisq(0.9, df = 2))

mean(out_norm_2_30 <= qchisq(0.95, df = 2))
mean(ll_adj_est_2 <= qchisq(0.95, df = 2))

mean(out_norm_2_30 <= qchisq(0.99, df = 2))
mean(ll_adj_est_2 <= qchisq(0.99, df = 2))


xx <- seq(0, 1, by = 0.001)
ll_qq_est_2 <- sapply(xx, function(x) quantile(out_norm_2_30, probs = x))
ll_adj_qq_est_2 <- sapply(xx, function(x) quantile(ll_adj_est_2, probs = x))
Chi2_2_qq <- sapply(xx, function(x) qchisq(p = x, df = 2))

# pdf(file = "simulations_3classes/qq_chisq_Norm_1_50.pdf", width = 5, height = 5)
plot(Chi2_2_qq, ll_qq_est_2, cex = 0.6, pch = 16, xlim = c(0, 15),
     ylim = c(0, 15),
     xlab = expression(paste("Quantile of ", chi[2]^2, " distribution")),
     ylab = "Monte Carlo quantile of statistic")
points(Chi2_2_qq, ll_adj_qq_est_2, cex = 0.6, pch = 16, col = "blue")
points(qchisq(p = c(0.9, 0.95, 0.99), df = 2),
       qchisq(p = c(0.9, 0.95, 0.99), df = 2), cex = 2, pch = 17)
abline(0, 1, col = "gray30", lty = 2)



simu_TCF23_mix <- function(n1, n2, n3, para_gamma, para_lnorm, para_weib,
                           tcf_10, tcf_20, tcf_30, t2, type_F){
  # generate data
  flag_data <- 0
  while (flag_data == 0) {
    X1 <- rgamma(n1, shape = para_gamma[1], rate = para_gamma[2])
    X2 <- rlnorm(n2, meanlog = para_lnorm[1], sdlog = para_lnorm[2])
    X3 <- rweibull(n3, shape = para_weib[1], scale = para_weib[2])
    flag_data <- as.numeric((mean(X1) < mean(X2)) * (mean(X2) < mean(X3)))
  }
  # our method
  tau1_est <- quantile(X1, probs = tcf_10)
  # tau2_est <- quantile(X3, probs = 1 - tcf_30)
  tau2_est <- t2
  # our method
  ll_est <- empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                          tcf1 = tcf_10, tcf2 = tcf_20, tcf3 = tcf_30,
                          tau = c(tau1_est, tau2_est), type_F = type_F)
  return(ll_est)
}

tau0_1_mix <- c(qgamma(p = 0.8, shape = 6, rate = 12),
                qweibull(1 - 0.8, shape = 4, scale = 6.6))
tcf_10_1_mix <- 0.8
tcf_20_1_mix <- plnorm(tau0_1_mix[2], 1.5, 0.5) - plnorm(tau0_1_mix[1], 1.5, 0.5)
tcf_30_1_mix <- 0.8

out_mix_1_30 <- sapply(1:10000, function(i){
  simu_TCF23_mix(n1 = 20, n2 = 40, n3 = 30, para_gamma = c(6, 12),
                para_lnorm = c(1.5, 0.5), para_weib = c(4, 6.6),
                tcf_10 = tcf_10_1_mix, tcf_20 = tcf_20_1_mix,
                tcf_30 = tcf_30_1_mix, t2 = tau0_1_mix[2],
                type_F = "empi")
})

ll_adj_est_3 <- 2*log(2)*out_mix_1_30/median(out_mix_1_30)
mean(out_mix_1_30 <= qchisq(0.9, df = 2))
mean(ll_adj_est_3 <= qchisq(0.9, df = 2))

mean(out_mix_1_30 <= qchisq(0.95, df = 2))
mean(ll_adj_est_3 <= qchisq(0.95, df = 2))

mean(out_mix_1_30 <= qchisq(0.99, df = 2))
mean(ll_adj_est_3 <= qchisq(0.99, df = 2))

xx <- seq(0, 1, by = 0.001)
ll_qq_est_3 <- sapply(xx, function(x) quantile(out_mix_1_30, probs = x))
ll_adj_qq_est_3 <- sapply(xx, function(x) quantile(ll_adj_est_3, probs = x))
Chi2_2_qq <- sapply(xx, function(x) qchisq(p = x, df = 2))

# pdf(file = "simulations_3classes/qq_chisq_Norm_1_50.pdf", width = 5, height = 5)
plot(Chi2_2_qq, ll_qq_est_3, cex = 0.6, pch = 16, xlim = c(0, 15),
     ylim = c(0, 15),
     xlab = expression(paste("Quantile of ", chi[2]^2, " distribution")),
     ylab = "Monte Carlo quantile of statistic")
points(Chi2_2_qq, ll_adj_qq_est_3, cex = 0.6, pch = 16, col = "blue")
points(qchisq(p = c(0.9, 0.95, 0.99), df = 2),
       qchisq(p = c(0.9, 0.95, 0.99), df = 2), cex = 2, pch = 17)
abline(0, 1, col = "gray30", lty = 2)


tau0_2_mix <- c(qgamma(p = 0.8, shape = 6, rate = 12),
                qweibull(1 - 0.8, shape = 4, scale = 10))
tcf_10_2_mix <- 0.8
tcf_20_2_mix <- plnorm(tau0_2_mix[2], 1.5, 0.5) - plnorm(tau0_2_mix[1], 1.5, 0.5)
tcf_30_2_mix <- 0.8

out_mix_2_30 <- sapply(1:10000, function(i){
  simu_TCF23_mix(n1 = 30, n2 = 30, n3 = 30, para_gamma = c(6, 12),
                 para_lnorm = c(1.5, 0.5), para_weib = c(4, 10),
                 tcf_10 = tcf_10_2_mix, tcf_20 = tcf_20_2_mix,
                 tcf_30 = tcf_30_2_mix, t2 = tau0_2_mix[2],
                 type_F = "empi")
})

ll_adj_est_4 <- 2*log(2)*out_mix_2_30/median(out_mix_2_30)

mean(out_mix_2_30 <= qchisq(0.9, df = 2))
mean(ll_adj_est_4 <= qchisq(0.9, df = 2))

mean(out_mix_2_30 <= qchisq(0.95, df = 2))
mean(ll_adj_est_4 <= qchisq(0.95, df = 2))

mean(out_mix_2_30 <= qchisq(0.99, df = 2))
mean(ll_adj_est_4 <= qchisq(0.99, df = 2))

xx <- seq(0, 1, by = 0.001)
ll_qq_est_4 <- sapply(xx, function(x) quantile(out_mix_2_30, probs = x))
ll_adj_qq_est_4 <- sapply(xx, function(x) quantile(ll_adj_est_4, probs = x))
Chi2_2_qq <- sapply(xx, function(x) qchisq(p = x, df = 2))

# pdf(file = "simulations_3classes/qq_chisq_Norm_1_50.pdf", width = 5, height = 5)
plot(Chi2_2_qq, ll_qq_est_4, cex = 0.6, pch = 16, xlim = c(0, 15),
     ylim = c(0, 15),
     xlab = expression(paste("Quantile of ", chi[2]^2, " distribution")),
     ylab = "Monte Carlo quantile of statistic")
points(Chi2_2_qq, ll_adj_qq_est_4, cex = 0.6, pch = 16, col = "blue")
points(qchisq(p = c(0.9, 0.95, 0.99), df = 2),
       qchisq(p = c(0.9, 0.95, 0.99), df = 2), cex = 2, pch = 17)
abline(0, 1, col = "gray30", lty = 2)

#### ---- scaled chi-square 1 ----
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

simu_TCF20_norm <- function(n1, n2, n3, mu_true, sigma_true, tcf_10, tcf_20,
                            tcf_30, t2, type_F){
  # generate data
  flag_data <- 0
  while (flag_data == 0) {
    X1 <- rnorm(n1, mu_true[1], sigma_true[1])
    X2 <- rnorm(n2, mu_true[2], sigma_true[2])
    X3 <- rnorm(n3, mu_true[3], sigma_true[3])
    flag_data <- as.numeric((mean(X1) < mean(X2)) * (mean(X2) < mean(X3)))
  }
  tau1_est <- quantile(X1, probs = tcf_10)
  # tau2_est <- quantile(X3, probs = 1 - tcf_30)
  tau2_est <- t2
  # our method
  ll2_est <- empi_llike_C2(X2 = X2, n2 = n2, tcf2 = tcf_20,
                          tau = c(tau1_est, tau2_est), type_F = type_F)
  ll3_est <- empi_llike_C3(X3 = X3, n3 = n3, tcf3 = tcf_30,
                           tau = c(tau1_est, tau2_est), type_F = type_F)
  return(c(ll2_est, ll3_est))
}

tau0_1 <- c(qnorm(0.8, 0, 1), qnorm(1 - 0.8, 3.69, 1.2))
tcf_10_1 <- pnorm(tau0_1[1], 0, 1)
tcf_20_1 <- pnorm(tau0_1[2], 2.5, 1.1) - pnorm(tau0_1[1], 2.5, 1.1)
tcf_30_1 <- pnorm(tau0_1[2], 3.69, 1.2, lower.tail = FALSE)

out_norm_1_30_2 <- sapply(1:10000, function(i){
  simu_TCF20_norm(n1 = 30, n2 = 30, n3 = 30, mu_true = c(0, 2.5, 3.69),
                  sigma_true = c(1, 1.1, 1.2), tcf_10 = tcf_10_1,
                  tcf_20 = tcf_20_1, tcf_30 = tcf_30_1, t2 = tau0_1[2],
                  type_F = "empi")
})

ww <- (7/9)^3/median(out_norm_1_30_2[1,])
WW <- ww*rchisq(10000, 1) + rchisq(10000, 1)


ll_adj_est_C2_1 <- ww*out_norm_1_30_2[1,] + out_norm_1_30_2[2,]

mean(out_norm_1_30_2[1,] + out_norm_1_30_2[2,] <= qchisq(0.9, df = 2))
mean(ll_adj_est_C2_1 <= quantile(WW, probs = 0.9))

mean(out_norm_1_30_2[1,] + out_norm_1_30_2[2,] <= qchisq(0.95, df = 2))
mean(ll_adj_est_C2_1 <= quantile(WW, probs = 0.95))

mean(out_norm_1_30_2[1,] + out_norm_1_30_2[2,] <= qchisq(0.99, df = 2))
mean(ll_adj_est_C2_1 <= quantile(WW, probs = 0.99))


xx <- seq(0, 1, by = 0.001)
ll_qq_est_ww <- sapply(xx, function(x) {
  quantile(WW, probs = x)
  })
ll_adj_qq_est_C2_1 <- sapply(xx, function(x) quantile(ll_adj_est_C2_1, probs = x))

plot(ll_qq_est_ww, ll_adj_qq_est_C2_1, cex = 0.6, pch = 16, xlim = c(0, 15),
     ylim = c(0, 15),
     xlab = "Monte Carlo quantile of statistic",
     ylab = "Statistics")
abline(0, 1, col = "gray30", lty = 2)


Chi2_1_qq <- sapply(xx, function(x) qchisq(p = x, df = 1))

# pdf(file = "simulations_3classes/qq_chisq_Norm_1_50.pdf", width = 5, height = 5)
plot(Chi2_1_qq, ll_qq_est_6, cex = 0.6, pch = 16, xlim = c(0, 15),
     ylim = c(0, 15),
     xlab = expression(paste("Quantile of ", chi[1]^2, " distribution")),
     ylab = "Monte Carlo quantile of statistic")
points(Chi2_1_qq, ll_adj_qq_est_6, cex = 0.6, pch = 16, col = "blue")
points(qchisq(p = c(0.9, 0.95, 0.99), df = 1),
       qchisq(p = c(0.9, 0.95, 0.99), df = 1), cex = 2, pch = 17)
abline(0, 1, col = "gray30", lty = 2)



simu_TCF20_mix <- function(n1, n2, n3, para_gamma, para_lnorm, para_weib,
                           tcf_10, tcf_20, tcf_30, t2, type_F){
  # generate data
  flag_data <- 0
  while (flag_data == 0) {
    X1 <- rgamma(n1, shape = para_gamma[1], rate = para_gamma[2])
    X2 <- rlnorm(n2, meanlog = para_lnorm[1], sdlog = para_lnorm[2])
    X3 <- rweibull(n3, shape = para_weib[1], scale = para_weib[2])
    flag_data <- as.numeric((mean(X1) < mean(X2)) * (mean(X2) < mean(X3)))
  }
  # our method
  tau1_est <- quantile(X1, probs = tcf_10)
  # tau2_est <- quantile(X3, probs = 1 - tcf_30)
  tau2_est <- t2
  # our method
  ll2_est <- empi_llike_C2(X2 = X2, n2 = n2, tcf2 = tcf_20,
                           tau = c(tau1_est, tau2_est), type_F = type_F)
  ll3_est <- empi_llike_C3(X3 = X3, n3 = n3, tcf3 = tcf_30,
                           tau = c(tau1_est, tau2_est), type_F = type_F)
  return(c(ll2_est, ll3_est))
}

tau0_2_mix <- c(qgamma(p = 0.8, shape = 6, rate = 12),
                qweibull(1 - 0.8, shape = 4, scale = 10))
tcf_10_2_mix <- 0.8
tcf_20_2_mix <- plnorm(tau0_2_mix[2], 1.5, 0.5) - plnorm(tau0_2_mix[1], 1.5, 0.5)
tcf_30_2_mix <- 0.8

out_mix_2_30_2 <- sapply(1:10000, function(i){
  simu_TCF20_mix(n1 = 30, n2 = 30, n3 = 30, para_gamma = c(6, 12),
                 para_lnorm = c(1.5, 0.5), para_weib = c(4, 10),
                 tcf_10 = tcf_10_2_mix, tcf_20 = tcf_20_2_mix,
                 tcf_30 = tcf_30_2_mix, t2 = tau0_2_mix[2],
                 type_F = "empi")
})

ll_adj_est_6 <- (7/9)^3*out_mix_2_30_2[1,]/median(out_mix_2_30_2[1,]) +
  out_mix_2_30_2[2,]

mean(ll_adj_est_6 <= qchisq(0.9, df = 2))
mean(out_mix_2_30_2[1,] + out_mix_2_30_2[2,] <= qchisq(0.9, df = 2))

mean(ll_adj_est_6 <= qchisq(0.95, df = 2))
mean(out_mix_2_30_2[1,] + out_mix_2_30_2[2,] <= qchisq(0.95, df = 2))

mean(ll_adj_est_6 <= qchisq(0.99, df = 2))
mean(out_mix_2_30_2[1,] + out_mix_2_30_2[2,] <= qchisq(0.99, df = 2))

xx <- seq(0, 1, by = 0.001)
ll_qq_est_6 <- sapply(xx, function(x) quantile(out_mix_2_30_2, probs = x))
ll_adj_qq_est_6 <- sapply(xx, function(x) quantile(ll_adj_est_6, probs = x))
Chi2_1_qq <- sapply(xx, function(x) qchisq(p = x, df = 1))

# pdf(file = "simulations_3classes/qq_chisq_Norm_1_50.pdf", width = 5, height = 5)
plot(Chi2_1_qq, ll_qq_est_6, cex = 0.6, pch = 16, xlim = c(0, 15),
     ylim = c(0, 15),
     xlab = expression(paste("Quantile of ", chi[1]^2, " distribution")),
     ylab = "Monte Carlo quantile of statistic")
points(Chi2_1_qq, ll_adj_qq_est_6, cex = 0.6, pch = 16, col = "blue")
points(qchisq(p = c(0.9, 0.95, 0.99), df = 1),
       qchisq(p = c(0.9, 0.95, 0.99), df = 1), cex = 2, pch = 17)
abline(0, 1, col = "gray30", lty = 2)




