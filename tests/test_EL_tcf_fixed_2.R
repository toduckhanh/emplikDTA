simu_TCF23_norm <- function(n1, n2, n3, mu_true, sigma_true, tcf_10, tcf_20,
                            tcf_30, t2, type_F, enlarged, B){
  # generate data
  flag_data <- 0
  while (flag_data == 0) {
    X1 <- rnorm(n1, mu_true[1], sigma_true[1])
    X2 <- rnorm(n2, mu_true[2], sigma_true[2])
    X3 <- rnorm(n3, mu_true[3], sigma_true[3])
    tau1_est <- quantile(X1, probs = tcf_10)
    flag_data <- as.numeric((mean(X1) < mean(X2)) * (mean(X2) < mean(X3)) *
                              (tau1_est < t2))
  }
  # print(tcf2_est)
  # our method
  # ll2_est <- empi_llike_C2(X2 = X2, n2 = n2, tcf2 = tcf_20,
  #                          tau = c(tau1_est, t2), type_F = type_F)
  # ll3_est <- empi_llike_C3(X3 = X3, n3 = n3, tcf3 = tcf_30,
  #                          tau = c(tau1_est, t2), type_F = type_F)
  ll_est <- empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                          tcf1 = tcf_10, tcf2 = tcf_20, tcf3 = tcf_30,
                          tau = c(tau1_est, t2), type_F = type_F)
  # ll_est <- r_est <- Inf
  r_est <- Inf
  if (is.finite(ll_est)) { #(is.finite(ll2_est) & is.finite(ll3_est)) {
    tcf2_est <- mean(X2 <= t2) - mean(X2 <= tau1_est)
    r_est <- bts_func_C2_1(X1 = X1, X2 = X2, n1 = n1, n2 = n2, tcf1 = tcf_10,
                           tcf2 = tcf2_est, t2 = t2, enlarged = enlarged, B = B,
                           type_F = "Adi_ties")
  }
  return(c(ll_est, r_est))
}

tau0_1 <- c(qnorm(0.8, 0, 1), qnorm(1 - 0.8, 3.69, 1.2))
tcf_10_1 <- pnorm(tau0_1[1], 0, 1)
tcf_20_1 <- pnorm(tau0_1[2], 2.5, 1.1) - pnorm(tau0_1[1], 2.5, 1.1)
tcf_30_1 <- pnorm(tau0_1[2], 3.69, 1.2, lower.tail = FALSE)

out_norm_1_30_2 <- sapply(1:1000, function(i){
  simu_TCF23_norm(n1 = 50, n2 = 50, n3 = 50, mu_true = c(0, 2.5, 3.69),
                  sigma_true = c(1, 1.1, 1.2), tcf_10 = tcf_10_1,
                  tcf_20 = tcf_20_1, tcf_30 = tcf_30_1, t2 = tau0_1[2],
                  type_F = "empi", enlarged = TRUE, B = 200)
})

set.seed(235)
WW1 <- rchisq(1000, df = 1)
WW2 <- rchisq(1000, df = 1)

Qw_MC <- function(w, WW1, WW2) {
  my_fun <- function(x, WW1, WW2) {
    if (is.infinite(x)) return(rep(0, 3))
    else {
      return(quantile(x * WW1 + WW2, probs = c(0.9, 0.95, 0.99), names = FALSE))
    }
  }
  v_my_fun <- Vectorize(my_fun, vectorize.args = "x")
  return(v_my_fun(w, WW1, WW2))
}

system.time({
  Qw_MC(out_norm_1_30_2[2, 1:1000], WW1, WW2)
})

system.time({
  qq_w <- sapply(out_norm_1_30_2[2,], function(x) {
    if (is.infinite(x)) return(rep(0, 3))
    else {
      return(quantile((1/x) * WW1 + WW2, probs = c(0.9, 0.95, 0.99), names = FALSE))
    }
  })
  mean(out_norm_1_30_2[1, ] <= qq_w[1,])
  mean(out_norm_1_30_2[1, ] <= qq_w[2,])
  mean(out_norm_1_30_2[1, ] <= qq_w[3,])
})

system.time({
  rowMeans(apply(out_norm_1_30_2, 2, function(x) {
    if (is.infinite(x[1])) return(rep(0, 3))
    else {
      x[1] <= quantile(x[2] * WW1 + WW2, probs = c(0.9, 0.95, 0.99),
                       names = FALSE)
    }
  }))
})


