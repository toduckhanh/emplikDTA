# empirical likelihood ratio function for probability ----
ll_prob <- function(theta, theta_est, n) {
  res <- Inf
  if ((theta > 0 & theta < 1) & (theta_est > 0 & theta_est < 1)) {
    res <- 2 * n * (theta_est * log(theta_est/theta) +
                      (1 - theta_est) * log((1 - theta_est)/(1 - theta)))
  }
  return(res)
}

# adjusted likelihood ratio function 
ll_prob_adj <- function(theta, theta_est, r_adj, qc, n) {
  ll_est <- ll_prob(theta, theta_est, n)
  if (is.na(ll_est)) ll_est <- Inf
  ll_est_adj <- ll_est
  if (!is.infinite(ll_est)){
    ll_est_adj <- r_adj * ll_est_adj
  }
  return(ll_est_adj - qc)
}
