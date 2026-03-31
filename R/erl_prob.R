# empirical likelihood ratio function for probability ----
ll_prob <- function(theta, theta_est, n) {
  res <- Inf
  if ((theta > 0 & theta < 1) & (theta_est > 0 & theta_est < 1)) {
    res <- 2 * n * (theta_est * log(theta_est/theta) +
                      (1 - theta_est) * log((1 - theta_est)/(1 - theta)))
  }
  return(res)
}
