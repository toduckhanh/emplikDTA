ll_prob <- function(prob, prob_est, n) {
  res <- Inf
  if ((prob > 0 & prob < 1) & (prob_est > 0 & prob_est < 1)) {
    res <- 2 * n * (prob_est * log(prob_est/prob) +
                      (1 - prob_est) * log((1 - prob_est)/(1 - prob)))
  }
  return(res)
}