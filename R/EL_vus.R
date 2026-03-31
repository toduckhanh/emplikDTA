#' @importFrom emplik el.test findUL

#' @export
ll_prob <- function(theta, theta_est, n) {
  res <- Inf
  if ((theta > 0 & theta < 1) & (theta_est > 0 & theta_est < 1)) {
    res <- 2 * n * (theta_est * log(theta_est/theta) +
                      (1 - theta_est) * log((1 - theta_est)/(1 - theta)))
  }
  return(res)
}

#' @export
bts_ll_vus <- function(X1, X2, X3, n1, n2, n3, vus_est, enlarged = TRUE, B) {
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
    vus_est_bts <- vus(X1.b, X2.b, X3.b, type = "ties")
    nn <- length(X1.b) + length(X2.b) + length(X3.b)
    res <- ll_prob(theta = vus_est, theta_est = vus_est_bts, n = nn)
    return(res)
  })
  empi_bts[is.na(empi_bts)] <- Inf
  r_est <- qchisq(0.5, df = 1) / median(empi_bts)
  return(r_est)
}

#' @export
EL_ci_vus <- function(X1, X2, X3, n1, n2, n3, vus_est, ci_level = 0.95,
                      enlarged = TRUE, B = 200, seed, plot = TRUE) {
  ###
  if (missing(seed)) seed <- 34
  set.seed(seed)
  r_est <- bts_ll_vus(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                      vus_est = vus_est, enlarged = enlarged, B = B)
  ##
  nn <- n1 + n2 + n3
  myfun <- function(theta, theta_est, r_adj, qc, n) {
    ll_est <- ll_prob(theta, theta_est, n)
    if (is.na(ll_est)) ll_est <- Inf
    ll_est_adj <- ll_est
    if (!is.infinite(ll_est)){
      ll_est_adj <- r_adj * ll_est_adj
    }
    return(ll_est_adj - qc)
  }
  LI_eng <- uniroot(f = myfun, interval = c(0, vus_est), theta_est = vus_est,
                    qc = qchisq(ci_level, 1), r_adj = r_est, n = nn)$root
  UI_eng <- uniroot(f = myfun, interval = c(vus_est, 1), theta_est = vus_est,
                    qc = qchisq(ci_level, 1), r_adj = r_est, n = nn)$root
  ci_vus <- c(LI_eng, UI_eng)
  ##
  if (plot) {
    x22 <- seq(0, 1, by = 0.001)
    ll2 <- sapply(x22, function(x) {
      myfun(x, theta_est = vus_est, r_adj = r_est,
            qc = qchisq(ci_level, 1), n = nn) + qchisq(ci_level, 1)
    })
    plot(x22, exp(-0.5*ll2), type = "l", xaxs = "i", yaxs = "i", xlim = c(0, 1),
         ylim = c(0, 1), xlab = "VUS", ylab = "Emprical likelihood ratio")
    abline(h = exp(-0.5*qchisq(ci_level, 1)), lty = 2)
    abline(v = ci_vus, lty = 2)
    points(x = vus_est, y = 0, pch = 16)
    abline(v = vus_est, lty = 2, col = "blue")
  }
  ## p-value
  ll_0 <- ll_prob(theta = 1/6, theta_est = vus_est, n = nn)
  p_val <- pchisq(r_est * ll_0, df = 1, lower.tail = FALSE)
  ##
  return(list(vus_est = vus_est, ci_vus = ci_vus, r_est = r_est,
              ll_0 = ll_0, p_val = p_val))
}

### ---- EL - placement value ----
#' @export
vus_var_EL <- function(x, y, z, vus_est){
  return(vusC_varEL(x, y, z) - vus_est^2)
}

#' @export
place_U <- function(x, y, z){
  return(place_U(x, y, z))
}

#' @export
ll_vus_place <- function(Ui, theta){
  # Wj <- Ui - theta
  # lamb_est <- suppressWarnings(BBsolve(par = 0, fn = lamb_func, W = Wj,
  #                                      method = 2, quiet = TRUE)$par)
  # term <- 1 + lamb_est * Wj
  # if (any(term <= 0)) res <- Inf
  # else res <- 2 * sum(log(term))
  # return(res)
  el.test(Ui, theta)$`-2LLR`
}

#' @export
EL_ci_vus_place <- function(X1, X2, X3, n1, n2, n3, vus_est, ci_level = 0.95,
                            plot = TRUE) {
  ###
  out_var <- vus_var_EL(X1, X2, X3, vus_est)
  r_est <- (out_var[2]/n2)/(out_var[1]/n1 + out_var[2]/n2 + out_var[3]/n3)
  Ui_est <- place_U(X1, X2, X3)
  ##
  myfun <- function(theta, Ui, r_adj) {
    ll_est <- el.test(Ui, theta)$`-2LLR` # ll_vus_place(Ui, theta)
    if (is.na(ll_est)) ll_est <- Inf
    ll_est_adj <- ll_est
    if (!is.infinite(ll_est)){
      ll_est_adj <- r_adj * ll_est_adj
    }
    return(list("-2LLR" = ll_est_adj))
  }
  res_ci <- findUL(step = 0.001, initStep = 0, fun = myfun, MLE = vus_est,
                   level = qchisq(ci_level, 1), Ui = Ui_est, r_adj = r_est)
  ci_vus <- c(res_ci$Low, res_ci$Up)
  ##
  if (plot) {
    x22 <- seq(0, 1, by = 0.001)
    ll2 <- sapply(x22, function(x) {
      myfun(x, Ui = Ui_est, r_adj = r_est)$"-2LLR"
    })
    plot(x22, exp(-0.5*ll2), type = "l", xaxs = "i", yaxs = "i", xlim = c(0, 1),
         ylim = c(0, 1), xlab = "VUS", ylab = "Emprical likelihood ratio")
    abline(h = exp(-0.5*qchisq(ci_level, 1)), lty = 2)
    abline(v = ci_vus, lty = 2)
    points(x = vus_est, y = 0, pch = 16)
    abline(v = vus_est, lty = 2, col = "blue")
  }
  return(list(vus_est = vus_est, ci_vus = ci_vus))
}

### ---- JEL method ----

#' @export
jack_U <- function(x, y, z, n1, n2, n3, vus_est, theta) {
  n <- n1 + n2 + n3
  var_term <- vusC_full_core(x, y, z)
  var_term_i <- sapply(1:n1, function(x) sum(var_term$ind1[-x]))
  var_term_j <- sapply(1:n2, function(x) sum(var_term$ind2[-x]))
  var_term_k <- sapply(1:n3, function(x) sum(var_term$ind3[-x]))
  Vi00 <- n1 * vus_est - var_term_i / (n2 * n3)
  V0j0 <- n2 * vus_est - var_term_j / (n1 * n3)
  V00k <- n3 * vus_est - var_term_k / (n1 * n2)
  Vi <- -2 * n * vus_est / (n - 3) + n * (n - 1) *
    c(Vi00/n1, V0j0/n2, V00k/n3) / (n - 3)
  EVi <- n * theta * c((n - 2 * n1 - 1) * rep(1, n1) / n1,
                       (n - 2 * n2 - 1) * rep(1, n2) / n2,
                       (n - 2 * n3 - 1) * rep(1, n3) / n3) / (n - 3)
  return(list(Vi = Vi, EVi = EVi))
}

#' @export
ll_vus_JEL <- function(X1, X2, X3, n1, n2, n3, vus_est, theta){
  jack_sample <- jack_U(x = X1, y = X2, z = X3, n1 = n1, n2 = n2, n3 = n3,
                        vus_est = vus_est, theta = theta)
  ll_est <- el.test(jack_sample$Vi - jack_sample$EVi, 0)$`-2LLR`
  return(ll_est)
}

#' @export
EL_ci_vus_JEL <- function(X1, X2, X3, n1, n2, n3, vus_est, ci_level = 0.95,
                          plot = TRUE) {
  ###
  myfun <- function(theta, X1, X2, X3, n1, n2, n3, vus_est) {
    jack_sample <- jack_U(x = X1, y = X2, z = X3, n1 = n1, n2 = n2, n3 = n3,
                          vus_est = vus_est, theta = theta)
    ll_est <- el.test(jack_sample$Vi - jack_sample$EVi, 0)$`-2LLR`
    return(list("-2LLR" = ll_est))
  }
  res_ci <- findUL(step = 0.001, initStep = 0, fun = myfun, MLE = vus_est,
                   level = qchisq(ci_level, 1), X1 = X1, X2 = X2, X3 = X3,
                   n1 = n1, n2 = n2, n3 = n3, vus_est = vus_est)
  ci_vus <- c(res_ci$Low, res_ci$Up)
  ##
  if (plot) {
    x22 <- seq(0, 1, by = 0.001)
    ll2 <- sapply(x22, function(x) {
      myfun(x, X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
            vus_est = vus_est)$"-2LLR"
    })
    plot(x22, exp(-0.5*ll2), type = "l", xaxs = "i", yaxs = "i", xlim = c(0, 1),
         ylim = c(0, 1), xlab = "VUS", ylab = "Emprical likelihood ratio")
    abline(h = exp(-0.5*qchisq(ci_level, 1)), lty = 2)
    abline(v = ci_vus, lty = 2)
    points(x = vus_est, y = 0, pch = 16)
    abline(v = vus_est, lty = 2, col = "blue")
  }
  return(list(vus_est = vus_est, ci_vus = ci_vus))
}
