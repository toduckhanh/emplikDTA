vus_normal <- function(mu, sigma){
  a <- sigma[2]/sigma[1]
  b <- (mu[1] - mu[2])/sigma[1]
  c <- sigma[2]/sigma[3]
  d <- (mu[3] - mu[2])/sigma[3]
  return(integrate(function(x, a, b, c, d) {
    pnorm(a*x - b)*pnorm(-c*x + d)*dnorm(x)
    }, a = a, b = b, c = c, d = d, lower = -Inf, upper = Inf)$value)
}

simu_vus_norm <- function(n1, n2, n3, mu_true, sigma_true, vus_true) {
  # generate data
  flag_data <- 0
  while (flag_data == 0) {
    X1 <- rnorm(n1, mu_true[1], sigma_true[1])
    X2 <- rnorm(n2, mu_true[2], sigma_true[2])
    X3 <- rnorm(n3, mu_true[3], sigma_true[3])
    flag_data <- as.numeric((mean(X1) < mean(X2)) * (mean(X2) < mean(X3)))
  }
  vus_est <- vus(X1, X2, X3, type = "Ustat")
  # EL with placement values
  out_var <- vus_var_EL(X1, X2, X3, vus_est)
  r_est <- (out_var[2]/n2)/(out_var[1]/n1 + out_var[2]/n2 + out_var[3]/n3)
  Ui_est <- place_U(X1, X2, X3)
  ll_est <- ll_vus_place(Ui_est, vus_true)
  # EL method
  n <- n1 + n2 + n3
  ll_est_EL <- ll_prob(vus_true, vus_est, n)
  r_est_B <- bts_ll_vus(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                        vus_est = vus_est, enlarged = FALSE, B = 200)
  r_est_B2 <- bts_ll_vus(X1 = X1, X2 = X2, X3 = X3, n1 = n1, n2 = n2, n3 = n3,
                         vus_est = vus_est, enlarged = TRUE, B = 200)
  ## JEL method
  ll_est_JEL <- ll_vus_JEL(X1, X2, X3, n1, n2, n3, vus_est = vus_est,
                           theta = vus_true)
  ##
  return(c(vus_est, ll_est * r_est, ll_est_EL * r_est_B, ll_est_EL * r_est_B2,
           ll_est_JEL))
}

# vus_true <- vus_normal(mu = c(1, 2, 3), sigma = c(1, 1, 1))
vus_true <- vus_normal(mu = c(0, 3.5, 5.5), sigma = c(1, 1.1, 1.2))


library(doParallel)

ncpus <- 8
cl <- makeCluster(rep("localhost", ncpus), type = "PSOCK")
registerDoParallel(cl)

clusterEvalQ(cl, list(library(emplikROCS), library(KernSmooth),
                      library(rootSolve), library(BB), library(emplik)))
clusterExport(cl, list("simu_vus_norm", "vus_true"))

out_vus <- parSapply(cl, 1:2000, FUN = function(i) {
  simu_vus_norm(n1 = 50, n2 = 50, n3 = 50, mu_true = c(0, 3.5, 5.5),
                sigma_true = c(1, 1.1, 1.2), vus_true)
})

mean(out_vus[1,])

out_vus[is.na(out_vus)] <- Inf

mean(out_vus[2,] <= qchisq(0.90, 1))
mean(out_vus[2,] <= qchisq(0.95, 1))
mean(out_vus[2,] <= qchisq(0.99, 1))

mean(out_vus[3,] <= qchisq(0.90, 1))
mean(out_vus[3,] <= qchisq(0.95, 1))
mean(out_vus[3,] <= qchisq(0.99, 1))

mean(out_vus[4,] <= qchisq(0.90, 1))
mean(out_vus[4,] <= qchisq(0.95, 1))
mean(out_vus[4,] <= qchisq(0.99, 1))

mean(out_vus[5,] <= qchisq(0.90, 1))
mean(out_vus[5,] <= qchisq(0.95, 1))
mean(out_vus[5,] <= qchisq(0.99, 1))

qqplot(qchisq(ppoints(2000), df = 1), out_vus[5,], xlim = c(0, 14),
       ylim = c(0, 14))
qqline(out_vus[5,], distribution = function(p) qchisq(p, df = 1),
       probs = c(0.1, 0.6), col = 2)

qqplot(qchisq(ppoints(2000), df = 1), out_vus[3,], xlim = c(0, 14),
       ylim = c(0, 14))
qqline(out_vus[3,], distribution = function(p) qchisq(p, df = 1),
       probs = c(0.1, 0.6), col = 2)

qqplot(qchisq(ppoints(2000), df = 1), out_vus[2,], xlim = c(0, 14),
       ylim = c(0, 14))
qqline(out_vus[2,], distribution = function(p) qchisq(p, df = 1),
       probs = c(0.1, 0.6), col = 2)


n1 <- 35
n2 <- 40
n3 <- 45

mu_true <- c(0, 1, 1)
sigma_true <- c(1, 1, 2)

X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

vus_est_1 <- vus(X1, X2, X3, type = "Ustat")

EL_ci_vus(X1, X2, X3, n1, n2, n3, vus_est_1, ci_level = 0.95, enlarged = FALSE,
          B = 200, plot = TRUE)

EL_ci_vus_JEL(X1, X2, X3, n1, n2, n3, vus_est_1, ci_level = 0.95, plot = TRUE)

EL_ci_vus_place(X1, X2, X3, n1, n2, n3, vus_est_1, ci_level = 0.95, plot = TRUE)



uu1 <- jack_U(X1, X2, X3, n1, n2, n3, vus_est = vus_est_1, theta = 0.39)
mean(uu1$Vi)
mean(uu1$EVi)

myfun <- function(theta, Ui, r_adj) {
  ll_est <- emplik::el.test(Ui, theta)$`-2LLR`
  if (is.na(ll_est)) ll_est <- Inf
  ll_est_adj <- ll_est
  if (!is.infinite(ll_est)){
    ll_est_adj <- r_adj * ll_est_adj
  }
  return(list("-2LLR" = ll_est_adj))
}

out_var <- vus_var_EL(X1, X2, X3, vus_est_1)
r_est <- (out_var[2]/n2)/(out_var[1]/n1 + out_var[2]/n2 + out_var[3]/n3)
Ui_est <- place_U(X1, X2, X3)

emplik::findUL(step = 0.001, initStep = 0, fun = myfun, vus_est_1,
               level = qchisq(0.95, 1), Ui = Ui_est, r_adj = r_est)

myfun2 <- function(theta, X1, X2, X3, n1, n2, n3, vus_est) {
  uu1 <- jack_U(X1, X2, X3, n1, n2, n3, vus_est, theta = theta)
  ll_est <- emplik::el.test(uu1$Vi - uu1$EVi, 0)$`-2LLR`
  return(list("-2LLR" = ll_est))
}

emplik::findUL(step = 0.001, initStep = 0, fun = myfun2, vus_est_1,
               level = qchisq(0.95, 1), X1 = X1, X2 = X2, X3 = X3, n1 = n1,
               n2 = n2, n3 = n3, vus_est = vus_est_1)

lamb_func <- function(par, W){
  sum(W / (1 + par * W))
}

lamb_func_deriv <- function(par, W){
  -1 * sum(W^2 / (1 + par * W)^2)
}

vv <- sapply(seq(2, 8, by = 0.01), function (x) lamb_func(x, Ui_est - 0.3))
plot(seq(2, 8, by = 0.01), vv, type = "l")


zz <- rep(0, 10)
for (i in 1:9){
  zz[i + 1] <- zz[i] - lamb_func(zz[i], Ui_est - 0.3) / lamb_func_deriv(zz[i], Ui_est - 0.3)
}
zz
