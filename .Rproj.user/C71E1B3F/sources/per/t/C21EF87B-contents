mu_true <- c(0, 1)
sigma_true <- c(1, 1.1)

n1 <- 15
n2 <- 15

X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])

data_test <- data.frame(biom = c(X1, X2), 
                        diag = rep(c("1", "2"), c(n1, n2)))
auc(x = X1, y = X2)

auc(formula = biom ~ diag, data = data_test)

auc(formula = biom ~ diag, data = data_test, ci_level = 0.9)

auc(formula = biom ~ diag, data = data_test, diag_levels = c("2", "1"))

out_test <- auc(formula = biom ~ diag, data = data_test)
print(out_test)

auc(formula = biom ~ diag, data = data_test, plot = TRUE)
