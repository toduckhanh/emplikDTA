mu_true <- c(0, 2.5, 3.69)
sigma_true <- c(1, 1.1, 1.2)

n1 <- 15
n2 <- 15
n3 <- 15

X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

data_test <- data.frame(biom = c(X1, X2, X3), 
                        diag = rep(c("1", "2", "3"), c(n1, n2, n3)))

ROC_emp_surface(data_test$X1, data_test$X2, data_test$X3)

