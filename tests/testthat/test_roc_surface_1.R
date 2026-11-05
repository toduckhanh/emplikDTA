set.seed(34)

mu_true <- c(0, 2.5, 3.69)
sigma_true <- c(1, 1.1, 1.2)

n1 <- 15
n2 <- 15
n3 <- 15

X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

test_that("ROC_emp_surface returns an empirical ROC surface object", {
  out <- ROC_emp_surface(X1, X2, X3, ncp = 10, plot = FALSE)

  expect_s3_class(out, "ROC_emp_surface")
  expect_equal(out$ncp, 10)
  expect_equal(nrow(out$vals), 45)
  expect_equal(ncol(out$vals), 3)
  expect_equal(dim(out$tcf1), c(9, 9))
  expect_equal(dim(out$tcf2), c(9, 9))
  expect_equal(dim(out$tcf3), c(9, 9))
})

test_that("CR_tcfs adds a region to the fitted ROC surface", {
  out <- ROC_emp_surface(1:10, 3:12, 5:14, ncp = 10)
  on.exit(rgl::close3d())

  expect_identical(CR_tcfs(out, cpts = c(5, 9), fill = TRUE), out)
})
