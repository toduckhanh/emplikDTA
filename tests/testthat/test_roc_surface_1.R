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

test_that("ROC_emp_surface_2 returns mesh data", {
  out <- ROC_emp_surface_2(X1, X2, X3, ncp = 10, plot = FALSE)

  expect_s3_class(out, "ROC_emp_surface")
  expect_true(is.matrix(out$vertices))
  expect_true(is.matrix(out$triangles))
  expect_equal(nrow(out$vertices), 4)
  expect_equal(ncol(out$triangles), 3)

  xyz <- t(out$vertices[1:3, ])
  area <- apply(out$triangles, 1, function(id) {
    v1 <- xyz[id[2], ] - xyz[id[1], ]
    v2 <- xyz[id[3], ] - xyz[id[1], ]
    cross_prod <- c(
      v1[2] * v2[3] - v1[3] * v2[2],
      v1[3] * v2[1] - v1[1] * v2[3],
      v1[1] * v2[2] - v1[2] * v2[1]
    )
    sqrt(sum(cross_prod^2)) / 2
  })
  expect_true(all(area > 0))
})
