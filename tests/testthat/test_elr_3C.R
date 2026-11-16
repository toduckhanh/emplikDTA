test_that("three-class EL uses deviance limits at both middle-group boundaries", {
  x <- z <- c(-2, 3)
  y <- c(0, 0, 1, 2)
  intervals <- list(empty = c(0.1, 0.9), below = c(-1.5, -1),
                    full = c(-1, 2.5))
  fractions <- c(empty = 0, below = 0, full = 1)
  target <- 0.3

  for (name in names(intervals)) {
    obj <- empi_llike3_prepare(x, y, z, intervals[[name]])
    p <- fractions[[name]]
    expected <- -2 * length(y) * if (p == 0) log1p(-target) else log(target)

    expect_equal(obj$F2, p)
    expect_equal(obj$n2, length(y))
    expect_equal(empi_llike_3C(obj, 0.5, target, 0.5), expected)
    expect_equal(empi_llike_3C(obj, 0.5, c(p, 1 - p), 0.5), c(0, Inf))
  }
})

test_that("three-class EL preserves interior fractions and coincident thresholds", {
  x <- z <- c(-2, 3)
  y <- c(0, 0, 1, 2)
  interior <- empi_llike3_prepare(x, y, z, c(0.1, 1))
  coincident <- empi_llike3_prepare(x, y, z, c(2, 2))

  expect_equal(interior$F2, 0.25)
  expect_equal(coincident$F2, 0)
  expect_identical(empi_llike_3C(coincident, 0.5, 0.3, 0.5), Inf)
  expect_equal(empi_llike_3C(coincident, 0.5, 0, 0.5), 0)
  expect_null(empi_llike3_prepare(x, y, z, c(2, 1)))
})

test_that("outer groups use finite deviance limits without support checks", {
  obj <- empi_llike3_prepare(c(1, 2), c(-2, 0, 2), c(-2, -1), c(-1, 1))
  expect_equal(c(obj$F1, obj$F2, obj$F3), c(0, 1/3, 1))
  expect_equal(empi_llike_3C(obj, 0.2, 1/3, 0.3),
               -4 * (log1p(-0.2) + log1p(-0.3)))
  expect_equal(empi_llike_3C(obj, c(0, 1), 1/3, 0), c(0, Inf))
})

test_that("fixed bootstrap uses half-count references and raw deviance in all groups", {
  obj <- empi_llike3_prepare(c(1, 2), c(1, 2), c(-2, -1), c(-1, 0))
  draws <- rbind(c(0.1, 0.1, 0.1), c(0.99, 0.99, 0.99))
  reference <- 0.5 / 3
  expected <- -12 * c(log1p(-reference), log(reference))
  expect_equal(bts_fixed_tcfs(obj, draws), expected)
})
