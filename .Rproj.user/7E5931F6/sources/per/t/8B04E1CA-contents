## ---- plot ROC surface and ellipse confidence region for TCFs ----
#' @import rgl
#' @import misc3d
#' @export
tcfs_emp <- function(X1, X2, X3, tau) {
  tcf1 <- mean(X1 <= tau[1])
  tcf2 <- mean((X2 <= tau[2])*(X2 > tau[1]))
  tcf3 <- mean(X3 > tau[2])
  return(c(tcf1, tcf2, tcf3))
}

#' @export
ROC_emp_surface <- function(X1, X2, X3, ncp = 100, main, color = "gray40",
                            alpha = 0.5) {
  # , ellipsoid = FALSE, cpts = NULL, ci_level = 0.95) {
  cp <- c(-Inf, seq(min(c(X1, X2, X3)), max(c(X1, X2, X3)),
                    length.out = ncp - 2), Inf)
  cp1 <- rep(cp, seq(ncp - 1, 0, by = -1))
  cp2 <- c()
  for(i in 1:(ncp - 1)){
    cp2 <- c(cp2, cp[-c(1:i)])
  }
  cpoint <- cbind(cp1, cp2)
  ROCpoint <- t(apply(cpoint, 1, function(t) {
    tcfs_emp(X1 = X1, X2 = X2, X3 = X3, tau = t)
  }))
  # ROCpoint <- matrix(unlist(ROCpoint), ncol = 3, byrow = TRUE)
  colnames(ROCpoint) <- c("TCF1", "TCF2", "TCF3")
  rownames(ROCpoint) <- paste("(",round(cp1, 3),", " ,round(cp2, 3), ")",
                              sep = "")
  ct1 <- numeric(ncp - 1)
  for(i in 1:(ncp - 1)){
    ct1[i] <- i*ncp - i*(i + 1)/2
  }
  tcf1 <- matrix(ROCpoint[ct1, 1], ncp - 1, ncp - 1, byrow = FALSE)
  tcf3 <- matrix(ROCpoint[1:(ncp - 1), 3], ncp - 1, ncp - 1, byrow = TRUE)
  tcf2 <- matrix(0, nrow = ncp - 1, ncol = ncp - 1)
  tcf2[lower.tri(tcf2, diag = TRUE)] <- ROCpoint[, 2]
  tcf2 <- t(tcf2)
  res <- list()
  res$vals <- ROCpoint
  res$cpoint <- cpoint
  res$ncp <- ncp
  res$tcf1 <- tcf1
  res$tcf2 <- tcf2
  res$tcf3 <- tcf3
  ###
  open3d()
  my_user_matrix <- rbind(c(-0.8370321, -0.5446390, -0.0523976, 0),
                          c(0.1272045, -0.2868422, 0.9494949, 0),
                          c(-0.5321618, 0.7880925, 0.3093767, 0),
                          c(0, 0, 0, 1))
  par3d(windowRect = 50 + c(0, 0, 640, 640), userMatrix = my_user_matrix)
  if (missing(main)) {
    main <- "Empirical ROC surface"
  }
  plot3d(0, 0, 0, type = "n", box = FALSE, xlab = "", ylab = "", zlab = "",
         xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1), axes = FALSE)
  axes3d(edges = c("x--", "y--", "z--"))
  mtext3d("TCF 1", "x--", line = 2, at = 0.35)
  mtext3d("TCF 2", "z--", line = 4, at = 0.55)
  mtext3d("TCF 3", "y--", line = 4, at = 0.15, level = 2)
  bgplot3d({
    plot.new()
    title(main = main, line = 1)
  })
  surface3d(tcf1, tcf3, tcf2, col = color, alpha = alpha)
  # if (ellipsoid) {
  #   if (is.null(cpts)) stop("Need to specified pair of thresholds to plot the confidence region.")
  #   else {
  #     z1 <- seq(0, 1, length.out = 51)
  #     z2 <- seq(0, 1, length.out = 51)
  #     z3 <- seq(0, 1, length.out = 51)
  #     contour3d(f = function(x, y, z){
  #       empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = length(X1),
  #                     n2 = length(X2), n3 = length(X3), tcf1 = x, tcf2 = z,
  #                     tcf3 = y, tau = cpts, type_F = "empi")
  #     }, level = qchisq(ci_level, 3), x = z1, y = z3, z = z2, draw = TRUE,
  #     add = TRUE, color2 = "blue", smooth = "standard", alpha = 0.5,
  #     fill = FALSE)
  #     tcf_orgi <- tcfs_emp(X1 = X1, X2 = X2, X3 = X3, tau = cpts)
  #     plot3d(tcf_orgi[1], tcf_orgi[3], tcf_orgi[2], type = "s", col = "red",
  #            radius = 0.01, add = TRUE)
  #   }
  # }
  invisible(res)
}

#' @export
CR_emp_tcfs <- function(X1, X2, X3, cpts = NULL, ci_level = 0.95,
                        color1 = "red", color2 = "blue", smooth = 0,
                        alpha = 0.5, fill = FALSE) {
  if (is.null(cpts)) stop("Need to specified pair of thresholds to plot the confidence region.")
  else {
    z1 <- seq(0, 1, length.out = 51)
    z2 <- seq(0, 1, length.out = 51)
    z3 <- seq(0, 1, length.out = 51)
    contour3d(f = function(x, y, z){
      empi_llike_3C(X1 = X1, X2 = X2, X3 = X3, n1 = length(X1),
                    n2 = length(X2), n3 = length(X3), tcf1 = x, tcf2 = z,
                    tcf3 = y, tau = cpts, type_F = "empi")
    }, level = qchisq(ci_level, 3), x = z1, y = z3, z = z2, draw = TRUE,
    add = TRUE, color2 = color2, smooth = smooth, alpha = alpha, fill = fill)
    tcf_orgi <- tcfs_emp(X1 = X1, X2 = X2, X3 = X3, tau = cpts)
    plot3d(tcf_orgi[1], tcf_orgi[3], tcf_orgi[2], type = "s", col = color1,
           radius = 0.01, add = TRUE)
  }
}
