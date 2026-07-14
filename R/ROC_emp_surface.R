## ---- plot ROC surface and ellipse confidence region for TCFs ----
#' @import rgl
#' @import misc3d

tcfs_emp <- function(x, y, z, tau) {
  c(TCF1 = mean(x <= tau[1]),
    TCF2 = mean(y <= tau[2] & y > tau[1]),
    TCF3 = mean(z > tau[2])
  )
}

.check_roc_surface_data <- function(x, y, z, ncp) {
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  .check_numeric(z, "z")
  if (any(!is.finite(x))) {
    stop("'x' must contain only finite values", call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop("'y' must contain only finite values", call. = FALSE)
  }
  if (any(!is.finite(z))) {
    stop("'z' must contain only finite values", call. = FALSE)
  }
  if (!is.numeric(ncp) || length(ncp) != 1L || is.na(ncp) ||
      ncp < 3 || ncp != floor(ncp)) {
    stop("'ncp' must be a single integer greater than or equal to 3",
         call. = FALSE)
  }
  as.integer(ncp)
}

.roc_surface_cutpoints <- function(x, y, z, ncp) {
  values <- c(x, y, z)
  c(-Inf, seq(min(values), max(values), length.out = ncp - 2), Inf)
}

.roc_surface_threshold_pairs <- function(cutpoints) {
  ncp <- length(cutpoints)
  cp1 <- rep(cutpoints, seq(ncp - 1, 0, by = -1))
  cp2 <- unlist(
    lapply(seq_len(ncp - 1), function(i) cutpoints[-seq_len(i)]),
    use.names = FALSE
  )
  cbind(cp1, cp2)
}

.roc_surface_points <- function(x, y, z, cpoint) {
  ROCpoint <- t(vapply(seq_len(nrow(cpoint)), function(i) {
    tcfs_emp(x = x, y = y, z = z, tau = cpoint[i, ])
  }, numeric(3)))
  colnames(ROCpoint) <- c("TCF1", "TCF2", "TCF3")
  rownames(ROCpoint) <- paste(
    "(", round(cpoint[, 1], 3), ", ", round(cpoint[, 2], 3), ")",
    sep = ""
  )
  ROCpoint
}

.roc_surface_grids <- function(ROCpoint, ncp) {
  n_grid <- ncp - 1L
  ct1 <- vapply(seq_len(n_grid), function(i) {
    i * ncp - i * (i + 1) / 2
  }, numeric(1))
  tcf1 <- matrix(ROCpoint[ct1, 1], n_grid, n_grid, byrow = FALSE)
  tcf3 <- matrix(ROCpoint[seq_len(n_grid), 3], n_grid, n_grid, byrow = TRUE)
  tcf2 <- matrix(0, nrow = n_grid, ncol = n_grid)
  tcf2[lower.tri(tcf2, diag = TRUE)] <- ROCpoint[, 2]
  list(
    tcf1 = tcf1,
    tcf2 = t(tcf2),
    tcf3 = tcf3
  )
}

.new_ROC_emp_surface <- function(x, y, z, ncp = 150, call = NULL) {
  ncp <- .check_roc_surface_data(x, y, z, ncp)
  cutpoints <- .roc_surface_cutpoints(x, y, z, ncp)
  cpoint <- .roc_surface_threshold_pairs(cutpoints)
  ROCpoint <- .roc_surface_points(x, y, z, cpoint)
  grids <- .roc_surface_grids(ROCpoint, ncp)
  structure(
    c(
      list(
        vals = ROCpoint,
        cpoint = cpoint,
        cutpoints = cutpoints,
        ncp = ncp,
        n = c(x = length(x), y = length(y), z = length(z)),
        call = call
      ),
      grids
    ),
    class = c("ROC_emp_surface", "roc_emp_surface")
  )
}

.roc_surface_default_matrix <- function() {
  rbind(
    c(-0.8370321, -0.5446390, -0.0523976, 0),
    c(0.1272045, -0.2868422, 0.9494949, 0),
    c(-0.5321618, 0.7880925, 0.3093767, 0),
    c(0, 0, 0, 1)
  )
}

.open_roc_surface_scene <- function() {
  open3d(antialias = 8)
  par3d(
    windowRect = 50 + c(0, 0, 1250, 1250),
    userMatrix = .roc_surface_default_matrix()
  )
}

.draw_roc_surface_frame <- function(main = "Empirical ROC surface") {
  plot3d(0, 0, 0, type = "n", box = FALSE, xlab = "", ylab = "", zlab = "",
    xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1), axes = FALSE)
  axes3d(edges = c("x--", "y--", "z--"), cex = 1.4, lwd = 2)
  mtext3d("TCF 1", "x--", line = 2, at = 0.35)
  mtext3d("TCF 2", "z--", line = 4, at = 0.55)
  mtext3d("TCF 3", "y--", line = 4, at = 0.15, level = 2)
  if (!is.null(main)) {
    bgplot3d({
      plot.new()
      title(main = main, line = 1)
    })
  }
}

.draw_roc_surface <- function(x, color = "steelblue", alpha = 0.5) {
  surface3d(x$tcf1, x$tcf3, x$tcf2, col = color, alpha = alpha)
}

#' @export
ROC_emp_surface <- function(x, y, z, ncp = 150, main, color = "steelblue",
                            alpha = 0.5, plot = TRUE) {
  object <- .new_ROC_emp_surface(x = x, y = y, z = z,
    ncp = ncp, call = match.call()
  )
  if (plot) {
    main <- if (missing(main)) "Empirical ROC surface" else main
    plot(object, type = "surface", main = main, color = color, alpha = alpha)
  }
  invisible(object)
}

#' @export
plot.ROC_emp_surface <- function(x, main = "Empirical ROC surface",
                                 color = "steelblue", alpha = 0.5,
                                 new_window = TRUE, add = FALSE, ...) {
  if (new_window && !add) {
    .open_roc_surface_scene()
  }
  if (!add) {
    .draw_roc_surface_frame(main = main)
  }
  .draw_roc_surface(x, color = color, alpha = alpha)
  light3d()
  invisible(x)
}

#' @export
print.ROC_emp_surface <- function(x, ...) {
  cat("Empirical ROC surface\n")
  cat("Cut points:", x$ncp, "\n")
  cat("Threshold pairs:", nrow(x$cpoint), "\n")
  cat("Sample sizes:", paste(names(x$n), x$n, sep = " = ", collapse = ", "),
      "\n")
  invisible(x)
}

#' @export
CR_emp_tcfs <- function(x, y, z, cpts = NULL, ci_level = 0.95,
                        color1 = "red", color2 = "blue", smooth = 0,
                        alpha = 0.5, fill = FALSE) {
  if (is.null(cpts)) {
    stop("Need to specified pair of thresholds to plot the confidence region.")
  } else {
    X1 <- x
    X2 <- y
    X3 <- z
    z1 <- seq(0, 1, length.out = 51)
    z2 <- seq(0, 1, length.out = 51)
    z3 <- seq(0, 1, length.out = 51)
    contour3d(f = function(x, y, z){
      empi_llike_3C(x = X1, y = X2, z = X3, n1 = length(X1), n2 = length(X2), 
                    n3 = length(X3), tcf1 = x, tcf2 = z, tcf3 = y, 
                    tau = cpts, type_F = "empi")
    }, level = qchisq(ci_level, 3), x = z1, y = z3, z = z2, draw = TRUE,
    add = TRUE, color2 = color2, smooth = smooth, alpha = alpha, fill = fill)
    tcf_orgi <- tcfs_emp(x = x, y = y, z = z, tau = cpts)
    plot3d(tcf_orgi[1], tcf_orgi[3], tcf_orgi[2], type = "s", col = color1,
           radius = 0.01, add = TRUE)
  }
}
