
YI_kern <- function(x, y, h1, h2, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, h1, h2, type_ker){
    term1 <- cdf_kernel(x = tau, X = x, kernel_type = type_ker, bwd = h1)
    term2 <- cdf_kernel(x = tau, X = y, kernel_type = type_ker, bwd = h2)
    return(term1 - term2)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, upper = a2,
               control = list(fnscale = -1), x = x, y = y, h1 = h1,
               h2 = h2, type_ker = type_ker)$par
  return(out)
}

CtP_kern <- function(x, y, h1, h2, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, h1, h2, type_ker){
    F0 <- cdf_kernel(x = tau, X = x, kernel_type = type_ker, bwd = h1)
    F1 <- cdf_kernel(x = tau, X = y, kernel_type = type_ker, bwd = h2)
    res <- (1 - F0)^2 + F1^2
    return(res)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, upper = a2,
               x = x, y = y, h1 = h1, h2 = h2, type_ker = type_ker)$par
  return(out)
}

MA_kern <- function(x, y, h1, h2, type_ker, a0, a1, a2){
  ff <- function(tau, x, y, h1, h2, type_ker){
    F0 <- cdf_kernel(x = tau, X = x, kernel_type = type_ker, bwd = h1)
    F1 <- cdf_kernel(x = tau, X = y, kernel_type = type_ker, bwd = h2)
    res <- log(F0) + log(1 - F1)
    return(res)
  }
  out <- optim(a0, ff, method = "L-BFGS-B", lower = a1, upper = a2,
               control = list(fnscale = -1),
               x = x, y = y, h1 = h1, h2 = h2, type_ker = type_ker)$par
  return(out)
}


#'@export
threshold2 <- function(x, ...) {
  UseMethod("threshold2")
}

#' @exportS3Method
threshold2.default <- function(x, y, method = c("YI", "CtP", "MV", "all"),
                               kernel_type = c("gauss", "epan"), h_x = NULL, 
                               h_y = NULL, bwd = c("RT", "AZZ", "CV")){
  call <- match.call()
  .check_numeric(x, "x")
  .check_numeric(y, "y")
  method <- match.arg(method)
  kernel_type <- match.arg(kernel_type)
  bwd <- match.arg(bwd)
  if(is.null(h_x)){
    h_x <- switch(bwd_method,
                  RT = bwd_plugin(X = x, bwd_method = "RT"),
                  AZZ = bwd_plugin(X = x, bwd_method = "AZZ"),
                  CV = bwd_cv(X = x, kernel_type = kernel_type)
    )
  }
  if(is.null(h_y)){
    h_y <- switch(bwd_method,
                  RT = bwd_plugin(X = y, bwd_method = "RT"),
                  AZZ = bwd_plugin(X = y, bwd_method = "AZZ"),
                  CV = bwd_cv(X = y, kernel_type = kernel_type)
    )
  }
  a1 <- min(c(x, y))
  a2 <- max(c(x, y))
  a0 <- mean(mean(x), mean(y))
  out_thresh <- switch(method,
                       YI = YI_kern(x = x, y = y, h1 = h_x, h2 = h_y, 
                                    type_ker = kernel_type, a0 = a0, 
                                    a1 = a1, a2 = a2),
                       CtP = CtP_kern(x = x, y = y, h1 = h_x, h2 = h_y,
                                      type_ker = kernel_type, a0 = a0, 
                                      a1 = a1, a2 = a2),
                       MA = MA_kern(x = x, y = y, h1 = h_x, h2 = h_y,
                                    type_ker = kernel_type, a0 = a0, 
                                    a1 = a1, a2 = a2)
  )
  return(out_thresh)
}





