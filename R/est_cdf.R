#' @importFrom Rcpp evalCpp
#' @useDynLib emplikDTA, .registration = TRUE


cdf_kernel <- function(x, X, kernel_type = c("gauss", "epan"), bwd){
  if(any(is.na(x)) | any(is.na(X))) return(NA)
  kernel_type <- match.arg(kernel_type)
  kernel_type_value <- switch(kernel_type,
                              "gauss" = 0,
                              "epan" = 1)
  res <- cdf_kernel_C(x, X, kernel_type_value, bwd)
  return(res)
}

bwd_plugin <- function(X, bwd_method = c("RT", "AZZ"), hc = NULL){
  bwd_method <- match.arg(bwd_method)
  if(bwd_method %in% c("RT", "AZZ") && is.null(hc)) {
    hc <- switch (bwd_method,
                  RT = 0.9,
                  AZZ = 1.3
    )
  }
  bwd <- switch(bwd_method,
                RT = hc*min(c(sd(X), IQR(X)/1.34))*length(X)^(-1/5),
                AZZ = hc*sd(X)*length(X)^(-1/3)
  )
  return(bwd)
}

bwd_cv <- function(X, kernel_type = c("gauss", "epan"), n_bwd = 101, n_x = 151){
  if(any(is.na(X))) return(NA)
  kernel_type <- match.arg(kernel_type)
  kernel_type_value <- switch(kernel_type,
                              "gauss" = 0,
                              "epan" = 1)
  x_grid <- seq(min(X), max(X), length.out = n_x)
  hx <- diff(x_grid)[1]
  range_X <- max(X) - min(X)
  bwd_seq <- seq(range_X/200, range_X/2, length.out = n_bwd)
  res <- cv_bwd_C(x = x_grid, X = X, Ktype = kernel_type_value, hx = hx,
                  bwd_seq = bwd_seq)
  return(res)
}
