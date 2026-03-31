#' @importFrom Rcpp evalCpp
#' @useDynLib emplikDTA, .registration = TRUE

auc_core <- function(x, y){
  if (any(is.na(x)) | any(is.na(y))) return(NA)
  else {
    res <- aucC_ties(x, y)
    return(res)
  }
}

vus_core <- function(x, y, z){
  if (any(is.na(x)) | any(is.na(y)) | any(is.na(z))) return(NA)
  else {
    res <- vusC_ties(x, y, z)
    return(res)
  }
}
