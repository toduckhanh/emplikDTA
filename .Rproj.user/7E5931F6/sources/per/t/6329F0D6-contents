#' @export
auc <- function(x, y, type = c("Ustat", "ties")){
  type <- match.arg(type)
  if (any(is.na(x)) | any(is.na(y))) return(NA)
  else {
    res <- switch(type,
                  Ustat = auc_U_C(x, y),
                  ties = auc_ties_C(x, y))
    return(res)
  }
}

