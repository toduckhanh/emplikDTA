.check_numeric <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
  if (length(x) == 0) {
    stop(sprintf("'%s' must not be empty", name), call. = FALSE)
  }
}

.check_ci <- function(ci_level) {
  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("'ci_level' must be in (0,1)", call. = FALSE)
  }
}
