## function for checking the numeric class 
.check_numeric <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
  if (length(x) == 0) {
    stop(sprintf("'%s' must not be empty", name), call. = FALSE)
  }
}

## function for checking the confidence level
.check_ci <- function(ci_level) {
  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("'ci_level' must be in (0,1)", call. = FALSE)
  }
}

## function for checking the order of classes
.check_levl_class <- function(trace, levl_class, temp_levl, n_class) {
  if (is.null(levl_class)) {
    if (trace) {
      cat("The ordered levels of classes are specified by the order of averages of the test values for each class:\n")
      cat(paste(temp_levl, collapse = " < "), "\n")
    }
    levl_class <- temp_levl
  } else {
    if (any(is.na(levl_class)) || !inherits(levl_class, "character") ||
        length(levl_class) != n_class) {
      stop(paste0("agrument diag_levels must be a character vector with length", n_class, "without NA."))
    }
    if (all(levl_class == temp_levl)) {
      if (trace) {
        cat("The user-defined orders are the same as the orders of averages of tests values for each class:\n")
        cat(paste(levl_class, collapse = " < "), "\n")
      }
    } else {
      if (trace) {
        cat("The user-defined orders are not the same as the orders of averages of tests values for each class\n")
        cat("The correct one should be:\n")
        cat(paste(temp_levl, collapse = " < "), "\n")
      }
      levl_class <- temp_levl
    }
  }
  return(list(levl_class = levl_class))
}

