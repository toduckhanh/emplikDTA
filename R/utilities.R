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
    stop("'ci_level' must be in (0, 1)", call. = FALSE)
  }
}

## function for checking the order of classes
# .check_levl_class <- function(trace, levl_class, temp_levl, n_class) {
#   if (is.null(levl_class)) {
#     if (trace) {
#       cat("The ordered levels of classes are specified by the order of averages of the test values for each class:\n")
#       cat(paste(temp_levl, collapse = " < "), "\n")
#     }
#     levl_class <- temp_levl
#   } else {
#     if (any(is.na(levl_class)) || !inherits(levl_class, "character") ||
#         length(levl_class) != n_class) {
#       stop(paste0("agrument diag_levels must be a character vector with length",
#                   n_class, "without NA."))
#     }
#     if (all(levl_class == temp_levl)) {
#       if (trace) {
#         cat("The user-defined orders are the same as the orders of averages of tests values for each class:\n")
#         cat(paste(levl_class, collapse = " < "), "\n")
#       }
#     } else {
#       if (trace) {
#         cat("The user-defined orders are not the same as the orders of averages of tests values for each class\n")
#         cat("The correct one should be:\n")
#         cat(paste(temp_levl, collapse = " < "), "\n")
#       }
#       levl_class <- temp_levl
#     }
#   }
#   return(list(levl_class = levl_class))
# }

.check_levl_class <- function(diag_levels = NULL, original_levels,
                              mean_levels, n_class = 3) {
  if (length(original_levels) != n_class) {
    stop(sprintf("diagnostic group must have exactly %d levels", n_class),
         call. = FALSE)
  }
  if (!is.null(diag_levels)) {
    if (length(diag_levels) != n_class) {
      stop(sprintf("'diag_levels' must contain %d group names.", n_class),
           call. = FALSE)
    }
    if (!all(diag_levels %in% original_levels)) {
      stop("'diag_levels' contains unknown group labels.",
           call. = FALSE)
    }
    levl_class <- diag_levels
  } else {
    levl_class <- original_levels
  }
  list(levl_class = levl_class, original_levels = original_levels,
       mean_levels = mean_levels, 
       mean_order_ok = identical(original_levels, mean_levels))
}


##
.extract_formula_data <- function(formula, data, diag_levels = NULL,
                                  subset, na.action, n_class = 3, 
                                  trace = TRUE) {
  if (missing(data)) {
    stop("'data' must be provided for formula method", call. = FALSE)
  }
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$diag_levels <- NULL
  if (missing(subset)) mf$subset <- NULL
  if (missing(na.action)) mf$na.action <- NULL
  mf[[1]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  response <- model.response(mf)
  group <- mf[[2]]
  if (!is.factor(group)) group <- factor(group)
  # if (nlevels(group) != n_class) {
  #   stop(sprintf("diagnostic group must have exactly %d levels", n_class),
  #        call. = FALSE)
  # }
  original_levels <- levels(group)
  # mean_temp <- aggregate(formula, FUN = mean, data = data)
  # mean_levels <- as.character(mean_temp[order(mean_temp[, 2]), 1])
  mean_temp <- tapply(response, group, mean)
  mean_levels <- names(sort(mean_temp))
  out <- .check_levl_class(diag_levels = diag_levels,
                           original_levels = original_levels,
                           mean_levels = mean_levels,
                           n_class = n_class)
  if (!out$mean_order_ok) {
    warning(
      paste(
        "The order of diagnostic groups does not agree with",
        "the ordering based on increasing sample means.",
        "The analysis proceeds using the supplied group order."
      ),
      call. = FALSE
    )
  }
  levl_class <- out$levl_class
  split_x <- lapply(levl_class, function(cl) response[group == cl])
  names(split_x) <- levl_class
  return(list(response = response, group = group, group.levels = levl_class,
              original_levels = original_levels,
              mean_levels = mean_levels,
              mean_order_ok = out$mean_order_ok,
              split = split_x))
}

.safe_uniroot <- function(interval, ...) {
  f.lower <- ll_prob_adj(interval[1], ...)
  f.upper <- ll_prob_adj(interval[2], ...)
  if (is.nan(f.lower) || is.nan(f.upper)) return(NA_real_)
  if (f.lower * f.upper > 0) return(NA_real_)
  out <- uniroot(ll_prob_adj, interval = interval, ...)$root
  return(out)
}


