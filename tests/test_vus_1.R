mu_true <- c(0, 2.5, 3.69)
sigma_true <- c(1, 1.1, 1.2)

n1 <- 15
n2 <- 15
n3 <- 15

X1 <- rnorm(n1, mu_true[1], sigma_true[1])
X2 <- rnorm(n2, mu_true[2], sigma_true[2])
X3 <- rnorm(n3, mu_true[3], sigma_true[3])

data_test <- data.frame(biom = c(X1, X2, X3), 
                        diag = rep(c("1", "2", "3"), c(n1, n2, n3)))
vus(x = X1, y = X2, z = X3)

vus(formula = biom ~ diag, data = data_test)

vus(formula = biom ~ diag, data = data_test, ci_level = 0.9)

vus(formula = biom ~ diag, data = data_test, diag_levels = c("2", "3", "1"))

out_test <- vus(formula = biom ~ diag, data = data_test)
print(out_test)

vus(formula = biom ~ diag, data = data_test, plot = TRUE)


# vus.formula2 <- function(formula, data, subset, na.action, 
#                          diag_levels = NULL, ...) {
#   call <- match.call()
#   if (missing(data)) {
#     stop("'data' must be provided for formula method", call. = FALSE)
#   }
#   mf <- match.call(expand.dots = FALSE)
#   mf$diag_levels <- NULL
#   mf$... <- NULL
#   mf[[1]] <- quote(model.frame)
#   mf <- eval(mf, parent.frame())
#   response <- model.response(mf)
#   group <- mf[[2]]
#   if (!is.factor(group)) {
#     group <- factor(group)
#   }
#   if (nlevels(group) != 3) {
#     stop("diagnostic group must have 3 levels", call. = FALSE)
#   }
#   mean_temp <- aggregate(formula, FUN = mean, data = data)
#   print(mean_temp)
#   temp_levl <- mean_temp[order(mean_temp[, 2]), 1]
#   print(temp_levl)
#   # if (!is.null(diag_levels)) {
#   #   if (!all(diag_levels %in% levels(group))) {
#   #     stop("Invalid 'levels'", call. = FALSE)
#   #   }
#   #   if (length(levels) != 3) {
#   #     stop("'levels' must have length 3", call. = FALSE)
#   #   }
#   #   group <- factor(group, levels = diag_levels, ordered = TRUE)
#   # } else {
#   #   if (nlevels(group) != 3) {
#   #     stop("group must have 3 levels", call. = FALSE)
#   #   }
#   # }
#   .check_levl_class(trace = TRUE, diag_levels, temp_levl)
#   lev <- levels(group)
#   print(lev)
# }
# 
# vus.formula2(formula = biom ~ diag, data = data_test, 
#              diag_levels = c("1", "2", "3"))
# 
# vus.formula2(formula = biom ~ diag, data = data_test)
# 
# vus.formula2(formula = biom ~ diag, data = data_test, 
#              diag_levels = c("2", "3", "1"))
