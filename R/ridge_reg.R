#' Fit a ridge regression model
#' 
#' @description This function fits a ridge regression model. 
#' @param formula a formula
#' @param lambda a penalizing parameter lambda
#' @param data a data.frame
#' @return an ridge_reg object
#' @importFrom stats model.matrix
#' @examples
#' fit <- ridge_reg(Sepal.Length ~., iris, lambda = 1)
#' summary(fit)
#' @export 
ridge_reg <- function(formula, lambda, data) {
  X <- model.matrix(formula, data)
  y <- data[, all.vars(formula)[1]]
  svd_obj <- svd(X)
  U <- svd_obj[["u"]]
  V <- svd_obj[["v"]]
  svals <- svd_obj[["d"]]
  D <- diag(svals / (svals ^ 2 + lambda))
  ridge_beta <- V %*% D %*% t(U) %*% y
  frr <- list(coefficients = ridge_beta)
  class(frr) <- "ridge_reg"
  return(frr)
}