
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
# @importFrom stats lm
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  # lm(formula, data)
  X <- model.matrix(formula, data)
  y <- data[, all.vars(formula)[1]]
  svd_output <- svd(X)
  U <- svd_output[["u"]]
  Sinv <- diag(1 / svd_output[["d"]])
  V <- svd_output[["v"]]
  pseudo_inv <- V %*% Sinv %*% t(U)
  betahat <- pseudo_inv %*% y
  flm <- list()
  flm$coefficients <- betahat
  class(flm) <- "lm"
  return(flm)
}
