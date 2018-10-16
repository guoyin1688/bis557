#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return an lm object
#' @importFrom stats model.matrix terms
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  # lm(formula, data)
  X <- model.matrix(formula, data)
  y <- data[, all.vars(formula)[1]]
  betahat <- qr.solve(X, y)
  betahat[which(betahat == 0)] <- NA
  # svd_output <- svd(X)
  # U <- svd_output[["u"]]
  # Sinv <- diag(1 / svd_output[["d"]])
  # V <- svd_output[["v"]]
  # pseudo_inv <- V %*% Sinv %*% t(U)
  # betahat <- pseudo_inv %*% y
  y_fit = X %*% betahat
  residuals = y - y_fit
  X_qr = qr(X)
  flm <- list(coefficients = betahat, residals = residuals, fitted.values = y_fit,
              rank = ncol(X), weights = NULL, df.residual = nrow(X) - ncol(X), 
              call = call('lm', formula), terms = terms(x = formula, data = data),
              contrasts = NULL, xlevels = NULL, offset = NULL, y = y, x = X, 
              model = formula, na.action = NULL, qr = X_qr)
  class(flm) <- "lm"
  return(flm)
}
