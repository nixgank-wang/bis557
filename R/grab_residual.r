#' @title Grab the residuals
#' @description This is a simple function to grab the residuals of
#' a linear model.
#' @param lm_obj the linear model object created by the lm() function.
#' @examples
#' library(palmerpenguins)
#' fit <- lm(bill_length_mm ~ ., data = penguins[, -8])
#' grab_resids(fit)
#' @export
grab_resids <- function(lm_obj) {
  lm_obj$residuals
}