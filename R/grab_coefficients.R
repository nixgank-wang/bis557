#' @title  Grab the slope coefficients
#' @description This is a simple function to grab the slope coefficients of
#' the linear model
#' @param lm_ob the linear object created by the lm() function.
#' @examples
#' library(palmerpenguins)
#' fit<- lm(bill_length_mm~.,data=penguins[,-8])
#' grab_coeffs(fit)
#'
#' @export
grab_coeffs<-function(lm_ob){
  lm_ob$coefficients
}
