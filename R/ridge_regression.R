#' @title Linear ridge regression 
#' @name  lm_ridge
#' @description This is a function implementing ridge regression taking
#' taking into account of collinear variables by singular value decomposition
#' @param form A given formula for fitting the regression.
#' @param d A given data set to fit the model.
#' @param lambda A tuning parameter for ridge regression to control the penalty

#' @examples
#' library(palmerpenguins)
#' data(penguins)
#' my_rigde = lm_ridge(form=bill_length_mm ~ .,d=penguins,lambda=0.001)
#' @export
lm_ridge<-function(form,d,lambda){
  #grab design matrix and response vector from given input
  d_no_na<-model.frame(form,d)
  X<-model.matrix(form,d_no_na)
  y_name <-as.character(form)[2]
  Y<-matrix(d_no_na[,y_name],ncol=1)
  #singular value decomposition of X to take care of collinearity
  svd_x<- svd(X)
  # beta = V (sigma + lambda I)^{-1} *sigma U^T* Y
  sigma <- diag(svd_x$d)
  lambda_I <-diag(rep(lambda,length(svd_x$d)))
  beta <- svd_x$v %*% solve(sigma^2 +lambda_I) %*% sigma %*% t(svd_x$u) %*% Y
  ret<- list(coefficients= beta,form=form)
  class(ret)<- "ridge_object"
  ret 
}
