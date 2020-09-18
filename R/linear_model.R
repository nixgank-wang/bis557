
#' @title  Linear Model
#' @name linear_model
#' @description This is a function takes a formula and a dataset as input,
#' fit a linear model based on the input and returns a list of coefficients
#' @param form A given formula for fitting the regression.
#' @param data A given dataset to fit the model.
#' @param contrasts a list of constasts for factor variables.

#' @examples
#' library(palmerpenguins)
#' data(penguins)
#' my_fit = linear_model(bill_length_mm ~ ., data = penguins[,-8])
#' @export

linear_model = function(form, data,contrasts = NULL){
  # remove rows with NA values based on the given formula
  data_no_na <- model.frame(form,data)
  X <- model.matrix(form, data_no_na,contrasts.arg = contrasts)
  y_name<- as.character(form)[2]
  Y <- matrix(data_no_na[,y_name],ncol=1)
  #beta<- solve(t(X)%*%X) %*% t(X) %*% Y
  beta<-solve.qr(qr(X),Y)
  beta[which(beta==0)]=NA

  beta_names <- rownames(beta)
  beta <-as.numeric(beta)
  names(beta) <- beta_names
  ret <- list(coefficients=beta)
  class(ret) <- "my_lm"
  ret
}
#reorganize the output
print.my_lm <- function(x,...){
  cat("\nCoefficients:\n",x$coefficients)
}



