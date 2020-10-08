#' @title Loss of OLS model based on out-of-sample accuracy
#' @description This function fits OLS model using gradient descent that calculates the
#' loss based on out-of-sample accuracy
#' @param form A given formula for fitting the regression
#' @param data A given dataset to fit the model
#' @param alpha The learning rate
#' @param num_iters The number of iterations
#' @param v  The number of folds when implementing n-folds cross-validation
#' @examples
#' library(palmerpenguins)
#' data(penguins)
#' gradient_descent_loss(bill_length_mm ~ ., data = penguins[,-8],alpha=0.1,num_iters=1000)

#' @export

gradient_descent_loss <- function(form,data,alpha,num_iters,v){
  #create cross-validation folds for out-of-sample accuracy
  v=10
  folds<- vfold_cv(data,v=v)
  y_name<- as.character(form)[2]
  resids<-foreach(fold=folds$splits,.combine = c) %do% {
    fit<- gradient_descent(form,data,alpha,num_iters)
    as.vector(as.matrix(assessment(fold)[,y_name],ncol=1))-as.vector(predict.gd_object(fit,assessment(fold)))
  }
  
}

