
#' @title Implement gradient descent for ordinary least squares
#' @name gradient_descent
#' @description This is a function implementing gradient descent algorithm to 
#' estimate the parameters in OLS model
#' @param form A given formula for fitting the regression
#' @param data A given dataset to fit the model
#' @param alpha The learning rate
#' @param num_iters The number of iterations
#' @param contrasts list of constasts for factor variables

#' @examples
#' my_fit =gradient_descent(bill_length_mm ~ ., data = penguins[,-8],alpha=0.1,num_iters=1000)
#' 
#' @export
gradient_descent <- function(form,data,alpha,num_iters,contrasts=NULL){
  # remove rows with NA values based on the given formula
  data_no_na <- model.frame(form,data)
  # get the design matrix and dependent variable Y
  X<- model.matrix(form, data_no_na,contrasts.arg = contrasts)
  y_name<- as.character(form)[2]
  Y <- matrix(data_no_na[,y_name],ncol=1)
  #initialize theta
  l= dim(X)[2]
  theta<- as.matrix(rep(0,l))
  #set up the cost function for OLS regression
  cost<- function(X,Y,theta){
    m <-length(Y)
    J<-sum((X%*%theta-Y)^2)/(2*m)
    return(J)
  }
  #running for iterations to perform gradient descent
  J_hist <- rep(0, num_iters)
  for (i in 1:num_iters){
    # this for-loop records the cost history for every iterative move of the gradient descent
    theta = theta- alpha*(1/length(Y))*(t(X)%*%(X%*%theta - Y))
    J_hist[i]  <- cost(X, Y, theta)
  }
  results <-list(coefficients=theta)
  return(results)
}



