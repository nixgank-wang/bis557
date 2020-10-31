#' @title Multi-classification logistic regression
#' @description This function implement a classification model generalizing logistic regression
#' to accommodate more than two classes
#' @param form A given formula for fitting the regression
#' @param data A given dataset to fit the model
#' @param maxit Integer maximum number of iterations
#' @param tol  Numeric tolerance parameter
#' @examples
#' library(palmerpenguins)
#' data(penguins)
#' form = is_gentoo ~ body_mass_g +bill_length_mm
#' my_penguins<- penguins %>% mutate(is_gentoo = as.numeric(species == "Gentoo")) %>% model.frame(form,.)
#' multinom_logit(form=form,data=my_penguins)
#' @export
multinom_logit<- function(form,data,maxit=25,tol=1e-10){
  #create design matrix and label vector given formula and data
  label_name = as.character(form)[2]
  y<- as.factor(matrix(as.list(data)[label_name]))
  X<- model.matrix(form,data)
  class_num<- length(unique(y))
  # initialize the parameter as a matrix, with the dimension of number of parameter * number of classes
  beta <- matrix(0,nrow=class_num,ncol=ncol(X))
  beta_old <- matrix(0,num,ncol(X))
  #claim we are using logisitic regression
  family= binomial(link = "logit")
  # the following code is referenced from textbook, solve generalized linear models with Newton-Ralphson method
  for(j in seq_len(maxit)){
    for (i in 1:class_num){
      curr_label<-as.numeric(levels(y)[i])
      curr_y <- ifelse(y==curr_label,1,0)
      beta_old[i,] <- beta[i,]
      eta <- X %*% beta[i,]
      mu <- family$linkinv(eta)
      mu_p <- family$mu.eta(eta)
      z <- eta + (curr_y - mu) / mu_p
      W <- as.numeric(mu_p^2 / family$variance(mu))
      XtX <- crossprod(X, diag(W) %*% X)
      Xtz <- crossprod(X, W * z)
      beta[i,] <- solve(XtX, Xtz)
      if(sqrt(crossprod(beta[i,] - beta_old[i,])) < tol) break
    }
  }
  #return regression matrix beta
  return(beta)
}
