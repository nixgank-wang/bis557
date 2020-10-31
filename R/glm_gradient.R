#' @title Generalized Linear Model first order MLE using constant gradient descent
#' @description This function implement a first-order solution for the GLM maximum likelihood problem using only
#' gradient information
#' @param X The design matrix
#' @param y The response variable
#' @param mu_fun The function of eta, which is the matrix multiplication of design matrix with vector beta
#' @param lambda The stepsize in gradient descent updata
#' @param maxit Integer maximum number of iterations
#' @param tol Numeric tolerance parameter
#' @examples
#'  pd <- tibble(offers=c(rep(0,50),rep(1,30),rep(2,10),rep(3,7),rep(4,3)),
#' division=sample(c("A","B","C"),100,replace = TRUE),
#' exam=c(runif(50,60,80),runif(30,65,95),runif(20,75,95)))
#' y<- matrix(pd$offers,ncol = 1)
#' X<- model.matrix(offers~division+exam,data=pd)
#' gdConstantGLM(X,y,mu_fun=function(eta) exp(eta),lambda = 1e-3)
#' @export
gdConstantGLM<- function(X,y,mu_fun,lambda, maxit=10000,tol=1e-5){
  #initialize a matrix for storage of updated parameters
  beta<-matrix(rep(0,ncol(X),ncol=1))
  beta_diff<-c()
  #iterates given the number of maximum iterations
  for (i in seq_len(maxit)){
    beta_old <-beta
    # calculate eta
    eta <- X %*% beta
    mu <- mu_fun(eta)
    # calculate gradient in current iteration
    grad <- t(X)%*%(y-mu)
    #update beta based on gradient descent, given learning rate as lambda
    beta <- beta+ lambda*grad
    beta_diff<- c(beta_diff,sum(beta-beta_old)^2)
    # end iteration if reaches tolerance
    if(tail(beta_diff,1)<= tol){
      break
    }
  }
  #return a list of calculated parameters and the beta_difference
  list(beta=beta, beta_diff=beta_diff)
}

#' @title Generalized Linear Model first order MLE using momentum gradient descent
#' @description This function implement a first-order solution for the GLM maximum likelihood problem using only
#' gradient information with adaptive momentum gradient descent algorithm
#' @param X The design matrix
#' @param y The response variable
#' @param mu_fun The function of eta, which is the matrix multiplication of design matrix with vector beta
#' @param lambda The stepsize in gradient descent updata
#' @param maxit Integer maximum number of iterations
#' @param tol Numeric tolerance parameter
#' @param gamma The momentum term which indicates how much acceleration you want
#' @examples
#'  pd <- tibble(offers=c(rep(0,50),rep(1,30),rep(2,10),rep(3,7),rep(4,3)),
#' division=sample(c("A","B","C"),100,replace = TRUE),
#' exam=c(runif(50,60,80),runif(30,65,95),runif(20,75,95)))
#' y<- matrix(pd$offers,ncol = 1)
#' X<- model.matrix(offers~division+exam,data=pd)
#' gdMomentum_GLM(X,y,mu_fun=function(eta) exp(eta),lambda = 1e-3)
#' @export
gdMomentum_GLM<- function(X,y,mu_fun,lambda, maxit=10000,tol=1e-5,gamma=0.8){
  #initialize a matrix for storage of updated parameters
  beta<-matrix(rep(0,ncol(X),ncol=1))
  beta_diff<-c()
  #iterates given the number of maximum iterations
  for (i in seq_len(maxit)){
    beta_old <-beta
    # calculate eta
    eta <- X %*% beta
    mu <- mu_fun(eta)
    # calculate gradient in current iteration
    grad <- t(X)%*%(y-mu)
    #accelerate gradient convergence by momentum
    weighted_grad<- (1-gamma)*grad + beta*gamma
    #update beta by weighted gradient
    beta <- beta+ lambda*weighted_grad
    beta_diff<- c(beta_diff,sum(beta-beta_old)^2)
    # end iteration if reaches tolerance
    if(tail(beta_diff,1)<= tol){
      break
    }
  }
  #return a list of calculated parameters and the beta_difference
  list(beta=beta, beta_diff=beta_diff)
}




