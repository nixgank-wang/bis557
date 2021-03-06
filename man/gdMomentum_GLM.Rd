% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm_gradient.R
\name{gdMomentum_GLM}
\alias{gdMomentum_GLM}
\title{Generalized Linear Model first order MLE using momentum gradient descent}
\usage{
gdMomentum_GLM(X, y, mu_fun, lambda, maxit = 10000, tol = 1e-05, gamma = 0.8)
}
\arguments{
\item{X}{The design matrix}

\item{y}{The response variable}

\item{mu_fun}{The function of eta, which is the matrix multiplication of design matrix with vector beta}

\item{lambda}{The stepsize in gradient descent updata}

\item{maxit}{Integer maximum number of iterations}

\item{tol}{Numeric tolerance parameter}

\item{gamma}{The momentum term which indicates how much acceleration you want}
}
\description{
This function implement a first-order solution for the GLM maximum likelihood problem using only
gradient information with adaptive momentum gradient descent algorithm
}
\examples{
 pd <- tibble(offers=c(rep(0,50),rep(1,30),rep(2,10),rep(3,7),rep(4,3)),
division=sample(c("A","B","C"),100,replace = TRUE),
exam=c(runif(50,60,80),runif(30,65,95),runif(20,75,95)))
y<- matrix(pd$offers,ncol = 1)
X<- model.matrix(offers~division+exam,data=pd)
gdMomentum_GLM(X,y,mu_fun=function(eta) exp(eta),lambda = 1e-3)
}
