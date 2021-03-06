% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge_regression.R
\name{lm_ridge}
\alias{lm_ridge}
\title{Linear ridge regression}
\usage{
lm_ridge(form, d, lambda)
}
\arguments{
\item{form}{A given formula for fitting the regression.}

\item{d}{A given data set to fit the model.}

\item{lambda}{A tuning parameter for ridge regression to control the penalty}
}
\description{
This is a function implementing ridge regression taking
taking into account of collinear variables by singular value decomposition
}
\examples{
library(palmerpenguins)
data(penguins)
my_rigde = lm_ridge(form=bill_length_mm ~ .,d=penguins,lambda=0.001)
}
