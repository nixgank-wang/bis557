% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lambda_optimizer.R
\name{lambda_optimizer}
\alias{lambda_optimizer}
\title{Optimizing ridge parameter}
\usage{
lambda_optimizer(form, data, v, lambdas)
}
\arguments{
\item{form}{A formula}

\item{data}{The dataset that we use to fit the model}

\item{v}{Number of folds in cross-validation}

\item{lambdas}{A vector of lambdas}
}
\description{
This function optimizes the ridge parameter by comparing the mean value of residuals
}
\examples{
lambda_optimizer(form=form,data=iris,v=10,lambdas=lambdas)
}
