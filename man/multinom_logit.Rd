% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_logit.R
\name{multinom_logit}
\alias{multinom_logit}
\title{Multi-classification logistic regression}
\usage{
multinom_logit(form, data, maxit = 25, tol = 1e-10)
}
\arguments{
\item{form}{A given formula for fitting the regression}

\item{data}{A given dataset to fit the model}

\item{maxit}{Integer maximum number of iterations}

\item{tol}{Numeric tolerance parameter}
}
\description{
This function implement a classification model generalizing logistic regression
to accommodate more than two classes
}
\examples{
library(palmerpenguins)
data(penguins)
form = is_gentoo ~ body_mass_g +bill_length_mm
my_penguins<- penguins \%>\% mutate(is_gentoo = as.numeric(species == "Gentoo")) \%>\% model.frame(form,.)
multinom_logit(form=form,data=my_penguins)
}
