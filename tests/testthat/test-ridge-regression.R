
library(testthat)
library(MASS)

context("Test the performance of ridge regression function")

test_that("Your ridge_regression() function works in an easy case.", {
  
  #  let's test on a 500x6 matrix
  b <- c(0,1, rep(0, 4))
  X <- matrix(rnorm(3000), ncol = 6)
  X[,2] <- X[,2]*0.001 + X[,6]*0.999
  y <- X %*% b + rnorm(500)
  
  data <- as.data.frame(cbind(y,X))
  colnames(data)[1]="response"
  
  fit_lmridge= lm.ridge(response ~ ., data, lambda = 0.5)
  fit_ridge = ridge(X, data$response, lambda = 0.5)
  
  expect_equivalent(unlist(fit_lmridge$coef), fit_ridge$coefficients,
                    tolerance = 0.05)
  
})

