
library(testthat)
library(MASS)

context("Test the performance of ridge regression function")

test_that("Your lm_ridge() function works well", {

  set.seed(11)
  X <- matrix(rnorm(400), ncol = 4)
  Y <- rnorm(100)
  test_data= as.data.frame(cbind(X,Y))
  #implent lm.ridge function
  fit_lm.ridge= lm.ridge(Y ~ ., data=test_data, lambda = 0.01)
  #implemt our function
  fit_ridge_regression = lm_ridge(Y ~ .,data=test_data , lambda = 0.01)
  expect_equivalent(coef(fit_lm.ridge), fit_ridge_regression$coefficients,
                    tolerance = 0.1)
})

