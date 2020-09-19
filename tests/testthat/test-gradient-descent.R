library(testthat)

context("Test the function Gradient Descent.")

test_that("Your gradient_descent() function works",{
  set.seed(11)
  X <- matrix(rnorm(400), ncol = 4)
  Y <- rnorm(100)
  test_data= as.data.frame(cbind(X,Y))
  #test on gradient descent function
  fit_gradient_descent<- gradient_descent(Y~.,data=test_data,alpha=0.1,num_iters = 1000)
  fit_lm <-lm(Y~.,data=test_data)
  expect_equivalent(fit_gradient_descent$coefficients,fit_lm$coefficients,tolerance=1e-3)
})


