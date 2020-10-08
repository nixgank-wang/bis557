library(testthat)

context("Test the performance of gradient descent out of sample function")

test_that("You gradient_descent_loss() function works in an easy case.", {
  contrasts=NULL
  data("iris")
  data = iris 
  data_no_na <- model.frame(form,data)
  # get the design matrix and dependent variable Y
  X<- model.matrix(form, data_no_na,contrasts.arg = contrasts)
  form = Sepal.Length ~.
  resids_gd <- gradient_descent_loss(form=form, data=iris, alpha=0.01,num_iters=1000 , v=10)
  original_resids<- gradient_descent(form=form, data=iris, alpha=0.01,num_iters=1000 , v=10)$coefficients %*% X
  expect_equivalent(resids_gd,original_resids)
})
