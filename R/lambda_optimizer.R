#' @title Optimizing ridge parameter
#' @description This function optimizes the ridge parameter by comparing the mean value of residuals
#' @param form A formula
#' @param data The dataset that we use to fit the model
#' @param v    Number of folds in cross-validation
#' @param lambdas A vector of lambdas
#' @importFrom foreach %do% foreach
#' @importFrom tibble tibble
#' @importFrom rsample vfold_cv

#' @examples
#'lambda_optimizer(form=form,data=iris,v=10,lambdas=lambdas)
#' @export
lambda_optimizer<-function(form,data,v,lambdas){
  #create v folds for cross validation
  folds <-vfold_cv(data,v=v)
  y_name<- as.character(form)[2]
  resids<-foreach(lambda=lambdas) %do%{
      foreach(fold=folds$splits,.combine = c) %do%  {
        fit<- lm_ridge(form=form, d= analysis(fold), lambda = lambda)
        as.vector(assessment(fold)[,y_name] - predict(fit, assessment(fold)))
      }
   }

  rd <- tibble(lambda=lambdas, meam = map_dbl(resids,mean))
  rd
}

