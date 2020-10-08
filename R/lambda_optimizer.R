

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

