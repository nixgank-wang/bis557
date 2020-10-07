
#' @title Prediction based on a gradient descent object
#' @description This function takes in a gradient descent model object and a data set, returns the
#' prediction based on the model.
#' @param object A gradient descent model object
#' @param ...   An input dataset which we need prediction of.

#' @examples
#' library(palmerpenguins)
#' data(penguins)
#' my_fit =gradient_descent(bill_length_mm ~ ., data = penguins[,-8],alpha=0.1,num_iters=1000)
#'my_gd_predict = predict.gd_object(my_fit,data = penguins[,-8])

#' @export
predict.gd_object <- function (object, ...) {
  dots <- list(...)
  d <- dots[[1]]
  if (!inherits(d, "data.frame")) {
    stop("Second argument must be a data frame.")
  }
  m <- model.matrix(object$form, d)
  m %*% object$coefficients
}
