#' Advanced Programming in R - Lab 4
#' Linear Regression using ordinary least squares


linreg1 <- function(formula, data) {
  linreg_obj <- linreg$new(formula = formula,
                           data    = data)
  return(linreg_obj)
}

