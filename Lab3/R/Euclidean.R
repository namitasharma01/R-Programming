#' Advanced Programming in R - Lab 3
#' Euclidean algorithm

#' @name euclidean
#' @aliases euclidean
#' @title Euclidean Algorithm
#' @description This function computes the greatest common divisor for 
#'              any two given numbers 
#' @param x Numeric scalar
#' @param y Numeric scalar
#' @return Numeric scalar which is greatest common divisor of x and y
#' @usage euclidean(x, y)
#' @examples euclidean(x = 100, y = 1000)
#' @source \url{"https://en.wikipedia.org/wiki/Euclidean_algorithm"} 
#' @export 

euclidean <- function(x, y) {
  if (!is.numeric.scalar(x) || !is.numeric.scalar(y)) {
    stop("Invalid inputs!")
  }
  while (y != 0) {
    temp <- y
    y <- x %% y
    x <- temp
  }
  return(x)
}

is.numeric.scalar <- function(x) {
  return(is.numeric(x) && length(x) == 1)
}