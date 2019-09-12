#Advanced Programming in R - Lab 3

#' @title Euclidean algorithm
#' @description This package computes the greatest common divisor for two given numbers
#' @param x (numeric scalar)
#' @param y (numeric scalar)
#' @return greatest common divisor (numeric scalar)
#' @examples euclidean(100, 1000)
#' @export euclidean

name  <- "Namita Sharma"
liuid <- "namsh440"

euclidean <- function(x, y){
  if( !is.numeric.scalar(x) || !is.numeric.scalar(y) ){
    stop("Invalid inputs!")
  }
  while(y != 0){
    temp <- y
    y <- x %% y
    x <- temp
  }
  return(x)
}

is.numeric.scalar <- function(x){
  return( is.numeric(x) && length(x)==1 )
}