#Advanced Programming in R - Lab 3
#Euclidean algorithm

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