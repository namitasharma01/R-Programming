#Advanced Programming in R - Lab 2
#Name   : Namita Sharma
#LiU-ID : namsh440

name  <- "Namita Sharma"
liuid <- "namsh440"

#1. sheldon_game(player1, player2)
sheldon_game <- function(player1, player2) {
  
# create a list with named vector elements, one for each of the possible user inputs
# where each vector contains all user inputs below its name rank
  rank <- list(rock     = c("scissors", "lizard"  ), 
               paper    = c("rock"    , "spock"   ),
               scissors = c("paper"   , "lizard"  ), 
               spock    = c("rock"    , "scissors"),
               lizard   = c("paper"   , "spock"   ))
  
  if (!(tolower(player1) %in% names(rank)) || 
      !(tolower(player2) %in% names(rank))) {
    stop("Invalid user input")
  }
  
  return(ifelse(tolower(player2) %in% rank[[tolower(player1)]], 
                "Player 1 wins!", 
                ifelse(tolower(player1) %in% rank[[tolower(player2)]], 
                       "Player 2 wins!", 
                       "Draw!")))
}

#2. my_moving_median(x, n, ...)
my_moving_median <- function(x, n, ...) {
  if (!is.vector(x, mode = "numeric") || !is.numeric.scalar(n)) {
    stop("Invalid inputs")
  }
  
  moving_median <- numeric(length(x) - n)
  for (i in 1:(length(x) - n)) {
    moving_median[i] <- median(x[i:(i + n)], na.rm = ...)
  }
  return(moving_median)
}

#3. for_mult_table(from, to)
for_mult_table <- function(from, to) {
  if (!is.numeric.scalar(from) || !is.numeric.scalar(to)) {
    stop("Invalid inputs")
  }
  mult_table <- matrix(data = NA, 
                       nrow = to - from + 1, 
                       ncol = to - from + 1, 
                       dimnames = list(c(from:to), c(from:to)))
  for (i in c(from:to)) { 
    mult_table[,as.character(i)] <- i * c(from:to)
  }
  return(mult_table)
}

#4. cor_matrix()
cor_matrix <- function(X) {
  if (!is.data.frame((X))) {
    stop("Invalid input")
  }
  n <- dim(X)[1]
  corr_matrix <- matrix( data = NA,
                         nrow = dim(X)[2],
                         ncol = dim(X)[2])
  for (i in 1:length(X)) {
    for (j in 1:length(X)) {
      x <- X[[i]]
      y <- X[[j]]
      x_mean <- sum(x) / n
      y_mean <- sum(y) / n
      x_sd <- sqrt(sum((x - x_mean) ^ 2) / (n - 1))
      y_sd <- sqrt(sum((y - y_mean) ^ 2) / (n - 1)) 
      
      cor_coeff <- (1 / (n - 1)) * sum((x - x_mean) * (y - y_mean)) / (x_sd * y_sd)
      corr_matrix[i, j] <- round(cor_coeff, 5)
    }
  }
  return(corr_matrix)
}

#5. find_cumsum(x, find_sum)
find_cumsum <- function(x, find_sum) {
  if (!is.vector(x, mode = "numeric") || !is.numeric.scalar(find_sum)) {
    stop("Invalid inputs")
  }
  i <- 1
  total <- 0 
  while (total <= find_sum && i <= length(x)) {
    total <- total + x[i]
    i <- i + 1
  }
  return(total)
}

#6. while_mult_table(from, to)
while_mult_table <- function(from, to) {
  if (!is.numeric.scalar(from) || !is.numeric.scalar(to)) {
    stop("Invalid inputs")
  }
  i <- 1 
  mult_table <- matrix(data = NA,
                       nrow = to - from + 1,
                       ncol = to - from + 1,
                       dimnames = list(c(from:to), c(from:to)))
  while (i <= to - from + 1) {
    mult_table[, i] <- c(from:to)[i] * c(from:to)
    i <- i+1
  }
  return(mult_table)
}

#7. trial_division_factorization(x)
trial_division_factorization <- function(x) {
  if (!is.numeric.scalar(x)) {
    stop("Invalid input")
  }
  factors <- c()
  r <- x
  i <- 2
  while (prod(factors) != x) {
    if (r %% i == 0){
      factors <- c(factors, i)
      r <- r / i
      i <- 1
    }
    i <- i + 1
  }
  return(factors)
}

#8. repeat_find_cumsum(x, find_sum)
repeat_find_cumsum <- function(x, find_sum) {
  if (!is.vector(x, mode = "numeric") || !is.numeric.scalar(find_sum)) {
    stop("Invalid inputs")
  }
  i <- 1
  total <- 0
  repeat{
    if (total > find_sum || i > length(x)) {
      break
    }
    total <- total + x[i]
    i <- i + 1
  }
  return(total)
}

#9. repeat_my_moving_median(x, n)
repeat_my_moving_median <- function(x, n, ...) {
  if (!is.vector(x, mode = "numeric") || !is.numeric.scalar(n)) {
    stop("Invalid inputs")
  }
  i <- 1
  moving_median <- numeric(length(x) - n)
  repeat {
    if (i > (length(x) - n)) {
      break
    }
    moving_median[i] <- median(x[i:(i + n)], na.rm = ...)
    i <- i + 1
  }
  return(moving_median)
}

#10. in_environment(env)
in_environment <- function(env) {
  return(ls(env))
}

#11. where(fun)
where <- function(fun) {
  if (!(is.character(fun) && length(fun) == 1)) {
    stop("Invalid input")
  }
  i <- 1
  while (i <= length(search())) {
    if (fun %in% ls(search()[i])) {
      env.name <- search()[i]
      break
    }
    i <- i+1
  }
  return(ifelse(exists("env.name"), env.name, paste(fun, "not found!")))
}

#12. cov(X)
cov <- function(X) {
  if (!is.data.frame(X)) {
    stop("Invalid input")
  }
  cov_list <- lapply( X, 
                      function(x) { 
                        return(round(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE), 5))
                      })
  cov_vector <- as.numeric(unlist(cov_list))
  names(cov_vector) <- names(X)
  return(cov_vector)
}

#13. moment(i)
moment <- function(i) {
  if (!is.numeric.scalar(i)) {
    stop("Invalid input")
  }
  function(x) {
    return(mean((x-mean(x)) ^ i))
  }
}

#14.mcmc_counter_factory(burnin, thin)
mcmc_counter_factory <- function(burnin, thin) {
  if (!(is.numeric.scalar(burnin) && burnin >= 0) ||
      !(is.numeric.scalar(thin) && thin > 0)) {
    stop("Invalid inputs")
  }
  iteration <- 0
  samples <- 0
  
  function() {
    iteration   <<- iteration + 1
    store_sample <- FALSE
    if (iteration > burnin && (iteration-burnin) %% thin == 0) {
      samples <<- samples + 1
      store_sample <- TRUE
    }
    return(list(iteration, store_sample, samples))
  }
}

#15. is.numeric.scalar(x)
is.numeric.scalar <- function(x) {
  return(is.numeric(x) && length(x) == 1)
}

