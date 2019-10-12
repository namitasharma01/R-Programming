# Profiling

#RNGkind(sample.kind = "Rounding")
#set.seed(42)
#n <- 1000000
#knapsack_objects <- data.frame(
#  w=sample(1:4000, size = n, replace = TRUE),
#  v=runif(n = n, 0, 10000)
#)
#
#source("C:/Users/namit/OneDrive/Documents/R-Programming-Lab/knapsack/R/greedy_knapsack.R")
#profvis::profvis(dk <- greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))
#
#source("C:/Users/namit/OneDrive/Documents/R-Programming-Lab/knapsack/R/dynamic_knapsack.R")
#profvis::profvis(dk <- dynamic_knapsack(x = knapsack_objects[1:500,], W = 3500))
#
#source("C:/Users/namit/OneDrive/Documents/R-Programming-Lab/knapsack/R/brute_force_knapsack.R")
#profvis::profvis(bfk <- brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500))
#profvis::profvis(bfk_par <- brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500, parallel = TRUE))
#
#Rcpp::sourceCpp("C:/Users/namit/OneDrive/Documents/R-Programming-Lab/knapsack/src/dynamic_knapsack.cpp")
#dk_cpp <- dynamic_knapsack_cpp(x = knapsack_objects[1:500,], W = 3500)
#
#
