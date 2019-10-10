
brute_force_knapsack <- function(x, W) {
  if (any(x < 0) ||
      any(names(x) %in% c("w", "v") == FALSE) ||
      W < 0) {
    stop("Invalid inputs!")
  }

  n <- nrow(x)
  brute_force = list(value    = 0,
                     elements = integer(n))

  # Consider all possible combinations of n elements that can be
  # put in the knapsack
  for (i in 1:((2 ^ n) - 1)) {
    binary_i <- intToBits(i)
    elements <- which(binary_i == 01)

    v_tot <- sum(x[elements, "v"])
    w_tot <- sum(x[elements, "w"])

    # Store the combination of elements with the highest total value
    # with total weight less than or equal to knapsack capacity
    if (v_tot > brute_force$value && w_tot <= W) {
      brute_force$value    <- v_tot
      brute_force$elements <- elements
    }
  }
  return(brute_force)
}


knapsack_dynamic = function(x, W) {
  if (any(x < 0) ||
      any(names(x) %in% c("w", "v") == FALSE) ||
      W < 0) {
    stop("Invalid inputs!")
  }

  n <- nrow(x)
  m <- matrix(data = -1,
              nrow = n,
              ncol = W)

  # Function output- optimum value and list of elements
  knapsack = list(value    = 0,
                  elements = integer())

  # For the first row in matrix m, we can calculate optimum value based
  # only on the first element- by including it for knapsack capacities
  # greater than weight of the first element and excluding it for knapsack
  # capacities less than the weight of the first element
  m[1, x[1, "w"]:W] <- x[1, "w"]
  m[1, which(m[1, ] == -1)] <- 0

  for (i in 2:n) {
    for (j in 1:W) {
      if (x[i, "w"] > j) {
        # If weight of current element i is greater than assumed
        # capacity j of knapsack, then take the best optimum value
        # calculated upto previous element from i - 1 row
        m[i, j] <- m[i - 1, j]

      } else {
        # If weight of current element i is within the assumed capacity
        # j of knapsack, then calculate the new optimum value by comparing
        # both cases - including element i and not including element i
        if (j - x[i, "w"] == 0) {
          # Exception case when the new elements weight is equal to the
          # knapsack capacity j
          m[i, j] <- max(m[i - 1, j], x[i, "v"])
        } else {
          m[i, j] <- max(m[i - 1, j], m[i - 1, j - x[i, "w"]] + x[i, "v"])
        }
      }
    }
  }

  # The optimum value that can be put in the knapsack is m[n][W]
  knapsack$value <- m[n,W]

  # Determine the elements included for the optimum value
  i <- n
  j <- W
  k <- 1
  while (i > 0 && j > 0) {
    if ( m[i, j] != m[i - 1, j]) {
      knapsack$elements[k] <- i
      j <- j - x[i, "w"]
      k <- k + 1
    }
    i <- i - 1
  }
  return(knapsack)
}

greedy_knapsack <- function(x, W) {
  if (any(x < 0) ||
      any(names(x) %in% c("w", "v") == FALSE) ||
      W < 0) {
    stop("Invalid inputs!")
  }

  greedy = list(value    = 0,
                elements = integer())
  i<- 1
  w_tot <- 0

  # Get the value-by-weight ratio for all elements and consider them
  # in decreasing order to put in the knapsack
  v.w <- x[["v"]] / x[["w"]]
  element <- order(v.w, decreasing = TRUE)

  while (w_tot + x[element[i], "w"] <= W) {
    greedy$value <- greedy$value + x[element[i], "v"]
    w_tot        <- w_tot + x[element[i], "w"]
    i <- i + 1
  }

  # Add the fraction (remaining knapsack capacity / weight of the
  # next element) of the value of the next element to the total value
  # of the knapsack
  greedy$value <- greedy$value +
                  ((W - w_tot) / x[element[i], "w"]) * x[element[i], "v"]
  greedy$elements <- element[1:i]

  return(greedy)
}
