#Advanced Programming in R - Lab 1
#Name   : Namita Sharma
#LiU-ID : namsh440

name  <- "Namita Sharma"
liuid <- "namsh440"

#1. my_num_vector()
my_num_vector <- function() {
  my_num <- c(log10(11), cos(pi / 5), exp(pi / 3), (1173 %% 7) / 19)
  return(my_num)
}

#2. filter_my_vector(x, leq)
filter_my_vector <- function(x, leq) {
  x[x >= leq] <- NA
  return(x)
}

#3. dot_prod(a, b)
dot_prod <- function(a, b) {
  dot_prod_res <- sum(a * b)
  return(dot_prod_res)
}

#4. approx_e(N)
approx_e <- function(N) {
  approx_e_res <- sum(1 / factorial(c(0:N)))
  return(approx_e_res)
}
#N=9 to approximate e to the fifth decimal place

#5. my_magic_matrix()
my_magic_matrix <- function() {
  magic_matrix <- matrix(data = c(4, 3, 8, 9, 5, 1, 2, 7, 6), nrow = 3)
  return(magic_matrix)
}
#All columns, rows and diagonals add up to 15

#6. calculate_elements(A)
calculate_elements <- function(A) {
  elements <- length(A)
  return(elements)
}

#7. row_to_zero(A, i)
row_to_zero <- function(A, i) {
  A[i, ] <- 0
  return(A)
}

#8. add_elements_to_matrix(A, x, i, j) 
add_elements_to_matrix <- function(A, x, i, j) {
  A[i,j] <- A[i, j] + x
  return((A))
}

#9. my_magic_list()
my_magic_list <- function() {
  magic_list <- list(info = "my own list", my_num_vector(), my_magic_matrix())
  return(magic_list)
}

#10. change_info(x, text)
change_info <- function(x, text) {
  x[which(names(x) == "info")] <- text
  return(x)
}

#11. add_note(x, note)
add_note <- function(x, note) {
  x[["note"]] <- note
  return(x)
}

#12. sum_numeric_parts(myList)
sum_numeric_parts <- function(x) {
  sum <- sum(sapply(x, function(x) { sum(as.numeric(x)) }), na.rm = TRUE)
  return(sum)
} 

#13. my_data.frame()
my_data.frame <- function(){
  data_frame <- data.frame(id     = c(1:3), 
                           name   = c("John", "Lisa", "Azra"), 
                           income = c(7.30, 0.00, 15.21), 
                           rich   = c(FALSE, FALSE, TRUE),
                           stringsAsFactors = FALSE)
  return(data_frame)
}

#14. sort_head(df, var.name, n)
sort_head <- function(df, var.name, n) {
  df_sort <- df[order(df[[var.name]], decreasing = TRUE)[1:n],]
  return(df_sort)
}

#15. add_median_variable(df, j)
add_median_variable <- function(df, j) {
  median_val <- median(df[[j]])
  df[["compared_to_median"]] <- ifelse(df[[j]] > median_val, "Greater", 
                                       ifelse(df[[j]] < median_val, "Smaller", "Median"))
  return(df)
}

#16. analyze_columns(df, j)
analyze_columns <- function(df, j) {
  result <- list(c(mean = mean(df[[j[1]]]), median = median(df[[j[1]]]), sd = sd(df[[j[1]]])),
                 c(mean = mean(df[[j[2]]]), median = median(df[[j[2]]]), sd = sd(df[[j[2]]])),
                 cor(df[, j]))
  names(result) <- c(names(df)[j[1]], names(df)[j[2]], "correlation_matrix")
  return(result)
}
