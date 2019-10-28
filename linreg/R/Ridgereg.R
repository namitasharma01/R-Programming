#' Advanced Programming in R - Bonus Lab
#' Ridge Regression using ordinary least squares

#' @name ridgereg
#' @aliases ridgereg
#' @title Ridge Regression Class
#' @description This class is implemented to handle special output
#'     functions such as print(), plot(), resid(), pred(), coef()
#'     and summary(). Please refer the methods section of documentation
#'     for information on each method
#' @param formula A formula object
#' @param data A dataset
#' @return Returns a ridge model built based on \code{formula} and \code{data}
#' @examples
#' \dontrun{
#' obj = ridgereg(formula = Petal.Length ~ Species, data = iris)
#' obj$print()
#' obj$plot()
#' }
#' @importFrom methods new setRefClass
#' @importFrom gridExtra grid.arrange
#' @export ridgereg

ridgereg <-
  methods::setRefClass(
    "ridgereg",

    fields = list(
      formula      = "formula",     # Formula
      data         = "data.frame",  # Dataset
      lambda       = "numeric",     # Lambda
      data_name    = "character",   # Name of the dataset
      X            = "matrix",      # Model matrix
      Y            = "matrix",      # observed values of predicted variable
      I            = "matrix",      # Identity matrix
      beta_hat     = "matrix",      # Regression Coefficients
      Y_hat        = "matrix",      # Predicted values
      E_hat        = "matrix",      # Residuals
      df           = "numeric",     # Degrees of freedom
      sigsq_hat    = "matrix",      # Residual variance
      E_hat_std    = "matrix",      # Root of standardized residuals
      var_beta_hat = "matrix",      # Variance of regression coefficients
      std_e        = "numeric",     # Standard error of regression coefficients
      t_values     = "numeric",     # t-values for regression coefficients
      p_values     = "numeric"      # p-values for regression coefficients
    ),

    methods = list(
      initialize = function(formula, data, lambda) {
        tryCatch(
          {
            formula      <<- formula
            data         <<- data
            data_name    <<- deparse(substitute(data))
            X            <<- model.matrix(object = formula, data = data)
            Y            <<- cbind(data[, all.vars(formula)[1]])
            I            <<- diag(nrow = ncol(X))
            beta_hat     <<- solve((t(X) %*% X) + (lambda * I)) %*% (t(X) %*% Y)
            Y_hat        <<- X %*% beta_hat
            E_hat        <<- Y - Y_hat
            df           <<- nrow(X) - ncol(X)
            sigsq_hat    <<- (t(E_hat) %*% E_hat) / df
            E_hat_std    <<- sqrt(abs(E_hat - mean(E_hat)) / as.numeric(sqrt(sigsq_hat)))
            var_beta_hat <<- as.numeric(sigsq_hat) * solve((t(X) %*% X))
            std_e        <<- sqrt(diag(var_beta_hat))
            t_values     <<- as.vector(beta_hat) / std_e
            p_values     <<- 2 * stats::pt(q = -abs(t_values), df = df)
          },
          error = function(e) {
            stop(e)
          }
        )
      },

      print = function() {
        "Print out the ridge regression coefficients and coefficient names"

        beta_hat_df        <- rbind.data.frame(as.vector(beta_hat))
        names(beta_hat_df) <- rownames(beta_hat)

        cat("\nCall:\n",
            paste0("ridgereg(formula = ", format(formula), ", data = ", data_name, ")"),
            "\n\nCoefficients:\n ")

        print.data.frame(beta_hat_df, digits = 4, row.names = FALSE)
      },

      predict = function() {
        "Return the predicted values"

        return(as.vector(Y_hat))
      },

      coef = function() {
        "Return the ridge regression coefficients as a named vector"

        beta_hat_v <- round(x = as.vector(beta_hat), digits = 3)
        names(beta_hat_v) <-dimnames(beta_hat)[[1]]

        return(beta_hat_v)
      }
    )
  )


#' @name ridgereg_f
#' @aliases ridgereg_f
#' @title Ridge Regression Function
#' @description This function takes two arguments- formula and data,
#'     and returns an object of class ridgereg which builds a ridge
#'     regression model
#' @param formula formula (y ~ x)
#' @param data dataframe
#' @return An object of RC class ridgereg
#' @usage ridgereg_f(formula, data)
#' @examples ridgereg_f(formula = Petal.Length ~ Species, data = iris)
#' @export ridgereg_f

ridgereg_f <- function(formula, data, lambda = 0) {

  ridgereg_obj <- ridgereg$new(formula = formula,
                               data    = data,
                               lambda  = lambda)
  return(ridgereg_obj)
}

#' @name visualize_airport_delays
#' @aliases visualize_airport_delays
#' @title Visualize airport delays
#' @description This function takes two arguments- formula and data,
#'     and returns an object of class ridgereg which builds a ridge
#'     regression model
#' @param formula formula (y ~ x)
#' @param data dataframe
#' @return An object of RC class ridgereg
#' @usage ridgereg_f(formula, data)
#' @examples ridgereg_f(formula = Petal.Length ~ Species, data = iris)
#' @export ridgereg_f
#'
visualize_airport_delays <- function() {
  # Plot two graphs:
  # 1. Mean departure delays for different airports (Origins)
  # 2. Mean arrival delays for different airports (Destinations)


  # Mean departure delays for different airports (Origins)
  dep_delay <- nycflights13::flights %>%
                 group_by(origin) %>%
                 summarise(avg = mean(dep_delay, na.rm = TRUE)) %>%
                 arrange(avg)

  outliers_dep <- ifelse(is_outlier(dep_delay$avg), dep_delay$origin, as.numeric(NA))
  f <- ggplot2::ggplot(data    = dep_delay,
                       mapping = ggplot2::aes(x = dep_delay$origin, y = dep_delay$avg)) +
       ggplot2::geom_point(shape = 1) +
       ggplot2::labs(title = "Mean departure delay of flights from airports",
                     x     = "Airports",
                     y     = "Mean flight departure delay") +
       ggplot2::geom_text(ggplot2::aes(label = outliers_dep), na.rm = TRUE, nudge_y = 1.5, size = 3) +
       ggplot2::theme_classic() +
       ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                      panel.border = ggplot2::element_rect(fill = NA)) +
       ggplot2::geom_smooth(method = stats::loess, se = FALSE, col = 2)


  # Mean arrival delays for different airports (Destinations)
  arr_delay <- nycflights13::flights %>%
                 group_by(dest) %>%
                 summarise(avg = mean(arr_delay, na.rm = TRUE)) %>%
                 arrange(avg)

  outliers_arr <- ifelse(is_outlier(arr_delay$avg), arr_delay$dest, as.numeric(NA))
  g <- ggplot2::ggplot(data    = arr_delay,
                       mapping = ggplot2::aes(x = arr_delay$dest, y = arr_delay$avg)) +
       ggplot2::geom_point(shape = 1) +
       ggplot2::labs(title = "Mean arrival delay of flights to airports",
                     x     = "Airports",
                     y     = "Mean flight arrival delay") +
       ggplot2::geom_text(ggplot2::aes(label = outliers_arr), na.rm = TRUE, nudge_y = 1.5, size = 3) +
       ggplot2::theme_classic() +
       ggplot2::theme(axis.text.x  = element_blank()) +
       ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                      panel.border = ggplot2::element_rect(fill = NA)) +
       ggplot2::geom_smooth(method = stats::loess, se = FALSE, col = 2)

  # Plot the two graphs on the same window
  gridExtra::grid.arrange(f, g, ncol = 2, respect = TRUE)
}

is_outlier <- function(x) {
  # Return TRUE if data point x is an outlier based on the 1.5 rule

  return(x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE) | x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
}

# Training dataset
BH2_train <- caret::createDataPartition(iris$Species, p = .6,
                                        list            = FALSE,
                                        times           = 1)

