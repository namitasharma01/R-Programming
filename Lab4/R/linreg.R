#' Advanced Programming in R - Lab 4
#' Linear Regression using ordinary least squares

#' @name linreg
#' @aliases linreg
#' @title Linear Regression Class
#' @description This class is implemented to handle special output
#'     functions such as print(), plot(), resid(), pred(), coef()
#'     and summary(). Please refer the methods section of documentation
#'     for information on each method
#' @param formula A formula object
#' @param data A dataset
#' @return Returns a linear model built based on \code{formula} and \code{data}
#' @examples
#' \dontrun{
#' obj = linreg(formula = Petal.Length ~ Species, data = iris)
#' obj$print()
#' obj$plot()
#' }
#' @importFrom methods new setRefClass
#' @importFrom gridExtra grid.arrange
#' @export linreg

linreg <-
  methods::setRefClass(
    "linreg",

    fields = list(
      formula      = "formula",     # Formula
      data         = "data.frame",  # Dataset
      data_name    = "character",   # Name of the dataset
      X            = "matrix",      # Model matrix
      Y            = "matrix",      # observed values of predicted variable
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
      initialize = function(formula, data) {
        tryCatch(
          {
            formula      <<- formula
            data         <<- data
            data_name    <<- deparse(substitute(data))
            X            <<- model.matrix(object = formula, data = data)
            Y            <<- cbind(data[, all.vars(formula)[1]])
            beta_hat     <<- solve(t(X) %*% X) %*% (t(X) %*% Y)
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
        "Print out the regression coefficients and coefficient names"

        beta_hat_df        <- rbind.data.frame(as.vector(beta_hat))
        names(beta_hat_df) <- rownames(beta_hat)

        cat("\nCall:\n",
            paste0("linreg(formula = ", format(formula), ", data = ", data_name, ")"),
            "\n\nCoefficients:\n ")

        print.data.frame(beta_hat_df, digits = 4, row.names = FALSE)
      },

      is_outlier = function(x) {
        "Return TRUE if data point x is an outlier based on the 1.5 rule"

        return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
      },

      plot = function() {
        "Plot two graphs - Residuals vs Fitted graph and Scale-Location"

        outliers <- ifelse(is_outlier(as.vector(E_hat)), rownames(data), as.numeric(NA))

        # Residuals vs Fitted graph
        f <- ggplot2::ggplot(data    = as.data.frame(cbind(Y_hat, E_hat)),
                             mapping = ggplot2::aes(x = Y_hat, y = E_hat)) +

          ggplot2::geom_point(shape = 1) +

          ggplot2::labs(title = "Residuals vs Fitted",
                            x = paste("Fitted values \nlinreg(", format(formula), ")"),
                            y = "Residuals") +

          ggplot2::geom_text(ggplot2::aes(label = outliers), na.rm = TRUE, nudge_x = 0.15, size = 3) +

          ggplot2::theme_classic() +

          ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                         panel.border = ggplot2::element_rect(fill = NA)) +

          ggplot2::geom_smooth(method = stats::loess, se = FALSE, col = 2)


        # Scale-Location graph
        g <- ggplot2::ggplot(data    = as.data.frame(cbind(Y_hat, E_hat_std)),
                             mapping = ggplot2::aes(x = Y_hat, y = E_hat_std)) +

          ggplot2::geom_point(shape = 1) +

          ggplot2::labs(title = "Scale-Location",
                            x = paste("Fitted values \nlinreg(", format(formula), ")"),
                            y = expression(sqrt("Standardized Residuals"))) +

          ggplot2::geom_text(ggplot2::aes(label = outliers), na.rm = TRUE, nudge_x = 0.15, size = 3) +

          ggplot2::theme_classic() +

          ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                         panel.border = ggplot2::element_rect(fill = NA)) +

          ggplot2::geom_smooth(method = stats::loess, se = FALSE, col = 2)

        # Plot the two graphs on the same window
        gridExtra::grid.arrange(f, g, ncol = 2, respect = TRUE)
      },

      resid = function() {
        "Return the vector of residuals"

        return(as.vector(E_hat))
      },

      pred = function() {
        "Return the predicted values"

        return(as.vector(Y_hat))
      },

      coef = function() {
        "Return the regression coefficients as a named vector"

        beta_hat_v <- round(x = as.vector(beta_hat), digits = 3)
        names(beta_hat_v) <-dimnames(beta_hat)[[1]]

        return(beta_hat_v)
      },

      summary = function() {
        "Print out the regression coefficients with their standard error, t-values and p-values,
         along with the estimate of residual standard error and degrees of freedom in the model"

        significance <- ifelse(p_values < 0.001, noquote("***"),
                               ifelse(p_values < 0.01, noquote("**"),
                                      ifelse(p_values < 0.05, noquote("*"),
                                             ifelse(p_values < 0.1, noquote("."), noquote(" ")))))

        coeff <- data.frame("Estimate"    = round(x = as.vector(beta_hat), digits = 5),
                            "Std.Error"   = round(x = std_e,    digits = 5),
                            "t value"     = round(x = t_values, digits = 2),
                            "P value"     = as.numeric(formatC(x = p_values, digits = 2)),
                            "signif.code" = significance)

        cat("\nCall:\n",
            paste0("linreg(formula = ", format(formula), ", data = ", data_name, ")\n\n"))

        print.data.frame(coeff)

        cat("\n---\n",
            "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.'0.1 ' ' 1",
            "\n\nResidual standard error:", round(x = sqrt(sigsq_hat), digits = 4),
            "on", df, "degrees of freedom")
      }
    )
  )

#' @name linreg_f
#' @aliases linreg_f
#' @title Linear Regression Function
#' @description This function takes two arguments- formula and data,
#'     and returns an object of class linreg which builds a linear
#'     regression model
#' @param formula formula (y ~ x)
#' @param data dataframe
#' @return An object of RC class linreg
#' @usage linreg_f(formula, data)
#' @examples linreg_f(formula = Petal.Length ~ Species, data = iris)
#' @export linreg_f

linreg_f <- function(formula, data) {

  linreg_obj <- linreg$new(formula = formula,
                           data    = data)
  return(linreg_obj)
}
