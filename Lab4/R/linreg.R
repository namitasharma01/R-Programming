#' Advanced Programming in R - Lab 4
#' Linear Regression using ordinary least squares

#' @name linreg
#' @aliases linreg
#' @title Linear Regression Class
#' @description This function finds the shortest distance from a source
#'              node to all the other nodes in a graph
#' @param formula formula (y ~ x)
#' @param data dataframe
#' @return Numeric vector representing the shortest distance form
#'         init_node to all the other nodes in graph
#' @usage linreg_f(formula, data)
#' @examples linreg_f(formula = Petal.Length ~ Species, data = iris)
#' @export

linreg <-
  methods::setRefClass(
    "linreg",

    fields = list(
      formula      = "formula",
      data         = "data.frame",
      X            = "matrix",      # Model matrix
      Y            = "numeric",     # observed values of predicted variable
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
            X            <<- model.matrix(object = formula, data = data)
            Y            <<- data[, all.vars(formula)[1]]
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
            stop("Unexpected inputs")
          }
        )
      },

      print = function() {
        beta_hat_v <- round(x = as.vector(beta_hat), digits = 3)
        names(beta_hat_v) <- dimnames(beta_hat)[[1]]

        cat("\nCall:\n",
            paste0("linreg(formula = ", format(formula), ", data = iris", ")"),
            "\n\nCoefficients:\n")

        return(beta_hat_v)
      },

      is_outlier = function(x) {
        return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
      },

      plot = function() {
        f_outliers <- ifelse(is_outlier(as.vector(E_hat)), rownames(data), as.numeric(NA))
        g_outliers <- ifelse(is_outlier(as.vector(E_hat_std)), rownames(data), as.numeric(NA))

        # Residuals vs Fitted graph
        f <- ggplot2::ggplot(data    = as.data.frame(cbind(Y_hat, E_hat)),
                             mapping = ggplot2::aes(x = Y_hat, y = E_hat)) +

          ggplot2::geom_point(shape = 1) +

          ggplot2::labs(title = "Residuals vs Fitted",
                        x = paste("Fitted values \nlinreg(", format(formula), ")"),
                        y = "Residuals") +

          ggplot2::geom_text(ggplot2::aes(label = f_outliers), na.rm = TRUE, hjust = -0.3, size = 3) +

          ggplot2::theme_classic() +

          ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                         panel.border = ggplot2::element_rect(fill = NA)) +

          ggplot2::geom_smooth(method = "loess", se = FALSE, col = 2)


        # Scale-Location graph
        g <- ggplot2::ggplot(data    = as.data.frame(cbind(Y_hat, E_hat_std)),
                             mapping = ggplot2::aes(x = Y_hat, y = E_hat_std)) +

          ggplot2::geom_point(shape = 1) +

          ggplot2::labs(title = "Scale-Location",
                        x = paste("Fitted values \nlinreg(", format(formula), ")"),
                        y = expression(sqrt("Standardized Residuals"))) +

          ggplot2::geom_text(ggplot2::aes(label = g_outliers), na.rm = TRUE, hjust = -0.3) +

          ggplot2::theme_classic() +

          ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                         panel.border = ggplot2::element_rect(fill = NA)) +

          ggplot2::geom_smooth(method = "loess", se = FALSE, col = 2)

        # Plot the two graphs on the same window
        gridExtra::grid.arrange(f, g, nrow = 2, respect = TRUE)
      },

      resid = function() {
        return(as.vector(E_hat))
      },

      pred = function() {
        return(as.vector(Y_hat))
      },

      coef = function() {
        beta_hat_v <- round(x = as.vector(beta_hat), digits = 3)
        names(beta_hat_v) <-dimnames(beta_hat)[[1]]

        return(beta_hat_v)
      },

    summary = function() {
        significance <- ifelse(p_values < 0.001, noquote("***"),
                               ifelse(p_values < 0.01, noquote("**"),
                                      ifelse(p_values < 0.05, noquote("*"),
                                             ifelse(p_values < 0.1, noquote("."), noquote(" ")))))

        coeff <- data.frame("Estimate"    = round(x = as.vector(beta_hat), digits = 5),
                            "Std. Error"  = round(x = std_e,    digits = 5),
                            "t value"     = round(x = t_values, digits = 2),
                            "P value"     = as.numeric(formatC(x = p_values, digits = 2)),
                            "signif.code" = significance)

        cat("\nCall:\n",
            paste0("linreg(formula = ", format(formula), ", data = iris", ")\n\n"))

        print.data.frame(coeff)

        cat("\n---\n",
            "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
            "\n\nResidual standard error:", round(x = sqrt(sigsq_hat), digits = 4),
            "on", df, "degrees of freedom")
      }
    )
  )

#' @name linreg_f
#' @aliases linreg_f
#' @title Linear Regression Function
#' @description This function finds the shortest distance from a source
#'              node to all the other nodes in a graph
#' @param formula formula (y ~ x)
#' @param data dataframe
#' @return An object of RC class linreg
#' @usage linreg_f(formula, data)
#' @examples linreg_f(formula = Petal.Length ~ Species, data = iris)
#' @export

linreg_f <- function(formula, data) {
  linreg_obj <- linreg$new(formula = formula,
                           data    = data)
  return(linreg_obj)
}
