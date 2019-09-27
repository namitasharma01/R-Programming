#' Advanced Programming in R - Lab 4
#' Linear Regression using ordinary least squares

linreg <-
  setRefClass(
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

      print = function() {
        beta_hat_v <- round(x = as.vector(beta_hat), digits = 5)
        names(beta_hat_v) <- dimnames(beta_hat)[[1]]

        cat("\nCall:\n",
            paste0("linreg(formula = ", format(formula), ", data = ", deparse(substitute(data)), ")"),
            "\n\nCoefficients:\n")

        return(beta_hat_v)
      },

      is_outlier = function(x) {
        return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
      },

      plot = function() {
        f_outliers <- ifelse(is_outlier(as.vector(E_hat)), rownames(data), as.numeric(NA))
        g_outliers <- ifelse(is_outlier(as.vector(E_hat_std)), rownames(data), as.numeric(NA))

        f <- ggplot2::ggplot(data    = as.data.frame(cbind(Y_hat, E_hat)),
                             mapping = ggplot2::aes(x = Y_hat, y = E_hat)) +
             ggplot2::geom_point(shape = 1) +
             ggplot2::labs(title = "Residuals vs Fitted",
                               x = paste("Fitted values \nlinreg(", format(formula), ")"),
                               y = "Residuals") +
             ggplot2::theme_classic() +
             ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                            panel.border = ggplot2::element_rect(fill = NA)) +
                            #plot.margin  = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "mm")) +
             #ggplot2::stat_summary(fun.y = median, color = "red", geom = "line", size = 1) +
             #ggplot2::stat_summary(fun.y = mean, color = "black", geom = "line", linetype = "dotted", size = 1) +
             ggplot2::geom_text(ggplot2::aes(label = f_outliers), na.rm = TRUE, hjust = -0.3, size = 3) +
             ggplot2::geom_smooth(method = lowess(as.data.frame(cbind(Y_hat, E_hat))), col = 2)
             #graphics::lines(lowess(as.data.frame(cbind(Y_hat, E_hat))), col = 2)


        g <- ggplot2::ggplot(data    = as.data.frame(cbind(Y_hat, E_hat_std)),
                             mapping = ggplot2::aes(x = Y_hat, y = E_hat_std)) +
             ggplot2::geom_point(shape = 1) +
             ggplot2::labs(title = "Scale-Location",
                               x = paste("Fitted values \nlinreg(", format(formula), ")"),
                               y = expression(sqrt("Standardized Residuals"))) +
             ggplot2::theme_classic() +
             ggplot2::theme(plot.title   = ggplot2::element_text(hjust = 0.5),
                            panel.border = ggplot2::element_rect(fill = NA)) +
             ggplot2::stat_summary(fun.y = mean , color = "red" , geom = "line" , size = 1) +
             ggplot2::geom_text(ggplot2::aes(label = g_outliers), na.rm = TRUE, hjust = -0.3)

        gridExtra::grid.arrange(f, g, nrow = 2, respect = TRUE)
      },

      resid = function() {
        return(as.vector(E_hat))
      },

      pred = function() {
        return(as.vector(Y_hat))
      },

      coef = function() {
        beta_hat_v <- round(x = as.vector(beta_hat), digits = 5)
        names(beta_hat_v) <-dimnames(beta_hat)[[1]]

        return(beta_hat_v)
      },

      summary = function() {
        coeff <- data.frame("Estimate"   = coef(),
                            "Std. Error" = round(x = std_e,    digits = 5),
                            "t value"    = round(x = t_values, digits = 2),
                            "P value"    = as.numeric(formatC(x = p_values, digits = 0)))

        cat("\nCall:\n",
            paste0("linreg(formula = ", format(formula), ", data = ", deparse(substitute(data)), ")\n\n"))

        print.data.frame(coeff)
        cat("\nResidual standard error:", round(x = sqrt(sigsq_hat), digits = 4),
            "on", df, "degrees of freedom")
      }
    )
  )

linreg1 <- function(formula, data) {
  linreg_obj <- linreg$new(formula = formula,
                           data    = data)
  return(linreg_obj)
}

