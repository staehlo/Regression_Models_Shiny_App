# Import libraries -------------------------------------------------------------
library(plotly)
library(shiny)
library(investr)


# Processing the input ---------------------------------------------------------

string_to_vector <- function(string){
  # Convert a string of space-separated numbers into a vector of numbers

  mylist <- strsplit(string, split = "\\s+")
  myvector <- mylist[[1]]
  myvector <- gsub(",", ".", myvector)
  # suppressWarnings: characters that cannot be converted to a number
  # will be silently converted to NA
  myvector <- suppressWarnings(as.numeric(myvector))
  
  return(myvector)
}


input_validator <- function(x_string, y_string) {
  # Check if the two input strings are valid
  # The return value is a string with either an "ok" statement or
  # a string of error messages
  
  x_vector <- string_to_vector(x_string)
  y_vector <- string_to_vector(y_string)

  msg <- "Error:"

  if (length(x_vector) == 0) {
    msg <- paste(msg, "No x-values were entered.")
  }
  if (length(x_vector) < 3) {
    msg <- paste(msg, "The input must have at least 3 values.",
                 "You entered", as.character(length(x_vector)), "values.")
  }
  if (length(x_vector) > 500) {
    msg <- paste(msg, "The input must not exceed 500 values.",
                 "You entered", as.character(length(x_vector)), "values.")
  }
  if (sum(is.na(x_vector)) > 0) {
    msg <- paste(msg, "At least one of the x-values could not be converted to",
                 "a number.")
  }
  if (length(y_vector) == 0) {
    msg <- paste(msg, "No y-values were entered.")
  }
  if (length(y_vector) < 3) {
    msg <- paste(msg, "The input must have at least 3 values.",
                 "You entered", as.character(length(y_vector)), "values.")
  }
  if (length(y_vector) > 500) {
    msg <- paste(msg, "The input must not exceed 500 values.",
                 "You entered", as.character(length(y_vector)), "values.")
  }
  if (sum(is.na(y_vector)) > 0) {
    msg <- paste(msg, "At least one of the y-values could not be converted to ",
                 "a number.")
  }
  if (length(x_vector) != length(y_vector)) {
    msg <- paste(msg, "X and Y have to be of the same length.",
                 "You entered",
                 as.character(length(x_vector)), "x-values and",
                 as.character(length(y_vector)), "y-values.")
  }
  if (msg == "Error:") {
    msg <- "input_is_valid"
  }
  
  return(msg)
}


strings_to_df <- function(x_string, y_string) {
  # Convert two input strings to a dataframe of numbers
  
  x_vector <- string_to_vector(x_string)
  y_vector <- string_to_vector(y_string)
  df <- data.frame(x = x_vector, y = y_vector)
  return(df)
}


# Legends for generic equations ------------------------------------------------

Legend_simpleLin <- function(){
  # Create legend for generic simple linear equation
  
  myHTMLstring <- helpText(
    paste(
      'with:',
      '\\(y_{i}\\) = the \\(i^{th}\\) of your y-values;',
      '\\(x_{i}\\) = the \\(i^{th}\\) of your x-values;',
      '\\(e_{i}\\) = an unknown error value for a particular',
      '\\(y_{i}\\) and \\(x_{i}\\);',
      '\\(\\beta_{0}\\) (the "intercept") = the expected y-value when x = 0;',
      '\\(\\beta_{1}\\) (the "slope") = the expected increase in y for a',
      'one-unit increase in x.'))
  return(myHTMLstring)
}


Legend_quadratic <- function(){
  # Create legend for generic quadratic equation

  myHTMLstring <- helpText(
    paste(
      'with:',
      '\\(y_{i}\\) = the \\(i^{th}\\) of your y-values;',
      '\\(x_{i}\\) = the \\(i^{th}\\) of your x-values;',
      '\\(e_{i}\\) = an unknown error value for a particular',
      '\\(y_{i}\\) and \\(x_{i}\\);',
      '\\(\\beta_{0}\\) (the "intercept") = the expected y-value when x = 0;',
      '\\(\\beta_{1}\\) and \\(\\beta_{2}\\) together indicate the expected',
      'increase in y for a one-unit increase in x. However, the relationship',
        'has become more complex than in the simple linear regression.'))
  return(myHTMLstring)
}


Legend_cubic <- function(){
  # Create legend for generic cubic equation
  
  myHTMLstring <- helpText(
    paste(
      'with:',
      '\\(y_{i}\\) = the \\(i^{th}\\) of your y-values;',
      '\\(x_{i}\\) = the \\(i^{th}\\) of your x-values;',
      '\\(e_{i}\\) = an unknown error value for a particular',
      '\\(y_{i}\\) and \\(x_{i}\\);',
      '\\(\\beta_{0}\\) (the "intercept") = the expected y-value when x = 0;',
      '\\(\\beta_{1}\\), \\(\\beta_{2}\\), and \\(\\beta_{3}\\) together',
      'indicate the expected increase in y for a one-unit increase in x.',
      'However, the relationship has become more complex than in the simple',
      'linear regression.'))
  return(myHTMLstring)
}


Legend_square_root <- function(){
  # Create legend for generic square root equation
  
  myHTMLstring <- helpText(
    paste(
      'with:',
      '\\(y_{i}\\) = the \\(i^{th}\\) of your y-values;',
      '\\(x_{i}\\) = the \\(i^{th}\\) of your x-values;',
      '\\(e_{i}\\) = an unknown error value for a particular',
      '\\(y_{i}\\) and \\(x_{i}\\);',
      '\\(\\beta_{0}\\) (the "intercept") = the expected y-value when x = 0;',
      '\\(\\beta_{1}\\) indicates the expected increase in y for a one-unit',
      'increase in \\(\\sqrt{x}\\).'))
  return(myHTMLstring)
}


Legend_logarithmic <- function(){
  # Create legend for generic logarithmic equation
  
  myHTMLstring <- helpText(
    paste(
      'with:',
      '\\(y_{i}\\) = the \\(i^{th}\\) of your y-values;',
      '\\(x_{i}\\) = the \\(i^{th}\\) of your x-values;',
      '\\(e_{i}\\) = an unknown error value for a particular',
      '\\(y_{i}\\) and \\(x_{i}\\);',
      '\\(\\beta_{0}\\) (the "intercept") = the expected y-value when x = 1;',
      'ln = natural logarithm function;',
      '\\(\\beta_{1}\\) indicates the expected increase in y for a one-unit',
      'increase in ln(x).'))
  return(myHTMLstring)
}


Legend_mich_ment <- function(){
  # Create legend for generic michael menten-style equation
  
  myHTMLstring <- helpText(
    paste(
      'with:',
      '\\(y_{i}\\) = the \\(i^{th}\\) of your y-values;',
      '\\(x_{i}\\) = the \\(i^{th}\\) of your x-values;',
      '\\(e_{i}\\) = an unknown error value for a particular',
      '\\(y_{i}\\) and \\(x_{i}\\);',
      '\\(V_{max}\\) = NOT YET FINISHED;',
      '\\(K_{M}\\) = NOT YET FINISHED;',
      '\\(const\\) = indicates the expected y-value when x = 0',
      '(\\(const\\) has the same function as \\(\\beta_{0}\\) in the linear',
      'equations).'))
  return(myHTMLstring)
}


# Regression models ------------------------------------------------------------

equation_simpleLin <- function(df){
  # Compute linear regression equation based on user input
  # Output is an html string with MathJax javascript elements
  
  fit <- lm(y ~ x, data = df)
  beta0 <- fit$coefficients['(Intercept)']
  beta0 <- round(beta0, 4)
  beta1 <- fit$coefficients['x']
  beta1 <- round(beta1, 4)
  mystring <- withMathJax(
    helpText(sprintf("$$ \\hat{y}_{i} = %s + %s x_{i} $$", beta0, beta1)))
  return(mystring)
}


equation_quadratic <- function(df){
  # Compute quadratic regression equation based on user input
  # Output is an html string with MathJax javascript elements
  
  fit <- lm(y ~ poly(x, 2, raw = TRUE), data = df)
  beta0 <- fit$coefficients['(Intercept)']
  beta0 <- round(beta0, 4)
  beta1 <- fit$coefficients['poly(x, 2, raw = TRUE)1']
  beta1 <- round(beta1, 4)
  beta2 <- fit$coefficients['poly(x, 2, raw = TRUE)2']
  beta2 <- round(beta2, 4)
  mystring <- withMathJax(
    helpText(sprintf("$$ \\hat{y}_{i} = %s + %s x_{i} + %s x_{i}^2 $$",
                     beta0, beta1, beta2)))
  return(mystring)
}


equation_cubic <- function(df){
  # Compute quadratic regression equation based on user input
  # Output is an html string with MathJax javascript elements
  
  fit <- lm(y ~ poly(x, 3, raw = TRUE), data = df)
  beta0 <- fit$coefficients['(Intercept)']
  beta0 <- round(beta0, 4)
  beta1 <- fit$coefficients['poly(x, 3, raw = TRUE)1']
  beta1 <- round(beta1, 4)
  beta2 <- fit$coefficients['poly(x, 3, raw = TRUE)2']
  beta2 <- round(beta2, 4)
  beta3 <- fit$coefficients['poly(x, 3, raw = TRUE)3']
  beta3 <- round(beta3, 4)
  mystring <- withMathJax(
    helpText(sprintf(paste("$$ \\hat{y}_{i} = %s + %s x_{i} + %s x_{i}^2 +",
                           "%s x_{i}^3 $$"),
                     beta0, beta1, beta2, beta3)))
  return(mystring)
}


equation_square_root <- function(df){
  # Compute square root equation based on user input
  # Output is an html string with MathJax javascript elements
  
  fit <- lm(y ~ sqrt(x), data = df)
  beta0 <- fit$coefficients['(Intercept)']
  beta0 <- round(beta0, 4)
  beta1 <- fit$coefficients['sqrt(x)']
  beta1 <- round(beta1, 4)
  mystring <- withMathJax(
    helpText(sprintf("$$ \\hat{y}_{i} = %s + %s \\sqrt{x_{i}} $$",
                     beta0, beta1)))
  return(mystring)
}


equation_logarithmic <- function(df){
  # Compute logarithmic equation based on user input
  # Output is an html string with MathJax javascript elements
  
  fit <- lm(y ~ log(x), data = df)
  beta0 <- fit$coefficients['(Intercept)']
  beta0 <- round(beta0, 4)
  beta1 <- fit$coefficients['log(x)']
  beta1 <- round(beta1, 4)
  mystring <- withMathJax(
    helpText(sprintf("$$ \\hat{y}_{i} = %s + %s ln(x_{i}) $$",
                     beta0, beta1)))
  return(mystring)
}


equation_mich_ment <- function(df){
  # Compute Michaelis Menten like equation based on user input
  # The function will select starting values for the iterative computation of
  # Vmax, Km, and const based on the user data.
  # Output is an html string with MathJax javascript elements
  
  Vmax.start <- max(df$y)
  # index of the nearest value to Vmax.start/2:
  # (R will choose the first one if there are several equally near values)
  index <- which.min(abs(df$y - Vmax.start/2))
  Km.start <- df$x[index]
  const.start <- 0
  
  fit <- nls(y ~ (Vmax*x/(Km+x)+const), data = df,
             start = list(Vmax = Vmax.start,
                          Km = Km.start,
                          const = const.start))
  sumfit <- summary(fit)
  Vmax <- sumfit$coefficients[1,"Estimate"]
  Km <- sumfit$coefficients[2,"Estimate"]
  const <- sumfit$coefficients[3,"Estimate"]
  Km <- round(Km, 4)
  Vmax <- round(Vmax, 4)
  const <- round(const, 4)
  mystring <- withMathJax(
    helpText(sprintf("$$ y_{i} = \\frac{ %s *x_{i}}{ %s + x_{i}} + %s $$",
                     Vmax, Km, const)))
  return(mystring)
}


# Tables -----------------------------------------------------------------------

goodness_of_fit_calculator_lm <- function(fit){
  # Compute goodness of fit measures for the linear models
  # Output is a dataframe
  
  sumfit <- summary(fit)
  rsquared <- sumfit$r.squared
  rsquaredadj <- sumfit$adj.r.squared
  
  observed <- fit$model$y
  predicted <- fit$fitted.values
  mae <- 1/length(observed) * sum(abs(predicted - observed))
  mae <- round(mae, 3)
  rmse <- sqrt(sum((predicted - observed)^2) / length(observed))
  rmse <- round(rmse, 3)
  
  output <- data.frame(measure = c('R-squared',
                                   'R-squared adjusted',
                                   'Mean Absolute Error (MAE)',
                                   'Root Mean Square Error (RMSE)'),
                       value = c(rsquared, rsquaredadj, mae, rmse))
  return(output)
}


goodness_of_fit_calculator_nls <- function(fit){
  # Compute goodness of fit measures for the nonlinear model
  # Output is a dataframe
  
  rsquared <- "Not available for non-linear models"
  rsquaredadj <- "Not available for non-linear models"
  
  observed <- fit$m$getEnv()$y
  predicted <- fit$m$fitted()
  mae <- 1/length(observed) * sum(abs(predicted - observed))
  mae <- round(mae, 3)
  rmse <- sqrt(sum((predicted - observed)^2) / length(observed))
  rmse <- round(rmse, 3)
  
  output <- data.frame(measure = c('R-squared',
                                   'R-squared adjusted',
                                   'Mean Absolute Error (MAE)',
                                   'Root Mean Square Error (RMSE)'),
                       value = c(rsquared, rsquaredadj, mae, rmse))
  return(output)
}


# Plots ------------------------------------------------------------------------

ggplot_subroutine <- function(df){
  # Generate ggplot-object based on a dataframe
  # The dataframe has to have the following columns:
  # x_value, observed, predicted, conf.int.lower, conf.int.upper,
  # pred.int.lower, pred.int.upper

  ggplot_obj <- ggplot() +
    geom_ribbon(data = subset(df, !is.na(pred.int.lower)),
                mapping = aes(x = x_value,
                              ymax = pred.int.upper,
                              ymin = pred.int.lower),
                fill = "blue",
                alpha = 0.1) +
    geom_ribbon(data = subset(df, !is.na(conf.int.lower)),
                mapping = aes(x = x_value,
                              ymax = conf.int.upper,
                              ymin = conf.int.lower),
                fill = "pink") +
    geom_point(data = subset(df, !is.na(observed)),
               mapping = aes(x = x_value, y = observed, colour = "observed")) +
    geom_point(data = subset(df, !is.na(observed)),
               mapping = aes(x = x_value, y = predicted,
                             colour = "predicted")) +
    geom_segment(data = subset(df, !is.na(observed)),
                 mapping = aes(x = x_value,
                               xend = x_value,
                               y = observed,
                               yend = predicted)) +
    geom_line(data = df,
              mapping = aes(x = x_value, y = predicted),
              colour = "red", linetype = "solid") +
    xlab("x values") +
    ylab("y values") +
    scale_color_manual(name = "Legend", # or name = element_blank()
                       labels = c("observed", "predicted"),
                       values = c("black", "red"))
  return(ggplot_obj)
}


create_plot <- function(df, fit){
  # Create a ggplot-object based on a dataframe with an x and y column
  # and a result object from either the lm or the nls function
  # This function will prepare a dataframe with x, y and further trendline
  # data as well as confidence and prediction intervals. It will then call
  # the ggplot_subroutine to create the plot

  df1 <- data.frame(x_value = df$x,
                    observed = df$y,
                    predicted = predFit(fit),
                    conf.int.lower = rep(NA, nrow(df)),
                    conf.int.upper = rep(NA, nrow(df)),
                    pred.int.lower = rep(NA, nrow(df)),
                    pred.int.upper = rep(NA, nrow(df)))
  
  # Interpolation x values to 100 data points:
  # This makes it possible to add a smooth-looking trendline as well as
  # smooth-looking confidence and prediction intervals
  df2 = data.frame(x = seq(from = min(df$x), to = max(df$x), length = 101))
  df2$observed <- rep(NA, nrow(df2))
  intervals <- predFit(fit, df2, interval = "confidence")
  df2$predicted <- intervals[, "fit"]
  df2$conf.int.lower <- intervals[, "lwr"]
  df2$conf.int.upper <- intervals[, "upr"]
  intervals <- predFit(fit, df2, interval = "prediction")
  df2$pred.int.lower <- intervals[, "lwr"]
  df2$pred.int.upper <- intervals[, "upr"]
  colnames(df2)[1] <- "x_value"
  res <- rbind(df1, df2)
  
  # create ggplot_object
  ggplot_obj <- ggplot_subroutine(res)
  return(ggplot_obj)
}
