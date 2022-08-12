# Import libraries -------------------------------------------------------------
library(shiny)
library(shinythemes)
library(plotly)


# Import functions -------------------------------------------------------------
source("subscripts/functions.R")


# User Interface ---------------------------------------------------------------
ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  titlePanel("Simple regression models for two variables"),

  ## sidebarLayout -------------------------------------------------------------
  sidebarLayout(
    
    sidebarPanel(h3("Your Input"),
      textInput(label = "x values", inputId = "x_string",
                value = "1  2.0  3,0  4  5.0"),
      textInput(label = "y values", inputId = "y_string",
                value = "1  2,0  3.0  4  5,5"),
      strong("Explanation:"),
      br(),
      paste("Please enter enter two series of real numbers that meet the",
            "following conditions:"),
      tags$ul(
        tags$li("Both series have to contain the same number of values"),
        tags$li("Each series must have at least 3 values"),
        tags$li("Each series can have up to 500 values"),
        tags$li(paste("Individual numbers have to be separated by space or tab",
                      "from each other")),
        tags$li("Decimal separator can be either comma or dot"),
        tags$li(paste("You can also copy and paste columns or rows from a",
                      "spreadsheet"))
        ),
      ), # end sidebarPanel

    ## mainPanel ---------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        ### tabPanel "Regression" ----------------------------------------------
        tabPanel("Regression",
          HTML("What kind of regression model do you want to apply?"),
          radioButtons(inputId = "model",
                       label = "",
                       choices = c("simple linear" = "simple_linear",
                                   "quadratic" = "quadratic",
                                   "cubic" = "cubic",
                                   "square root" = "square_root",
                                   "logarthmic" = "logarithmic",
                                   "Michaelis Menten with additional constant" =
                                     "mich_ment")),
          br(),
          HTML("Your selection:"),
          span(textOutput("your_selection",
                     inline = TRUE,
                     container = tags$b), style = "color:#ffccff"),
          br(),br(),
          tags$u("generic equation:"),
          uiOutput("generic_equation"),
          uiOutput("legend_for_generic_equation"),
  
          tags$u("estimated equation based on your data:"),
          span(textOutput("error_message"), style = "color:red"),
          uiOutput("specific_equation"),
          uiOutput("legend_for_specific"),
  
          tags$u("Goodness of fit of the estimated equation:"),
          tableOutput('table'),

          span(textOutput("polynomial_plot_error_message"),
               style = "color:red"),
          plotlyOutput("myplotly"),
          HTML("further legend:"), br(),
          img(src = 'color_of_conf_interval.png'),"confidence interval", br(),
          img(src = 'color_of_pred_interval.png'),"prediction interval",
          br(), br()
          ), # end tabPanel ("Regression")

        ### tabPanel "About" ---------------------------------------------------
        tabPanel("About",
          "This App was a training project to get familiar with the ",
          tags$a(href = paste0("https://cran.r-project.org/web/packages/",
                               "shiny/index.html"),
                 "\"Shiny\" R package."),
          br(),br(),
          " The app asks the user to input a series of numbers separated by",
          " \"spaces\". This is an unusual way to enter data.",
          " However, this makes it possible to directly copy data from a",
          " speadsheet to the app.",
          " The script uses a regular expression that will also parse such",
          " data correctly.",
          br(), br(),
          " It can be tricky to find input data that can be analysed",
          " by the Michaelis Menten-style equation.",
          " You can try a square root model as a start and see how well",
          " the non-linear model fits these data:", br(),
          " x-values:", br(),
          " 1  4  9 16 25", br(),
          " y-values:", br(),
          " 1  2  3  4  5",
          br(), br(),
          
          
          "The code can be found on GitHub:", br(),
          tags$a(href = paste0("https://github.com/staehlo/",
                               "Shiny_Regression_Models_App"),
                 "GitHub.com/staehlo/shiny_regression_models_app"),
          br(), br(),

          "--- Created in summer 2022 ---"

        )# end tabPanel ("About")
      ) # end tabsetPanel
    ) # end mainPanel 
  ) # end sidebarLayout
) # end fluidPage


# Server -----------------------------------------------------------------------
server <- function(input, output) {

  ## your selection ------------------------------------------------------------
  output$your_selection <- renderText(
    if (input$model == "simple_linear") {
      "simple linear regression"
    } else if (input$model == "quadratic") {
      "quadratic regression"
    } else if (input$model == "cubic") {
      "cubic regression"
    } else if (input$model == "square_root") {
      "square root regression"
    } else if (input$model == "logarithmic") {
      "logarithmic regression"
    } else if (input$model == "mich_ment") {
      "Michaelis Menten with additional constant"
    }
  )

  ## generic equation ----------------------------------------------------------
  output$generic_equation <- renderUI(
    if (input$model == "simple_linear") {
      helpText("$$y_{i} = \\beta_{0} + \\beta_{1} x_{i} + \\epsilon_{i}$$")
    } else if (input$model == "quadratic") {
      helpText(paste("$$y_{i} = \\beta_{0} + \\beta_{1} x_{i} + \\beta_{2}",
                     " x_{i}^2 + \\epsilon_{i}$$"))
    } else if (input$model == "cubic") {
      helpText(paste("$$y_{i} = \\beta_{0} + \\beta_{1} x_{i} + \\beta_{2}",
                     "x_{i}^2 + \\beta_{3} x_{i}^3 + \\epsilon_{i}$$"))
    } else if (input$model == "square_root") {
      helpText(paste("$$y_{i} = \\beta_{0} + \\beta_{1} \\sqrt{x_{i}} +",
                     "\\epsilon_{i}$$"))
    } else if (input$model == "logarithmic") {
      helpText(paste("$$y_{i} = \\beta_{0} + \\beta_{1} ln(x_{i}) +",
                     "\\epsilon_{i}$$"))
    } else if (input$model == "mich_ment") {
        helpText(paste0("$$y_{i} = \\frac{V_{max}*x_{i}}{K_{M} + x_{i}} +",
                        "const + \\epsilon_{i}$$"))
    }
  )

  ## legend for generic --------------------------------------------------------
  output$legend_for_generic_equation <- renderUI({
    
    if (input$model == "simple_linear") {
      legend <- Legend_simpleLin()
    } else if (input$model == "quadratic") {
      legend <- Legend_quadratic()
    } else if (input$model == "cubic") {
      legend <- Legend_cubic()
    } else if (input$model == "square_root") {
      legend <- Legend_square_root()
    } else if (input$model == "logarithmic") {
      legend <- Legend_logarithmic()
    } else if (input$model == "mich_ment") {
      legend <- Legend_mich_ment()
    }
    withMathJax(legend)
  })

  ## error message -------------------------------------------------------------
  output$error_message <- renderText({
    msg <- input_validator(input$x_string, input$y_string)
    if (msg == "input_is_valid") {
      ""
    } else {
      msg
    }
  })

## specific equation ---------------------------------------------------------
  output$specific_equation <- renderUI({
#    df <- df_creator()

#    # run the code only if the received data did NOT yield an error message:
#    req(!is.character(df))

    msg <- input_validator(input$x_string, input$y_string)
    if (msg == "input_is_valid") {
      df <- strings_to_df(input$x_string, input$y_string)
      if (input$model == "simple_linear") {
        equation_simpleLin(df)
      } else if (input$model == "quadratic") {
        equation_quadratic(df)
      } else if (input$model == "cubic") {
        equation_cubic(df)
      } else if (input$model == "square_root") {
        equation_square_root(df)
      } else if (input$model == "logarithmic") {
        equation_logarithmic(df)
      } else if (input$model == "mich_ment") {
        tryCatch(
          expr = { equation_mich_ment(df)},
          error = function(e){"*** not possible for these data ***"}
          )
      }
    }
  })

  ## legend for specific -------------------------------------------------------
  # one legend is sufficient for all five specific equations
  output$legend_for_specific <- renderUI({
    msg <- input_validator(input$x_string, input$y_string)
    if (msg == "input_is_valid") {
      helpText(paste('with:',
                     '\\(x_{i}\\) = the \\(i^{th}\\) of your x-values;',
                     '\\(\\hat{y}_{i}\\) ("y hat") = the estimate for the',
                     '\\(i^{th}\\) of your y-values;'))
    }
  })

  ## goodness of fit -----------------------------------------------------------
  output$table <- renderTable({
    msg <- input_validator(input$x_string, input$y_string)
    if (msg != "input_is_valid") {
      mae <- paste0('Mean Absolute Error (<a href="https://en.wikipedia.org/',
                    'wiki/Mean_absolute_error">MAE</a>)')
      rmse <- paste0('Root Mean Square Error (<a href="https://en.wikipedia.',
                     'org/wiki/Root-mean-square_deviation">RMSE</a>)')
      data.frame(measure = c('R-squared',
                             'R-squared adjusted',
                             mae,
                             rmse),
                 value = c(NA, NA, NA, NA))
    } else {
      df <- strings_to_df(input$x_string, input$y_string)
      if (input$model == "simple_linear") {
        fit <- lm(y ~ x, data = df)
        goodness_of_fit_calculator_lm(fit)
      } else if (input$model == "quadratic") {
        fit <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = df)
        goodness_of_fit_calculator_lm(fit)
      } else if (input$model == "cubic") {
        fit <- lm(y ~ poly(x, degree = 3, raw = TRUE), data = df)
        goodness_of_fit_calculator_lm(fit)
      } else if (input$model == "square_root") {
        fit <- lm(y ~ sqrt(x), data = df)
        goodness_of_fit_calculator_lm(fit)
      } else if (input$model == "logarithmic") {
        fit <- lm(y ~ log(x), data = df)
        goodness_of_fit_calculator_lm(fit)
      } else if (input$model == "mich_ment") {
        tryCatch(
          expr = {
            Vmax.start <- max(df$y)
            # index of the nearest value to Vmax.start/2:
            # (R will choose the first one if there are several equally near
            #  values)
            index <- which.min(abs(df$y - Vmax.start/2))
            Km.start <- df$x[index]
            const.start <- 0
            fit <- nls(y ~ (Vmax*x/(Km+x)+const), data = df,
                       start = list(Vmax = Vmax.start,
                                    Km = Km.start,
                                    const = const.start))
            return(goodness_of_fit_calculator_nls(fit))
          },
          error = function(e){"***not possible for these data***"}
      )}
    }
  }, colnames = FALSE, digits = 3, sanitize.text.function = function(x) x)

  ## polynomial plotting error -------------------------------------------------
  output$polynomial_plot_error_message <- renderText({
    msg <- input_validator(input$x_string, input$y_string)
    if (msg != "input_is_valid") {
      ""
    } else {
      df <- strings_to_df(input$x_string, input$y_string)
      if (input$model == "quadratic") {
        if (nrow(df) < 4) {
          paste("The quadratic model can only be plotted if there are more",
                "than 3 values for x and y.")
        }
      } else if (input$model == "cubic") {
        if (nrow(df) < 5) {
          paste("The cubic model can only be plotted if there are more",
                "than 4 values for x and y.")
        }
      }
    }
  })
  
  ## plotly chart --------------------------------------------------------------  
  output$myplotly <- renderPlotly({
    msg <- input_validator(input$x_string, input$y_string)
    if (msg != "input_is_valid") {
      ggplot()
    } else {
      df <- strings_to_df(input$x_string, input$y_string)
      if (input$model == "simple_linear") {
        fit <- lm(y ~ x, data = df)
        create_plot(df, fit)
      } else if (input$model == "quadratic") {
        fit <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = df)
        # plot will fail as R cannot calculate the confidence and 
        # prediction interval if there are less than 4 rows of data
        if (nrow(df) > 3) {
          create_plot(df, fit)
        } else {
          ggplot()
        }
      } else if (input$model == "cubic") {
        fit <- lm(y ~ poly(x, degree = 3, raw = TRUE), data = df)
        # plot will fail as R cannot calculate the confidence and 
        # prediction interval if there are less than 5 rows of data
        if (nrow(df) > 4) {
          create_plot(df, fit)
        } else {
          ggplot()
        }
      } else if (input$model == "square_root") {
        fit <- lm(y ~ sqrt(x), data = df)
        create_plot(df, fit)
      } else if (input$model == "logarithmic") {
        fit <- lm(y ~ log(x), data = df)
        create_plot(df, fit)
      } else if (input$model == "mich_ment") {
        tryCatch(
          expr = {
            Vmax.start <- max(df$y)
            # index of the nearest value to Vmax.start/2:
            # (R will choose the first one if there are several equally near
            #  values)
            index <- which.min(abs(df$y - Vmax.start/2))
            Km.start <- df$x[index]
            const.start <- 0
            fit <- nls(y ~ (Vmax*x/(Km+x)+const), data = df,
                       start = list(Vmax = Vmax.start,
                                    Km = Km.start,
                                    const = const.start))
            create_plot(df, fit)
          },
          error = function(e){ggplot()}
        )
      }
    }
  })
}


# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
