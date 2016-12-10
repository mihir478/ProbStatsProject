if (!require("ggplot2")) 
  install.packages("ggplot2")
if(!require("quantmod")) 
  install.packages("quantmod")
if(!require("shiny")) 
  install.packages("shiny")

library(shiny)
library(quantmod)
library(ggplot2)

shinyUI(fluidPage(theme="theme.css",
  tags$head(
    tags$style(HTML("
h1 {
  font-weight: 500;
  line-height: 1.1;
}
hr {
  width: 2px; 
}
"))
  ),
  headerPanel("Election Volatility"),
  sidebarPanel(
    wellPanel(
      p(strong("Stock Picker")),
      checkboxInput(inputId = "stock_aapl", label = "Apple (AAPL)",     value = FALSE),
      checkboxInput(inputId = "stock_msft", label = "Microsoft (MSFT)", value = FALSE),
      sliderInput("cilevel", label = h5("Confidence Level %:"), min = 80, max = 99, value = 95, step=1)
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Returns", 
               # Use multiple conditions to control output
                conditionalPanel(condition = "input.stock_aapl",
                                div(plotOutput(outputId = "october_aapl_return")),
                                div(plotOutput(outputId = "year_but_october_aapl_return")),
                                hr()),
                conditionalPanel(condition = "input.stock_msft",
                                div(plotOutput(outputId = "october_msft_return")),
                                div(plotOutput(outputId = "year_but_october_msft_return")))),
      tabPanel("Q-Q", 
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "qqplot_aapl")),
                                hr()), 
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "qqplot_msft")))),
      tabPanel("C.I.",
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                p(strong("Confidence Interval for Mean of AAPL Returns")),
                                div(textOutput(outputId = "ci_aapl"))),
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                p(strong("Confidence Interval for Variances of AAPL Returns")),
                                div(textOutput(outputId = "ci_var_aapl")),
                                hr()),
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                p(strong("Confidence Interval for Mean of MSFT Returns")),
                                div(textOutput(outputId = "ci_msft"))),
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                p(strong("Confidence Interval for Variances of MSFT Returns")),
                                div(textOutput(outputId = "ci_var_msft")))),
      
      tabPanel("Means Comp.",
               conditionalPanel(condition = "input.stock_aapl && input.stock_msft",
                                br(),
                                strong("Test output:"),
                                tableOutput(outputId = "aapl_msft_means"))),
      tabPanel("Time Regression",
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "reg_onestock_data_plot_aapl")),
                                hr(),
                                div(dataTableOutput(outputId = "reg_onestock_coeffs_aapl")),
                                hr(),
                                div(plotOutput(outputId = "reg_onestock_resid_plot_aapl")),
                                hr()),
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "reg_onestock_data_plot_msft")),
                                hr(),
                                div(dataTableOutput(outputId = "reg_onestock_coeffs_msft")),
                                hr(),
                                div(plotOutput(outputId = "reg_onestock_resid_plot_msft")))),
      tabPanel("Multi Stock Regression",
               conditionalPanel(condition = "input.stock_aapl && input.stock_msft",
                                br(),
                                plotOutput(outputId = "reg_twostocks_data_plot_aapl_msft"),
                                div(dataTableOutput(outputId = "reg_twostocks_coeffs_aapl_msft")),
                                plotOutput(outputId = "reg_twostocks_resid_plot_aapl_msft")))
    )
  )
))
