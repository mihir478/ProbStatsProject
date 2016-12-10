if (!require("ggplot2")) 
  install.packages("ggplot2")
if(!require("quantmod")) 
  install.packages("quantmod")

library(shiny)
library(quantmod)
library(ggplot2)

shinyUI(pageWithSidebar(
  headerPanel("October Effect"),
  
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
      tabPanel("General Log Returns Inspector", 
               #use multiple conditions to control output
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "plot_aapl"))),
               
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "plot_msft")))),
      tabPanel("October Effect - Log Returns Inspector", 
               # use multiple conditions to control output
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "october_aapl_return")),
                                div(plotOutput(outputId = "year_but_october_aapl_return"))),
               
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "october_msft_return")),
                                div(plotOutput(outputId = "year_but_october_msft_return")))
      ),
      tabPanel("Q-Q Plot", 
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "qqplot_aapl"))), 
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "qqplot_msft")))),
      tabPanel("Confidence Interval for Mean and Variance of Log Returns",
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                p(strong("Cofidence Interval for Mean of AAPL Returns")),
                                div(textOutput(outputId = "ci_aapl"))),
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                p(strong("Cofidence Interval for Mean of MSFT Returns")),
                                div(textOutput(outputId = "ci_msft"))),
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                p(strong("Cofidence Interval for Variances of AAPL Returns")),
                                div(textOutput(outputId = "ci_var_aapl"))),
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                p(strong("Cofidence Interval for Variances of MSFT Returns")),
                                div(textOutput(outputId = "ci_var_msft")))),
      
      tabPanel("Two Stocks Means Comparision",
               conditionalPanel(condition = "input.stock_aapl && input.stock_msft",
                                br(),
                                strong("Test output:"),
                                tableOutput(outputId = "aapl_msft_means"))),
      tabPanel("Single Stock Returns Regression against Time",
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "reg_onestock_data_plot_aapl")),
                                div(dataTableOutput(outputId = "reg_onestock_coeffs_aapl")),
                                div(plotOutput(outputId = "reg_onestock_resid_plot_aapl"))),
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "reg_onestock_data_plot_msft")),
                                div(dataTableOutput(outputId = "reg_onestock_coeffs_msft")),
                                div(plotOutput(outputId = "reg_onestock_resid_plot_msft")))),
      tabPanel("Two Stocks Returns Regression against Each Other",
               conditionalPanel(condition = "input.stock_aapl && input.stock_msft",
                                br(),
                                plotOutput(outputId = "reg_twostocks_data_plot_aapl_msft"),
                                div(dataTableOutput(outputId = "reg_twostocks_coeffs_aapl_msft")),
                                plotOutput(outputId = "reg_twostocks_resid_plot_aapl_msft")))
    )
  )
))
