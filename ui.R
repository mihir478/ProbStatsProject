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
      checkboxInput(inputId = "stock_ibm",  label = "IBM (IBM)",        value = FALSE),
      checkboxInput(inputId = "stock_goog", label = "Google (GOOG)",    value = FALSE),
      checkboxInput(inputId = "stock_yhoo", label = "Yahoo (YHOO)",     value = FALSE)
    )
    
    # selectInput(inputId = "chart_type",
    #             label = "Chart type",
    #             choices = c("Candlestick" = "candlesticks",
    #                         "Matchstick" = "matchsticks",
    #                         "Bar" = "bars",
    #                         "Line" = "line")
    # )
    
    #dateRangeInput(inputId = "daterange", label = "Date range",
     #              start = Sys.Date() - 365, end = Sys.Date()),
    
    #checkboxInput(inputId = "log_y", label = "log y axis", value = FALSE)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Log Returns Inspector", 
               # use multiple conditions to control output
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "plot_aapl"))),
               
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "plot_msft"))),
               
               conditionalPanel(condition = "input.stock_ibm",
                                br(),
                                div(plotOutput(outputId = "plot_ibm"))),
               
               conditionalPanel(condition = "input.stock_goog",
                                br(),
                                div(plotOutput(outputId = "plot_goog"))),
               
               conditionalPanel(condition = "input.stock_yhoo",
                                br(),
                                plotOutput(outputId = "plot_yhoo"))
               ),
      tabPanel("October Effect")
      # tabPanel("Normal Probability Plot"),
      # tabPanel("Confidence Interval"),
      # tabPanel("Regression")
    )
  )
))