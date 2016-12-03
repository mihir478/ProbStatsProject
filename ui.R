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
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Log Returns Inspector", 
               # use multiple conditions to control output
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "october_aapl_return")),
                                div(plotOutput(outputId = "year_but_october_aapl_return"))),
               
               conditionalPanel(condition = "input.stock_msft",
                                br(),
                                div(plotOutput(outputId = "october_msft_return")),
                                div(plotOutput(outputId = "year_but_october_msft_return"))),
               
               conditionalPanel(condition = "input.stock_ibm",
                                br(),
                                div(plotOutput(outputId = "october_ibm_return")),
                                div(plotOutput(outputId = "year_but_october_ibm_return"))),
               
               conditionalPanel(condition = "input.stock_goog",
                                br(),
                                div(plotOutput(outputId = "october_goog_return")),
                                div(plotOutput(outputId = "year_but_october_goog_return"))),
               
               conditionalPanel(condition = "input.stock_yhoo",
                                br(),
                                div(plotOutput(outputId = "october_yhoo_return")),
                                div(plotOutput(outputId = "year_but_october_yhoo_return")))
               )
      # tabPanel("Normal Probability Plot"),
      # tabPanel("Confidence Interval"),
      # tabPanel("Regression")
    )
  )
))
