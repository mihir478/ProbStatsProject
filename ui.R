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
  headerPanel("Trump Effect"),
  sidebarPanel(
    wellPanel(
      p(strong("Stock Picker")),
      checkboxInput(inputId = "stock_aapl", label = "Apple (AAPL)",     value = FALSE),
      checkboxInput(inputId = "stock_xom", label = "Exxon Mobile (XOM)", value = FALSE),
      sliderInput("cilevel", label = h5("Confidence Level %:"), min = 80, max = 99, value = 95, step=1)
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Returns", 
               # Use multiple conditions to control output
                conditionalPanel(condition = "input.stock_aapl",
                                div(plotOutput(outputId = "pre_election_news_aapl_return")),
                                div(plotOutput(outputId = "post_election_news_aapl_return")),
                                hr()),
                conditionalPanel(condition = "input.stock_xom",
                                div(plotOutput(outputId = "pre_election_news_xom_return")),
                                div(plotOutput(outputId = "post_election_news_xom_return")))),
      tabPanel("C.I.",
               conditionalPanel(condition = "input.stock_aapl",
                                p(strong("Confidence Intervals")),
                                br(),
                                p(strong("AAPL Mean Pre-Trump")),
                                div(textOutput(outputId = "ci_aapl_pre_election_news")),
                                br(),
                                p(strong("AAPL Mean Post-Trump")),
                                div(textOutput(outputId = "ci_aapl_post_election_news")),
                                br(),
                                p(strong("AAPL Var. Pre-Trump")),
                                div(textOutput(outputId = "ci_var_aapl_pre_election_news")),
                                br(),
                                p(strong("AAPL Var. Post-Trump")),
                                div(textOutput(outputId = "ci_var_aapl_post_election_news")),
                                hr()),
               conditionalPanel(condition = "input.stock_xom",
                                br(),
                                p(strong("XOM Mean Pre-Trump")),
                                div(textOutput(outputId = "ci_xom_pre_election_news")),
                                br(),
                                p(strong("XOM Mean Post-Trump")),
                                div(textOutput(outputId = "ci_xom_post_election_news")),
                                br(),
                                p(strong("XOM Var. Pre-Trump")),
                                div(textOutput(outputId = "ci_var_xom_pre_election_news")),
                                br(),
                                p(strong("XOM Var. Post-Trump")),
                                div(textOutput(outputId = "ci_var_xom_post_election_news")))),
      tabPanel("Means Comp.",
               conditionalPanel(condition = "input.stock_aapl && input.stock_xom",
                                br(),
                                strong("Test output:"),
                                tableOutput(outputId = "aapl_xom_means"))),
      tabPanel("Q-Q", 
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "qqplot_aapl")),
                                hr()), 
               conditionalPanel(condition = "input.stock_xom",
                                br(),
                                div(plotOutput(outputId = "qqplot_xom")))),
      tabPanel("Time Regression",
               conditionalPanel(condition = "input.stock_aapl",
                                br(),
                                div(plotOutput(outputId = "reg_onestock_data_plot_aapl")),
                                hr(),
                                div(dataTableOutput(outputId = "reg_onestock_coeffs_aapl")),
                                hr(),
                                div(plotOutput(outputId = "reg_onestock_resid_plot_aapl")),
                                hr()),
               conditionalPanel(condition = "input.stock_xom",
                                br(),
                                div(plotOutput(outputId = "reg_onestock_data_plot_xom")),
                                hr(),
                                div(dataTableOutput(outputId = "reg_onestock_coeffs_xom")),
                                hr(),
                                div(plotOutput(outputId = "reg_onestock_resid_plot_xom")))),
      tabPanel("Stock Regression",
               conditionalPanel(condition = "input.stock_aapl && input.stock_xom",
                                br(),
                                plotOutput(outputId = "reg_twostocks_data_plot_aapl_xom"),
                                div(dataTableOutput(outputId = "reg_twostocks_coeffs_aapl_xom")),
                                plotOutput(outputId = "reg_twostocks_resid_plot_aapl_xom")))
    )
  )
))
