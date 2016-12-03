if (!require(quantmod)) {
  stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}

from_dat <- as.Date("01/01/2015", format="%m/%d/%Y")
# Download data for a stock if needed, and return the data
require_symbol <- function(symbol, envir = parent.frame()) {
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol, auto.assign = FALSE, from = from_dat)
  }
  
  envir[[symbol]]
}

shinyServer(function(input, output) {
  # the link below introduces how to use environments in R
  # http://adv-r.had.co.nz/Environments.html
  # enviornment in R works somewhat similar as dictionary in Python
  # in this case, symbol is the key and getSymbols(symbol) is the value
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Make a histogram for a symbol, with the settings from the inputs
  make_hist <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    ggplot(data = log_returns, aes(log_returns)) + geom_histogram(binwidth = 1E-2, fill = 'white', color = 'black') + labs(x = "Log Returns", y = "Frequency", title = paste("Histogram of Log Returns", symbol, sep=" "))
    
    # chartSeries(symbol_data,
    #             name      = symbol,
    #             type      = input$chart_type,
    #             subset    = paste(input$daterange, collapse = "::"),
    #             log.scale = input$log_y,
    #             theme     = "white")
  }
  
  output$plot_aapl <- renderPlot({ make_hist("AAPL") })
  output$plot_msft <- renderPlot({ make_hist("MSFT") })
  output$plot_ibm  <- renderPlot({ make_hist("IBM")  })
  output$plot_goog <- renderPlot({ make_hist("GOOG") })
  output$plot_yhoo <- renderPlot({ make_hist("YHOO") })
})

