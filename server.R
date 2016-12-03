if (!require(quantmod)) {
  stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}

# Reduce unnecessary warnings
options("getSymbols.warning4.0"=FALSE)

# Download data for a stock if needed, then return that data
require_symbol <- function(symbol, oct_flag, envir = parent.frame()) {
  # if october data is needed
  data_key <- NULL
  if(oct_flag) {
    from_date <- as.Date("10/01/2015", format="%m/%d/%Y")
    to_date <- as.Date("10/31/2015", format="%m/%d/%Y")
    data_key <- paste(symbol, "Oct", sep="")
    if (is.null(envir[[data_key]])) {
      envir[[data_key]] <- getSymbols(symbol, auto.assign = FALSE, from = from_date, to = to_date)
    }  
  } else {
    from_date_first <- as.Date("01/01/2015", format="%m/%d/%Y")
    to_date_first <- as.Date("09/30/2015", format="%m/%d/%Y")
    from_date_second <- as.Date("11/01/2015", format="%m/%d/%Y")
    to_date_second <- as.Date("12/31/2015", format="%m/%d/%Y")
    data_key <- paste(symbol, "yearButOct", sep="")
    if (is.null(envir[[data_key]])) {
      envir[[data_key]] <- merge(getSymbols(symbol, auto.assign = FALSE, from = from_date_first, to = to_date_first), 
                                 getSymbols(symbol, auto.assign = FALSE, from = from_date_second, to = to_date_second))
    }
  }
  envir[[data_key]]
}

shinyServer(function(input, output) {
  # the link below introduces how to use environments in R
  # http://adv-r.had.co.nz/Environments.html
  # enviornment in R works somewhat similar as dictionary in Python
  # in this case, symbol is the key and getSymbols(symbol) is the value
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Make a histogram for a symbol, with the settings from the inputs
  make_hist <- function(symbol, oct_flag) {
    date_range <- NULL
    if(oct_flag) {
      date_range <- "Date Range: Oct, 2015"
    } else {
      date_range <- "Date Range: Jan-Sep and Nov-Dec, 2015"
    }
    symbol_data <- require_symbol(symbol, oct_flag, symbol_env)
    head(symbol_data)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    ggplot(data = log_returns, aes(log_returns)) + 
    geom_histogram(binwidth = 1E-2, fill = 'black', color = 'white') +
    labs(x = "Log Returns", y = "Frequency", title = paste("Log Returns for", symbol, date_range, sep=" "))
  }
  
  output$october_aapl_return <- renderPlot({ make_hist("AAPL", TRUE) })
  output$october_msft_return <- renderPlot({ make_hist("MSFT", TRUE) })
  output$october_ibm_return <- renderPlot({ make_hist("IBM", TRUE) })
  output$october_goog_return <- renderPlot({ make_hist("GOOG", TRUE) })
  output$october_yhoo_return <- renderPlot({ make_hist("YHOO", TRUE) })
  
  output$year_but_october_aapl_return <- renderPlot({ make_hist("AAPL", FALSE) })
  output$year_but_october_msft_return <- renderPlot({ make_hist("MSFT", FALSE) })
  output$year_but_october_ibm_return <- renderPlot({ make_hist("IBM", FALSE) })
  output$year_but_october_goog_return <- renderPlot({ make_hist("GOOG", FALSE) })
  output$year_but_october_yhoo_return <- renderPlot({ make_hist("YHOO", FALSE) })
})
