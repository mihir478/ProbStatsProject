if (!require(quantmod)) {
  stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}

# Reduce unnecessary warnings
options("getSymbols.warning4.0"=FALSE)

# Download data for a stock if needed, then return that data
makeRequestForYear <- function(symbol, oct_flag, year) {
  data <- NULL
  if(oct_flag) {
    from_date <- as.Date(paste("10/01/", year, sep=""), format="%m/%d/%Y")
    to_date <- as.Date(paste("10/31/", year, sep=""), format="%m/%d/%Y")
    data <- getSymbols(symbol, auto.assign = FALSE, from = from_date, to = to_date)
    }  
  else {
    from_date_first <- as.Date(paste("01/01/", year, sep=""), format="%m/%d/%Y")
    to_date_first <- as.Date(paste("09/30/", year, sep=""), format="%m/%d/%Y")
    from_date_second <- as.Date(paste("11/01/", year, sep=""), format="%m/%d/%Y")
    to_date_second <- as.Date(paste("12/01/", year, sep=""), format="%m/%d/%Y")
    data <- merge(getSymbols(symbol, auto.assign = FALSE, from = from_date_first, to = to_date_first), 
                  getSymbols(symbol, auto.assign = FALSE, from = from_date_second, to = to_date_second))
  }
  data
}

getDataForYear <- function(symbol, year, oct_flag, envir = parent.frame()) {
  data_key <- NULL
  if(oct_flag) {
    data_key <- paste(symbol, "Oct", sep="")
    } else {
    data_key <- paste(symbol, "yearButOct", sep="")
  }
  if (is.null(envir[[data_key]])) {
    envir[[data_key]] <- makeRequestForYear(symbol, oct_flag, year)
  }
  else {
    envir[[data_key]] <- getDataForYear(symbol, oct_flag, year)
  }
  envir[[data_key]]
}

require_symbol <- function(symbol, year, oct_flag, symbol_env) {
  data <- getDataForYear(symbol, year, oct_flag, symbol_env)
  data
}

shinyServer(function(input, output) {
  # the link below introduces how to use environments in R
  # http://adv-r.had.co.nz/Environments.html
  # enviornment in R works somewhat similar as dictionary in Python
  # in this case, symbol is the key and getSymbols(symbol) is the value
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Make a histogram for a symbol, with the settings from the inputs
  make_hist <- function(symbol, year, oct_flag) {
    date_range <- NULL
    if(oct_flag) {
      date_range <- paste("Date Range: Oct, ", year, sep="")
    } else {
      date_range <- paste("Date Range: Jan-Sep and Nov-Dec, ", year, sep="")
    }
    symbol_data <- require_symbol(symbol, year, oct_flag, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    ggplot(data = log_returns, aes(log_returns)) + 
    geom_histogram(binwidth = 1E-2, fill = 'black', color = 'white') +
    labs(x = "Log Returns", y = "Frequency", title = paste("Log Returns for", symbol, date_range, sep=" "))
  }
  
  output$october_aapl_return <- renderPlot({ make_hist("AAPL", 2015, TRUE) })
  output$october_msft_return <- renderPlot({ make_hist("MSFT", 2015, TRUE) })
  
  output$year_but_october_aapl_return <- renderPlot({ make_hist("AAPL", 2015, FALSE) })
  output$year_but_october_msft_return <- renderPlot({ make_hist("MSFT", 2015, FALSE) })
})
