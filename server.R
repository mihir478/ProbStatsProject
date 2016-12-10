# Reduce unnecessary warnings from scaling an xts data structure
options("getSymbols.warning4.0"=FALSE)

# Makes data request to fetch stock data for a symbol over a year
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

# Caches data to avoid expensive data requests
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

# Makes data requests and caches responses for a symbol over a year
require_symbol <- function(symbol, envir = parent.frame()) {
  from_date <- as.Date("01/01/2015", format="%m/%d/%Y")
  to_date <- as.Date("12/31/2015", format="%m/%d/%Y")
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol, auto.assign = FALSE, from = from_date, to = to_date)
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
  
  # Make a histogram for a symbol to investigate october effect
  make_hist_oct <- function(symbol, year, oct_flag) {
    date_range <- NULL
    if(oct_flag) {
      date_range <- paste("- Date Range: Oct, ", year, sep="")
    } else {
      date_range <- paste("- Date Range: Jan-Sep and Nov-Dec, ", year, sep="")
    }
    symbol_data <- getDataForYear(symbol, year, oct_flag, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    ggplot(data = log_returns, aes(log_returns)) + 
      geom_histogram(binwidth = 1E-2, fill = "#428bca", color = "white") +
      labs(x = "Log Returns", y = "Frequency", title = paste(symbol, "Log Returns", date_range, sep=" "))
  }
  
  # Make general histogram for a symbol 
  make_hist <- function(symbol, year) {
    symbol_data <- require_symbol(symbol, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    ggplot(data = log_returns, aes(log_returns)) + 
      geom_histogram(binwidth = 1E-2, fill = "#428bca", color = "white") + 
      labs(x = "Log Returns", y = "Frequency", title = paste(symbol, " Log Returns - Year: ", year, sep=""))
  }
  
  #make qqplot
  make_qqplot <- function(symbol, year) {
    symbol_data <- require_symbol(symbol, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    qqnorm(log_returns, col="#428bca")
  }
  
  #make histogram
  output$plot_aapl <- renderPlot({ make_hist("AAPL", 2015) })
  output$plot_msft <- renderPlot({ make_hist("MSFT", 2015) })
  
  #make qqplot
  output$qqplot_aapl <- renderPlot({ make_qqplot("AAPL", 2015) })
  output$qqplot_msft <- renderPlot({ make_qqplot("MSFT", 2015) })
  
  #calculate C.I. for mean returns
  get_ci <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env) 
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    sample_size = length(log_returns)
    log_returns_avg = mean(log_returns)
    log_returns_sd = sd(log_returns)
    #error <- qnorm((1-(input$cilevel/100))/2)*log_returns_sd/sqrt(sample_size)
    #need to be approximated as t-distribution since population variance is unknown
    error <- qt((1-(input$cilevel/100))/2, sample_size-1)*log_returns_sd/sqrt(sample_size)
    ci <- log_returns_avg + c(-error, error)
    return(ci)
  }
  #make ci
  output$ci_aapl <- renderPrint({get_ci("AAPL")})
  output$ci_msft <- renderPrint({get_ci("MSFT")})
  
  #calculate C.I. for variances
  get_ci_var <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env) 
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    sample_size = length(log_returns)
    log_returns_avg = mean(log_returns)
    log_returns_sd = sd(log_returns)
    
    left_ci <- ((sample_size-1)*(log_returns_sd^2))/qchisq((1-(input$cilevel/100)/2), sample_size-1)
    right_ci <- ((sample_size-1)*(log_returns_sd^2))/qchisq(((input$cilevel/100)/2), sample_size-1)
    ci <- c(left_ci, right_ci)
    return(ci)
  }
  
  # make ci for variances
  output$ci_var_aapl <- renderPrint({get_ci_var("AAPL")})
  output$ci_var_msft <- renderPrint({get_ci_var("MSFT")})
  
  # test the equality of the two population means with unknow population variances
  symbol_env1 <- new.env()
  symbol_env2 <- new.env()
  test_pop_means <- function(symbol1, symbol2) {
    symbol1_data <- require_symbol(symbol1, symbol_env1)
    symbol2_data <- require_symbol(symbol2, symbol_env2)
    
    log_returns1 = diff(log(symbol1_data[,4]))
    log_returns1 = log_returns1[!is.na(log_returns1)]
    
    log_returns2 = diff(log(symbol2_data[,4]))
    log_returns2 = log_returns2[!is.na(log_returns2)]
    
    t.test(scale(log_returns1), scale(log_returns1))
  }
  
  #output essential t-test results
  output$aapl_msft_means <- renderTable({
    mod = test_pop_means("AAPL", "MSFT")
    tab = matrix(c(mod$parameter,mod$statistic,mod$p.value),nrow=1)
    colnames(tab) = c("Degrees of Freedom","Test-Statistic","P-Value")
    rownames(tab) = "Values"
    tab
  })
  
  output$october_aapl_return <- renderPlot({ make_hist_oct("AAPL", 2015, TRUE) })
  output$october_msft_return <- renderPlot({ make_hist_oct("MSFT", 2015, TRUE) })
  
  output$year_but_october_aapl_return <- renderPlot({ make_hist_oct("AAPL", 2015, FALSE) })
  output$year_but_october_msft_return <- renderPlot({ make_hist_oct("MSFT", 2015, FALSE) })
  
  #regression of a single stock against time
  reg_onestock <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env) 
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    time = seq(1, length(log_returns), 1)
    lm = lm(formula = log_returns ~ time)
  }
  
  #one stock data with least-squares line
  reg_onestock_data_plot <- function(symbol) {
    plot(reg_onestock(symbol)$model[1])
  }
  
  output$reg_onestock_data_plot_aapl <- renderPlot({reg_onestock_data_plot("AAPL")})
  output$reg_onestock_data_plot_msft <- renderPlot({reg_onestock_data_plot("MSFT")})
  
  #one stock residuals plot with least-squares line
  reg_onestock_resid_plot <- function(symbol) {
    residuals = resid(reg_onestock(symbol))
    plot(reg_onestock(symbol)$model$time, residuals, ylab="Residuals", xlab="time", main="Residuals Plot")
    abline(reg_onestock(symbol))
  }
  
  output$reg_onestock_resid_plot_aapl <- renderPlot({reg_onestock_resid_plot("AAPL")})
  output$reg_onestock_resid_plot_msft <- renderPlot({reg_onestock_resid_plot("MSFT")})

  
  #one stock regression intercept and slope estimates
  reg_onestock_coeffs <- function(symbol) {
    summary(reg_onestock(symbol))$coefficients
  }
  #show the output
  output$reg_onestock_coeffs_aapl <- renderDataTable({
    coeffs = reg_onestock_coeffs("AAPL")
  })
  output$reg_onestock_coeffs_msft <- renderDataTable({
    coeffs = reg_onestock_coeffs("MSFT")
  })
  
  
  #FUNCTION regression of a two stocks against each other
  symbol_env1 <- new.env()
  symbol_env2 <- new.env()
  reg_twostocks <- function(symbol1, symbol2) {
    symbol1_data <- require_symbol(symbol1, symbol_env1)
    symbol2_data <- require_symbol(symbol2, symbol_env2)
    
    log_returns1 = diff(log(symbol1_data[,4]))
    log_returns1 = log_returns1[!is.na(log_returns1)]
    
    log_returns2 = diff(log(symbol2_data[,4]))
    log_returns2 = log_returns2[!is.na(log_returns2)]
    
    lm = lm(formula = log_returns1 ~ log_returns2)
  }
  
  #FUNCTION two stocks data with least-squares line
  reg_twostocks_data_plot <- function(symbol1, symbol2) {
    plot.zoo(reg_twostocks(symbol1, symbol2)$model[2], reg_twostocks(symbol1, symbol2)$model[1], main = "Two Stocks Regression", xlab = "Stock 2", ylab = "Stock 1")
    abline(reg_twostocks(symbol1, symbol2))
  }
  #OUTPUT two stocks data with least-squares line
  output$reg_twostocks_data_plot_aapl_msft <- renderPlot({reg_twostocks_data_plot("AAPL", "MSFT")})
  
  #FUNCTION two stocks regression intercept and slope estimates
  reg_twostocks_coeffs <- function(symbol1, symbol2) {
    summary(reg_twostocks(symbol1, symbol2))$coefficients
  }
  #OUTPUT two stocks regression intercept and slope estimates
  output$reg_twostocks_coeffs_aapl_msft <- renderDataTable({
    coeffs = reg_twostocks_coeffs("AAPL", "MSFT")
  })
  
  #FUNCTION two stocks residuals plot
  #one stock residuals plot with least-squares line
  reg_twostocks_resid_plot <- function(symbol1, symbol2) {
    residuals = resid(reg_twostocks(symbol1, symbol2))
    plot(residuals, ylab="Residuals", xlab="Time", main="Residuals Plot")
    abline(reg_twostocks(symbol1, symbol2))
  }
  #OUTPUT two stocks residules plot
  output$reg_twostocks_resid_plot_aapl_msft <- renderPlot({reg_twostocks_resid_plot("AAPL", "MSFT")})
})



