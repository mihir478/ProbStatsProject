# Reduce unnecessary warnings from scaling an xts data structure
options("getSymbols.warning4.0"=FALSE)

# Makes data request to fetch stock data for a symbol over a year
makeRequestForYear <- function(symbol, election_flag) {
  data <- NULL
  if(election_flag) {
    from_date <- as.Date("07/19/2015", format="%m/%d/%Y")
    data <- getSymbols(symbol, auto.assign = FALSE, from = from_date)
  } else {
    from_date <- as.Date("01/01/2016", format="%m/%d/%Y")
    to_date <- as.Date("07/18/2016", format="%m/%d/%Y")
    data <- getSymbols(symbol, auto.assign = FALSE, from = from_date, to = to_date)
  }
  data
}

# Caches data to avoid expensive data requests
getDataForYear <- function(symbol, election_flag, envir = parent.frame()) {
  data_key <- NULL
  if(election_flag) {
    data_key <- paste(symbol, "Before", sep="")
    } else {
    data_key <- paste(symbol, "After", sep="")
  }
  if (is.null(envir[[data_key]])) {
    envir[[data_key]] <- makeRequestForYear(symbol, election_flag)
  }
  else {
    envir[[data_key]] <- getDataForYear(symbol, election_flag)
  }
  envir[[data_key]]
}

# Makes data requests and caches responses for a symbol over a year
require_symbol <- function(symbol, election_flag, envir = parent.frame()) {
  from_date <- as.Date("01/01/2015", format="%m/%d/%Y")
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol, auto.assign = FALSE, from = from_date)
  }
  envir[[symbol]]
}

shinyServer(function(input, output) {
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Make a histogram for a symbol to investigate october effect
  make_hist_elec <- function(symbol, election_flag) {
    date_range <- NULL
    if(election_flag) {
      date_range <- "- Date Range: July 19, 2016 - Today"
    } else {
      date_range <- "- Date Range: Jan 1, 2016 - July 18, 2016"
    }
    symbol_data <- getDataForYear(symbol, election_flag, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    ggplot(data = log_returns, aes(log_returns)) + 
      geom_histogram(binwidth = 1E-2, fill = "#428bca", color = "white") +
      labs(x = "Log Returns", y = "Frequency", title = paste(symbol, "Log Returns", date_range, sep=" "))
  }
  
  # Make Q-Q plot
  make_qqplot <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    qqnorm(log_returns, col="#428bca")
  }
  
  output$qqplot_aapl <- renderPlot({ make_qqplot("AAPL") })
  output$qqplot_xom <- renderPlot({ make_qqplot("XOM") })
  
  #calculate C.I. for mean returns
  get_ci <- function(symbol, election_flag) {
    symbol_data <- require_symbol(symbol, election_flag, symbol_env) 
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    sample_size = length(log_returns)
    log_returns_avg = mean(log_returns)
    log_returns_sd = sd(log_returns)
    # need to be approximated as t-distribution since population variance is unknown
    error <- qt((1-(input$cilevel/100))/2, sample_size-1)*log_returns_sd/sqrt(sample_size)
    ci <- log_returns_avg + c(-abs(error), abs(error))
    return(ci)
  }
  output$ci_aapl_post_election_news <- renderPrint({get_ci("AAPL", TRUE)})
  output$ci_xom_post_election_news <- renderPrint({get_ci("XOM", TRUE)})
  output$ci_aapl_pre_election_news <- renderPrint({get_ci("AAPL", FALSE)})
  output$ci_xom_pre_election_news <- renderPrint({get_ci("XOM", FALSE)})
  
  # Calculate C.I. for variances
  get_ci_var <- function(symbol, election_flag) {
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
  
  # Make C.I. interval for variances
  output$ci_var_aapl_post_election_news <- renderPrint({get_ci_var("AAPL", TRUE)})
  output$ci_var_xom_post_election_news <- renderPrint({get_ci_var("XOM", TRUE)})
  output$ci_var_aapl_pre_election_news <- renderPrint({get_ci_var("AAPL", FALSE)})
  output$ci_var_xompre_election_news <- renderPrint({get_ci_var("XOM", FALSE)})
  
  # Test the equality of the two population means with unknow population variances
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
  
  # Output essential t-test results
  output$aapl_xom_means <- renderTable({
    mod = test_pop_means("AAPL", "XOM")
    tab = matrix(c(mod$parameter,mod$statistic,mod$p.value),nrow=1)
    colnames(tab) = c("Degrees of Freedom","Test-Statistic","P-Value")
    rownames(tab) = "Values"
    tab
  })
  
  output$post_election_news_aapl_return <- renderPlot({ make_hist_elec("AAPL", TRUE) })
  output$post_election_news_xom_return <- renderPlot({ make_hist_elec("XOM", TRUE) })
  
  output$pre_election_news_aapl_return <- renderPlot({ make_hist_elec("AAPL", FALSE) })
  output$pre_election_news_xom_return <- renderPlot({ make_hist_elec("XOM", FALSE) })
  
  # Regression of a single stock against time
  reg_onestock <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env) 
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    time = seq(1, length(log_returns), 1)
    lm = lm(formula = log_returns ~ time)
  }
  
  # One stock data with least-squares line
  reg_onestock_data_plot <- function(symbol) {
    plot(reg_onestock(symbol)$model[1], ylab=symbol, xlab="time", main="One Stock Regression")
  }
  
  output$reg_onestock_data_plot_aapl <- renderPlot({reg_onestock_data_plot("AAPL")})
  output$reg_onestock_data_plot_xom <- renderPlot({reg_onestock_data_plot("XOM")})
  
  # One stock residuals plot with least-squares line
  reg_onestock_resid_plot <- function(symbol) {
    residuals = resid(reg_onestock(symbol))
    plot(reg_onestock(symbol)$model$time, residuals, ylab="Residuals", xlab="time", main="Residuals Plot")
    abline(reg_onestock(symbol))
  }
  
  output$reg_onestock_resid_plot_aapl <- renderPlot({reg_onestock_resid_plot("AAPL")})
  output$reg_onestock_resid_plot_xom <- renderPlot({reg_onestock_resid_plot("XOM")})
  
  # One stock regression intercept and slope estimates
  reg_onestock_coeffs <- function(symbol) {
    summary(reg_onestock(symbol))$coefficients
  }
  output$reg_onestock_coeffs_aapl <- renderDataTable({
    coeffs = reg_onestock_coeffs("AAPL")
  }, options=list(paging = FALSE, searching = FALSE))
  output$reg_onestock_coeffs_xom <- renderDataTable({
    coeffs = reg_onestock_coeffs("XOM")
  }, options=list(paging = FALSE, searching = FALSE))
  
  # Regression of a two stocks against each other
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
  
  # Two stocks data with least-squares line
  reg_twostocks_data_plot <- function(symbol1, symbol2) {
    plot.zoo(reg_twostocks(symbol1, symbol2)$model[2], reg_twostocks(symbol1, symbol2)$model[1], main = "Two Stocks Regression", xlab = symbol2, ylab = symbol1)
    abline(reg_twostocks(symbol1, symbol2))
  }
  # Two stocks data with least-squares line
  output$reg_twostocks_data_plot_aapl_xom <- renderPlot({reg_twostocks_data_plot("AAPL", "XOM")})
  
  # Two stocks regression intercept and slope estimates
  reg_twostocks_coeffs <- function(symbol1, symbol2) {
    summary(reg_twostocks(symbol1, symbol2))$coefficients
  }
  # Two stocks regression intercept and slope estimates
  output$reg_twostocks_coeffs_aapl_xom <- renderDataTable({
    coeffs = reg_twostocks_coeffs("AAPL", "XOM")
  }, options = list(searching = FALSE, paging = FALSE))
  
  # Two stocks residuals plot
  # One stock residuals plot with least-squares line
  reg_twostocks_resid_plot <- function(symbol1, symbol2) {
    residuals = resid(reg_twostocks(symbol1, symbol2))
    plot(residuals, ylab="Residuals", xlab="Time", main="Residuals Plot")
    abline(reg_twostocks(symbol1, symbol2))
  }
  # Two stocks residuals plot
  output$reg_twostocks_resid_plot_aapl_xom <- renderPlot({reg_twostocks_resid_plot("AAPL", "XOM")})
})



