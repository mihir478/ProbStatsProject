# Reduce unnecessary warnings from scaling an xts data structure
options("getSymbols.warning4.0"=FALSE)

# Makes data request to fetch stock data for a symbol over a year
makeRequestForYear <- function(symbol, election_flag) {
  data <- NULL
  if(election_flag == 1) {
    from_date <- as.Date("07/19/2016", format="%m/%d/%Y")
    data <- getSymbols(symbol, auto.assign = FALSE, from = from_date)
  } else if(election_flag == -1) {
    from_date <- as.Date("01/01/2016", format="%m/%d/%Y")
    to_date <- as.Date("07/18/2016", format="%m/%d/%Y")
    data <- getSymbols(symbol, auto.assign = FALSE, from = from_date, to = to_date)
  }
  data
}

# Caches data to avoid expensive data requests
getDataForYear <- function(symbol, election_flag, envir = parent.frame()) {
  data_key <- NULL
  if(election_flag == -1) {
    data_key <- paste(symbol, "Before", sep="")
  } else if(election_flag == 1) {
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
  data_key <- NULL
  if(election_flag == 0) {
    data_key <- paste(symbol, "All", sep="")
    from_date <- as.Date("01/01/2015", format="%m/%d/%Y")
    if (is.null(envir[[data_key]])) {
      envir[[data_key]] <- getSymbols(symbol, auto.assign = FALSE, from = from_date)
    } else {
      envir[[data_key]]
    }
  } else if(election_flag == -1) {
    data_key <- paste(symbol, "Before", sep="")
    from_date <- as.Date("07/19/2016", format="%m/%d/%Y")
    if (is.null(envir[[data_key]])) {
      envir[[data_key]] <- getSymbols(symbol, auto.assign = FALSE, from = from_date)
    } else {
      envir[[data_key]]
    }
  } else if(election_flag == 1) {
    data_key <- paste(symbol, "After", sep="")
    from_date <- as.Date("01/01/2016", format="%m/%d/%Y")
    to_date <- as.Date("07/18/2016", format="%m/%d/%Y")
    if (is.null(envir[[data_key]])) {
      envir[[data_key]] <- getSymbols(symbol, auto.assign = FALSE, from = from_date)
    } else {
      envir[[data_key]]
    }
  }
  envir[[data_key]]
}

shinyServer(function(input, output) {
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Make a histogram for a symbol to investigate october effect
  make_hist_elec <- function(symbol, election_flag) {
    date_range <- NULL
    if(election_flag == 1) {
      date_range <- "- Date Range: July 19, 2016 - Today"
    } else if (election_flag == -1) {
      date_range <- "- Date Range: Jan 1, 2016 - July 18, 2016"
    }
    symbol_data <- getDataForYear(symbol, election_flag, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    ggplot(data = log_returns, aes(log_returns)) + 
      geom_histogram(binwidth = 1E-2, fill = "#428bca", color = "white") +
      labs(x = "Log Returns", y = "Frequency", title = paste(symbol, "Log Returns", date_range, sep=" "))
  }
  
  output$post_election_news_aapl_return <- renderPlot({ make_hist_elec("AAPL", 1) })
  output$post_election_news_xom_return <- renderPlot({ make_hist_elec("XOM", 1) })
  
  output$pre_election_news_aapl_return <- renderPlot({ make_hist_elec("AAPL", -1) })
  output$pre_election_news_xom_return <- renderPlot({ make_hist_elec("XOM", -1) })
  
  output$post_election_news_jpm_return <- renderPlot({ make_hist_elec("JPM", 1) })
  output$post_election_news_ba_return <- renderPlot({ make_hist_elec("BA", 1) })
  
  output$pre_election_news_jpm_return <- renderPlot({ make_hist_elec("JPM", -1) })
  output$pre_election_news_ba_return <- renderPlot({ make_hist_elec("BA", -1) })
  
  
  # Make Q-Q plot
  make_qqplot <- function(symbol) {
    symbol_data <- require_symbol(symbol, 0, symbol_env)
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    qqnorm(log_returns, col="#428bca")
  }
  
  output$qqplot_aapl <- renderPlot({ make_qqplot("AAPL") })
  output$qqplot_xom <- renderPlot({ make_qqplot("XOM") })
  output$qqplot_jpm <- renderPlot({ make_qqplot("JPM") })
  output$qqplot_ba <- renderPlot({ make_qqplot("BA") })
  
  # Calculate C.I. for mean returns
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
  output$ci_aapl_post_election_news <- renderText({get_ci("AAPL", 1)})
  output$ci_xom_post_election_news <- renderText({get_ci("XOM", 1)})
  output$ci_aapl_pre_election_news <- renderText({get_ci("AAPL", -1)})
  output$ci_xom_pre_election_news <- renderText({get_ci("XOM", -1)})
  
  output$ci_jpm_post_election_news <- renderText({get_ci("JPM", 1)})
  output$ci_ba_post_election_news <- renderText({get_ci("BA", 1)})
  output$ci_jpm_pre_election_news <- renderText({get_ci("JPM", -1)})
  output$ci_ba_pre_election_news <- renderText({get_ci("BA", -1)})
  
  # Calculate C.I. for variances
  get_ci_var <- function(symbol, election_flag) {
    symbol_data <- require_symbol(symbol, election_flag, symbol_env) 
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
  output$ci_var_aapl_post_election_news <- renderText({get_ci_var("AAPL", 1)})
  output$ci_var_xom_post_election_news <- renderText({get_ci_var("XOM", 1)})
  output$ci_var_aapl_pre_election_news <- renderText({get_ci_var("AAPL", -1)})
  output$ci_var_xom_pre_election_news <- renderText({get_ci_var("XOM", -1)})
  
  output$ci_var_jpm_post_election_news <- renderText({get_ci_var("JPM", 1)})
  output$ci_var_ba_post_election_news <- renderText({get_ci_var("BA", 1)})
  output$ci_var_jpm_pre_election_news <- renderText({get_ci_var("JPM", -1)})
  output$ci_var_ba_pre_election_news <- renderText({get_ci_var("BA", -1)})
  
  # Test the equality of the two population means with unknow population variances
  # use stock returns data
  symbol_env1 <- new.env()
  symbol_env2 <- new.env()
  test_pop_means <- function(symbol1, symbol2, same_time_period) {
    symbol1_data <- require_symbol(symbol1, if(same_time_period) 0 else -1, symbol_env1)
    symbol2_data <- require_symbol(symbol2, if(same_time_period) 0 else 1, symbol_env2)
    
    symbol1_data <- symbol1_data[,4]
    symbol2_data <- symbol2_data[,4]
    
    t.test(symbol1_data, symbol2_data)
  }
  
  # Output essential t-test results
  output$aapl_aapl_means <- renderTable({
    mod = test_pop_means("AAPL", "AAPL", FALSE)
    tab = matrix(c(mod$parameter,mod$statistic,mod$p.value),nrow=1)
    colnames(tab) = c("Degrees of Freedom","Test-Statistic","P-Value")
    rownames(tab) = "Values"
    tab
  })
  output$xom_xom_means <- renderTable({
    mod = test_pop_means("XOM", "XOM", FALSE)
    tab = matrix(c(mod$parameter,mod$statistic,mod$p.value),nrow=1)
    colnames(tab) = c("Degrees of Freedom","Test-Statistic","P-Value")
    rownames(tab) = "Values"
    tab
  })
  output$jpm_jpm_means <- renderTable({
    mod = test_pop_means("JPM", "JPM", FALSE)
    tab = matrix(c(mod$parameter,mod$statistic,mod$p.value),nrow=1)
    colnames(tab) = c("Degrees of Freedom","Test-Statistic","P-Value")
    rownames(tab) = "Values"
    tab
  })
  output$ba_ba_means <- renderTable({
    mod = test_pop_means("BA", "BA", FALSE)
    tab = matrix(c(mod$parameter,mod$statistic,mod$p.value),nrow=1)
    colnames(tab) = c("Degrees of Freedom","Test-Statistic","P-Value")
    rownames(tab) = "Values"
    tab
  })
  
  # Regression of a single stock against time
  reg_onestock <- function(symbol) {
    symbol_data <- require_symbol(symbol, 0, symbol_env) 
    log_returns = diff(log(symbol_data[,4]))
    log_returns = log_returns[!is.na(log_returns)]
    time = seq(1, length(log_returns), 1)
    lm = lm(formula = log_returns ~ time)
  }
  
  # One stock data with least-squares line
  reg_onestock_data_plot <- function(symbol) {
    plot(reg_onestock(symbol)$model[1], ylab=symbol, xlab="time", main = paste(symbol, " Regression"))
  }
  
  output$reg_onestock_data_plot_aapl <- renderPlot({reg_onestock_data_plot("AAPL")})
  output$reg_onestock_data_plot_xom <- renderPlot({reg_onestock_data_plot("XOM")})
  
  output$reg_onestock_data_plot_jpm <- renderPlot({reg_onestock_data_plot("JPM")})
  output$reg_onestock_data_plot_ba <- renderPlot({reg_onestock_data_plot("BA")})
  
  # One stock residuals plot with least-squares line
  reg_onestock_resid_plot <- function(symbol) {
    residuals = resid(reg_onestock(symbol))
    plot(reg_onestock(symbol)$model$time, residuals, ylab="Residuals", xlab="time", main="Residuals Plot")
    abline(reg_onestock(symbol))
  }
  
  output$reg_onestock_resid_plot_aapl <- renderPlot({reg_onestock_resid_plot("AAPL")})
  output$reg_onestock_resid_plot_xom <- renderPlot({reg_onestock_resid_plot("XOM")})
  
  output$reg_onestock_resid_plot_jpm <- renderPlot({reg_onestock_resid_plot("JPM")})
  output$reg_onestock_resid_plot_ba <- renderPlot({reg_onestock_resid_plot("BA")})
  
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
  
  output$reg_onestock_coeffs_jpm <- renderDataTable({
    coeffs = reg_onestock_coeffs("JPM")
  }, options=list(paging = FALSE, searching = FALSE))
  output$reg_onestock_coeffs_ba <- renderDataTable({
    coeffs = reg_onestock_coeffs("BA")
  }, options=list(paging = FALSE, searching = FALSE))
  
  #add R-squared
  reg_onestock_r2 <- function(symbol) {
    summary(reg_onestock(symbol))$r.squared
  }
  
  output$reg_onestock_r2_aapl <- renderText({reg_onestock_r2('AAPL')})
  output$reg_onestock_r2_xom <- renderText({reg_onestock_r2('XOM')})
  
  output$reg_onestock_r2_jpm <- renderText({reg_onestock_r2('JPM')})
  output$reg_onestock_r2_ba <- renderText({reg_onestock_r2('BA')})
  
  # Regression of a two stocks against each other
  symbol_env1 <- new.env()
  symbol_env2 <- new.env()
  reg_twostocks <- function(symbol1, symbol2) {
    symbol1_data <- require_symbol(symbol1, 0, symbol_env1)
    symbol2_data <- require_symbol(symbol2, 0, symbol_env2)
    
    log_returns1 = diff(log(symbol1_data[,4]))
    log_returns1 = log_returns1[!is.na(log_returns1)]
    
    log_returns2 = diff(log(symbol2_data[,4]))
    log_returns2 = log_returns2[!is.na(log_returns2)]
    
    lm = lm(formula = log_returns1 ~ log_returns2)
  }
  
  # Two stocks data with least-squares line
  reg_twostocks_data_plot <- function(symbol1, symbol2) {
    plot.zoo(reg_twostocks(symbol1, symbol2)$model[2], reg_twostocks(symbol1, symbol2)$model[1], main = paste(symbol1, " Regression against S&P 500 (SPY)"), xlab = symbol2, ylab = symbol1)
    abline(reg_twostocks(symbol1, symbol2))
  }
  # Two stocks data with least-squares line
  output$reg_twostocks_data_plot_aapl_spy <- renderPlot({reg_twostocks_data_plot("AAPL", "SPY")})
  output$reg_twostocks_data_plot_xom_spy <- renderPlot({reg_twostocks_data_plot("XOM", "SPY")})
  output$reg_twostocks_data_plot_jpm_spy <- renderPlot({reg_twostocks_data_plot("JPM", "SPY")})
  output$reg_twostocks_data_plot_ba_spy <- renderPlot({reg_twostocks_data_plot("BA", "SPY")})
  
  # Two stocks data R2
  reg_twostocks_r2 <- function(symbol1, symbol2) {
    summary(reg_twostocks(symbol1, symbol2))$r.squared
  }
  # Two stocks data R2 output
  output$reg_twostocks_r2_aapl_spy <- renderText({reg_twostocks_r2('AAPL', 'SPY')})
  output$reg_twostocks_r2_xom_spy <- renderText({reg_twostocks_r2('XOM', 'SPY')})
  output$reg_twostocks_r2_jpm_spy <- renderText({reg_twostocks_r2('JPM', 'SPY')})
  output$reg_twostocks_r2_ba_spy <- renderText({reg_twostocks_r2('BA', 'SPY')})
  
  # Two stocks regression intercept and slope estimates
  reg_twostocks_coeffs <- function(symbol1, symbol2) {
    summary(reg_twostocks(symbol1, symbol2))$coefficients
  }
  # Two stocks regression intercept and slope estimates
  output$reg_twostocks_coeffs_aapl_spy <- renderDataTable({
    coeffs = reg_twostocks_coeffs("AAPL", "SPY")
  }, options = list(searching = FALSE, paging = FALSE))
  output$reg_twostocks_coeffs_xom_spy <- renderDataTable({
    coeffs = reg_twostocks_coeffs("XOM", "SPY")
  }, options = list(searching = FALSE, paging = FALSE))
  output$reg_twostocks_coeffs_jpm_spy <- renderDataTable({
    coeffs = reg_twostocks_coeffs("JPM", "SPY")
  }, options = list(searching = FALSE, paging = FALSE))
  output$reg_twostocks_coeffs_ba_spy <- renderDataTable({
    coeffs = reg_twostocks_coeffs("BA", "SPY")
  }, options = list(searching = FALSE, paging = FALSE))
  
  # Two stocks residuals plot
  # One stock residuals plot with least-squares line
  reg_twostocks_resid_plot <- function(symbol1, symbol2) {
    residuals = resid(reg_twostocks(symbol1, symbol2))
    plot(residuals, ylab="Residuals", xlab="Time", main="Residuals Plot")
    abline(reg_twostocks(symbol1, symbol2))
  }
  # Two stocks residuals plot
  output$reg_twostocks_resid_plot_aapl_spy <- renderPlot({reg_twostocks_resid_plot("AAPL", "SPY")})
  output$reg_twostocks_resid_plot_xom_spy <- renderPlot({reg_twostocks_resid_plot("XOM", "SPY")})
  output$reg_twostocks_resid_plot_jpm_spy <- renderPlot({reg_twostocks_resid_plot("JPM", "SPY")})
  output$reg_twostocks_resid_plot_ba_spy <- renderPlot({reg_twostocks_resid_plot("BA", "SPY")})
})
