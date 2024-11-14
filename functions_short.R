#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * ljung_box*
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' Tests autocorrelation. Returns lags at which autocorrelation is found

test_ljung_box <- function(residuals, max_lag) {
  significant_lags <- data.frame(Lag = integer(), p_value = numeric())
  
  for (lag in 1:max_lag) {
    test_result <- stats::Box.test(residuals, lag = lag, type = "Ljung-Box")
    p_value <- test_result$p.value
    
    if (p_value < 0.05) {
      significant_lags <- rbind(significant_lags, data.frame(Lag = lag, p_value = p_value))
    }
  }
  
  return(significant_lags)
}



#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * arch*
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' Tests for heteroskedacity (lagrange)
#' Returns lags at which autocorrelation is found

test_arch <- function(residuals, max_lag) {
  significant_lags <- data.frame(Lag = integer(), p_value = numeric())
  
  for (lag in 1:max_lag) {
    test_result <- FinTS::ArchTest(residuals, lags = lag)
    p_value <- test_result$p.value
    
    if (p_value < 0.05) {
      significant_lags <- rbind(significant_lags, data.frame(Lag = lag, p_value = p_value))
    }
  }
  
  return(significant_lags)
}

#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * bootstrap CI baseline *
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' 
bootstrap_ci <- function(data, n_iterations = 10000, ci_small = 0.80, ci_wide = 0.95) {
  # Function to calculate bootstrapped confidence interval for the mean
  
results <- replicate(n_iterations, {
  sample_data <- sample(data, length(data), replace = TRUE)
  mean(sample_data)
})

# Compute the lower and upper bounds of the confidence interval
lower_bound_ci_small <- quantile(results, (1 - ci_small) / 2)
upper_bound_ci_small <- quantile(results, 1 - (1 - ci_small) / 2)
lower_bound_ci_wide <- quantile(results, (1 - ci_wide) / 2)
upper_bound_ci_wide <- quantile(results, 1 - (1 - ci_wide) / 2)

return(c(lower_bound_ci_small, upper_bound_ci_small, lower_bound_ci_wide, upper_bound_ci_wide))
}

#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * filt: fast filter *
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

filt <- function(data = data, f){
  data <- data
  if(f == 12){data <- data %>% filter(data$jaarweek > yearweek("2012 w04") & data$jaarweek < yearweek("2018 w01"))}
  else if (f == 18){data <- data %>% filter(data$jaarweek >= yearweek("2018 w01") & data$jaarweek < yearweek("2021 w50"))}
  return(data)
  }
