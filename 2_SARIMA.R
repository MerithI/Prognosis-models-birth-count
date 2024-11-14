
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * 2. SARIMA. 2a&b *
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' Rolling forecast: multistep forecast with re-estimation, see: 
#' https://robjhyndman.com/hyndsight/rolling-forecasts/
#' Uses all previous weekly realized births to re-estimate the parameter coefficients
#' Order was determined to be SARIMA(2,0,0)(1,1,1)[52] on the test data.
#' For model 2a: step_size = 4 weeks, forecast_window = c(17,20)
#' For model 2b: step_size = 52 weeks, forecast_window = c(1, 51)

# Data from 2012 W5 through 2021 W49

fc_sarima_tavd <- function(data_week, step_size, forecast_window, forecast_start) {
  
  #################################################
  ######### Preparation
  
  # Column for forecasted values
  data_tmp <- data_week %>% 
    
    mutate(forecast = NA_real_,
           ci_lower_80 = NA_real_, 
           ci_upper_80 = NA_real_,
           ci_lower_95 = NA_real_, 
           ci_upper_95 = NA_real_)
  
  # Initialization
  assign_start = 0
  
  forecast_horizon <- max(forecast_window)
  
  end_train_first <- forecast_start - min(forecast_window)
  
  
  #################################################
  ######### Loop for applying rolling forecast
  
  for (end_train in seq(end_train_first, nrow(data_tmp), by = step_size)) {
    
    # Determine the range for storing the forecast values
    assign_start <- end_train + forecast_window[1]
    assign_end <- min(end_train + forecast_window[2], nrow(data_tmp))
    
    if (assign_start < assign_end) {
      
      # Define train set for the current step
      ts_train <- ts(data_tmp$n[1:end_train], frequency = 52)
      
      cat("________\nNEW LOOP \n________\n", "\ntrainset goes up to:", end_train, "")
      cat("\nForecast starts at:", end_train + 1, "\n")
      
      # Fit SARIMA model
      fit_sarima <- Arima(ts_train, order = c(2,0,0), seasonal = list(order = c(1,1,1), period = 52))
      
      # Forecast the next "h" weeks (maximum forecast_window value)
      forecast_result <- forecast::forecast(fit_sarima, h = forecast_horizon, bootstrap = TRUE)
      
      cat("Assigning forecasts from:", assign_start, "to:", assign_end, "\n")
      
      # Assign the forecast values
      data_tmp$forecast[assign_start:assign_end] <- forecast_result$mean[forecast_window[1]:forecast_window[2]]
      data_tmp$ci_lower_80[assign_start:assign_end] <- forecast_result$lower[forecast_window[1]:forecast_window[2], "80%"]
      data_tmp$ci_upper_80[assign_start:assign_end] <- forecast_result$upper[forecast_window[1]:forecast_window[2], "80%"]
      data_tmp$ci_lower_95[assign_start:assign_end] <- forecast_result$lower[forecast_window[1]:forecast_window[2], "95%"]
      data_tmp$ci_upper_95[assign_start:assign_end] <- forecast_result$upper[forecast_window[1]:forecast_window[2], "95%"]
      
    }
  }
  
  return(data_tmp)
  
}
