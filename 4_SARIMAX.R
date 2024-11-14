
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * 4. SARIMAX *
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# dataframe with weekly birth counts (n) and weekly prognosis from model 3 (n_prog)

fc_sarimax_tavd <- function(data_week, step_size, forecast_window, forecast_start) {
  
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
  
  #################################################
  ######### Loop rolling forecast
  
  for (end_train in seq(forecast_start - min(forecast_window), nrow(data_tmp), by = step_size)) {
    
    cat("\n________\nNEW LOOP \n________\n")
    
    # Determine the range for storing the forecast values
    assign_start <- end_train + forecast_window[1]
    assign_end <- min(end_train + forecast_window[2], nrow(data_tmp))
    cat("Assigning forecasts from:", assign_start, "to:", assign_end, "\n")
    
    if (assign_start <= assign_end) {
      
      # Define train and test set for the current step
      ts_train_y <- ts(data_tmp$n[1:end_train], frequency = 52)
      ts_train_x <- ts(data_tmp$n_prog[1:end_train], frequency = 52)
      ts_test_x  <- ts(data_tmp$n_prog[(end_train + 1):(end_train + forecast_window[2])], frequency = 52)
      
      cat("\ntrainset goes up to:", end_train, "")
      cat("\nForecast starts at:", end_train + 1, "\n")
      
      valid_indices <- which(!is.na(ts_test_x))
      forecast_horizon <- length(valid_indices)
      
      # Fit SARIMA model
      fit_sarimax <- Arima(ts_train_y, order = c(0,0,1), seasonal = list(order = c(0,0,1), period = 52), xreg = ts_train_x)
      
      # Forecast the next "h" weeks (maximum forecast_window value)
      forecast_result <- forecast::forecast(fit_sarimax, h = forecast_horizon, xreg = ts_test_x[valid_indices], bootstrap = TRUE)
      
      # Check if assign_start is within bounds
      print(forecast_result$mean[forecast_window[1]:forecast_horizon])
      print(forecast_result$lower[forecast_window[1]:forecast_horizon, "80%"])
      
      # Assign the forecast values
      data_tmp$forecast[assign_start:assign_end] <- forecast_result$mean[forecast_window[1]:forecast_horizon]
      data_tmp$ci_lower_80[assign_start:assign_end] <- forecast_result$lower[forecast_window[1]:forecast_horizon, "80%"]
      data_tmp$ci_upper_80[assign_start:assign_end] <- forecast_result$upper[forecast_window[1]:forecast_horizon, "80%"]
      data_tmp$ci_lower_95[assign_start:assign_end] <- forecast_result$lower[forecast_window[1]:forecast_horizon, "95%"]
      data_tmp$ci_upper_95[assign_start:assign_end] <- forecast_result$upper[forecast_window[1]:forecast_horizon, "95%"]
      
    }
  }
  
  return(data_tmp)
  
}
