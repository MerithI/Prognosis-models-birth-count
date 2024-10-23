###########################################################################
#     '*Functions for model fit and performance * 
###########################################################################
# calc_metrics: for test data
# calc_metrics_train: for train data
# rsq: R^2, for comparing performance to pilot study results

###########################################################################
#'*TEST SET* 

# Uses train data, n_geb = realized birth count, prognose = predicted birth count

calc_metrics <- function(df, n_geb, prognose, title = ""){

  # Preparation
  df_tmp <- df
  n_geb = ensym(n_geb)
  prognose = ensym(prognose)

  # Calculate residuals = forecast errors
  df_tmp <- df_tmp %>% mutate(Residuals = !!n_geb - !!prognose,
                              isojaar = lubridate::isoyear(jaarweek))

  # Calculate and print performance measures for the whole test dataset
  print(df_tmp %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                  mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                  mape = Metrics::mape(!!n_geb, !!prognose),
                  smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())
  
  # Calculate and print performance per ISO year
  print(df_tmp %>% group_by(isojaar) %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                                               mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                                               mape = Metrics::mape(!!n_geb, !!prognose),
                                               smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())
  
  # Residual analysis (residuals vs time, vs realized births, normality)
  p_res_time_ <-ggplot(df_tmp, aes(x = jaarweek, y = Residuals)) +
    geom_line() + xlab("Time (ISO weeks)")
  
  p_res_act_ <- ggplot(df_tmp, aes(x = {{n_geb}}, y = Residuals)) +
    geom_point() + xlab("Nr. of realized births")
  
  p_hist <- ggplot(df_tmp, aes(x = Residuals)) + geom_histogram() + ylab("Count")
  
  print(p_res_time_)
  print(p_res_act_)
  print(p_hist)

  # save
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_res_time_", deparse(substitute(df)), ".png"),p_res_time_, width = 4.5, height = 2.5)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_act_time_", deparse(substitute(df)), ".png"),p_res_act_, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_hist", deparse(substitute(df)), ".png"),p_hist, width = 3, height = 3)
  }

print("function calc_metrics loaded")



###########################################################################
#'*TRAIN SET* 

# Uses train data, n_geb = realized birth count, prognose = predicted birth count
# sarima = TRUE if the model is a sarima model using seasonal differencing. In that case, the first 52 values should not be used, since they are used for initialization and close to zero.
#          i.e. the differencing means that the model is fitted to the change compared to 52 weeks ago. For the first 52 weeks, there are no values of 52 weeks ago and this change cannot be calculated. 

calc_metrics_train <- function(df, n_geb, prognose, sarima = FALSE){

  # Preparation
  df_tmp <- df 
  n_geb = ensym(n_geb)
  prognose = ensym(prognose)

  # Ignore first 52 values if sarima model with seasonal differencing
  if(sarima){df_tmp <- df_tmp[-c(1:52),]}

  # Residuals
  df_tmp <- df_tmp %>% mutate(Residuals = !!n_geb - !!prognose,
                              isojaar = lubridate::isoyear(jaarweek))
  
  ts_res <- ts(df_tmp$Residuals, frequency = 52)

  # Performance/fit measures for the whole test dataset
  print(df_tmp %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                             mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                             mape = Metrics::mape(!!n_geb, !!prognose),
                             smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())

  # Performance/fit measures per ISO year
  print(df_tmp %>% group_by(isojaar) %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                                                   mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                                                   mape = Metrics::mape(!!n_geb, !!prognose),
                                                   smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())

  # Fit tests (autocorrelation, heteroskedacity, normality)
  test_ljung_box(df_tmp$Residuals, max_lag = 60)
  test_arch(df_tmp$Residuals, max_lag = 60)
  print(shapiro.test(df_tmp$Residuals))

  # Residual analysis (vs time, vs realized births, vs prognosis, and histogram & QQ-plot for normality)
  p_res_time_ <-ggplot(df_tmp, aes(x = jaarweek, y = Residuals)) +
    geom_line() + xlab("Time (ISO weeks)")
  
  p_res_act_ <- ggplot(df_tmp, aes(x = {{n_geb}}, y = Residuals)) +
    geom_point() + xlab("Nr. of realized births")
  
  p_res_fit_ <- ggplot(df_tmp, aes(x = {{prognose}}, y = Residuals)) +
    geom_point() + xlab("Prognosis (nr. of births)")
  
  p_hist <- ggplot(df_tmp, aes(x = Residuals)) + geom_histogram() + ylab("Count") 
  
  p_qq <- df_tmp %>% ggplot(aes(sample = Residuals)) + stat_qq() + stat_qq_line() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles")

  # Autocorrelation and heteroskedacity
  p_acf <- ts_res %>% acf(lag.max = 60, plot = TRUE)
  p_pacf <- ts_res %>% pacf(lag.max = 60, plot = TRUE)
  
  # Save
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_res_time_tr_", deparse(substitute(df)), ".png"),p_res_time_, width = 4.5, height = 2.5)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_act_time_tr_", deparse(substitute(df)), ".png"),p_res_act_, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_hist__tr", deparse(substitute(df)), ".png"),p_hist, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_acf_", deparse(substitute(df)), ".png"),p_acf, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_pacf_", deparse(substitute(df)), ".png"),p_pacf, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_qq_tr_", deparse(substitute(df)), ".png"),p_qq, width = 3, height = 3)
  }

# R squared
rsq <- function(x, y) summary(lm(y~x))$r.squared
