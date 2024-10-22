###########################################################################
#'*TEST SET* 


calc_metrics <- function(df, n_geb, prognose, title = ""){
  df_tmp <- df
  n_geb = ensym(n_geb)
  prognose = ensym(prognose)
  df_tmp <- df_tmp %>% mutate(Residuals = !!n_geb - !!prognose,
                              isojaar = lubridate::isoyear(jaarweek))

  print(df_tmp %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                  mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                  mape = Metrics::mape(!!n_geb, !!prognose),
                  smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())
  
  print(df_tmp %>% group_by(isojaar) %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                                               mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                                               mape = Metrics::mape(!!n_geb, !!prognose),
                                               smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())
  
  p_res_time_ <-ggplot(df_tmp, aes(x = jaarweek, y = Residuals)) +
    geom_line() + xlab("Time (ISO weeks)")
  
  p_res_act_ <- ggplot(df_tmp, aes(x = {{n_geb}}, y = Residuals)) +
    geom_point() + xlab("Nr. of realized births")
  
  p_hist <- ggplot(df_tmp, aes(x = Residuals)) + geom_histogram() + ylab("Count")
  
  print(p_res_time_)
  print(p_res_act_)
  print(p_hist)
  
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_res_time_", deparse(substitute(df)), ".png"),p_res_time_, width = 4.5, height = 2.5)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_act_time_", deparse(substitute(df)), ".png"),p_res_act_, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_hist", deparse(substitute(df)), ".png"),p_hist, width = 3, height = 3)
  }

print("function calc_metrics loaded")



###########################################################################
#'*TRAIN SET* 

calc_metrics_train <- function(df, n_geb, prognose, sarima = FALSE){
  df_tmp <- df 
  n_geb = ensym(n_geb)
  prognose = ensym(prognose)
  
  if(sarima){df_tmp <- df_tmp[-c(1:52),]}
  
  df_tmp <- df_tmp %>% mutate(Residuals = !!n_geb - !!prognose,
                              isojaar = lubridate::isoyear(jaarweek))
  
  ts_res <- ts(df_tmp$Residuals, frequency = 52)
  
  df_dateseq <- seq(max(df_tmp$jaarweek) - length(ts_res), max(df_tmp$jaarweek), by = 1)
  
  print(df_tmp %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                             mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                             mape = Metrics::mape(!!n_geb, !!prognose),
                             smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())
  
  print(df_tmp %>% group_by(isojaar) %>% summarize(rmse = round(rmse(!!n_geb, !!prognose),2),
                                                   mae  = round(Metrics::mae(!!n_geb, !!prognose),2),
                                                   mape = Metrics::mape(!!n_geb, !!prognose),
                                                   smape = Metrics::smape(!!n_geb, !!prognose)) %>% as.data.frame())
  
  test_ljung_box(df_tmp$Residuals, max_lag = 60)
  test_arch(df_tmp$Residuals, max_lag = 60)
  print(shapiro.test(df_tmp$Residuals))
  
  p_res_time_ <-ggplot(df_tmp, aes(x = jaarweek, y = Residuals)) +
    geom_line() + xlab("Time (ISO weeks)")
  
  p_res_act_ <- ggplot(df_tmp, aes(x = {{n_geb}}, y = Residuals)) +
    geom_point() + xlab("Nr. of realized births")
  
  p_hist <- ggplot(df_tmp, aes(x = Residuals)) + geom_histogram() + ylab("Count")

  p_res_fit_ <- ggplot(df_tmp, aes(x = {{prognose}}, y = Residuals)) +
    geom_point() + xlab("Prognosis (nr. of births)")
  
  p_acf <- ts_res %>% acf(lag.max = 60, plot = TRUE)
  p_pacf <- ts_res %>% pacf(lag.max = 60, plot = TRUE)
  
  #
  p_qq <- df_tmp %>% ggplot(aes(sample = Residuals)) + stat_qq() + stat_qq_line() + xlab("Theoretical Quantiles") + ylab("Sample Quantiles")
  


  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_res_time_tr_", deparse(substitute(df)), ".png"),p_res_time_, width = 4.5, height = 2.5)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_act_time_tr_", deparse(substitute(df)), ".png"),p_res_act_, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_hist__tr", deparse(substitute(df)), ".png"),p_hist, width = 3, height = 3)
  # ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_acf_", deparse(substitute(df)), ".png"),p_acf, width = 3, height = 3)
  # ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_pacf_", deparse(substitute(df)), ".png"),p_pacf, width = 3, height = 3)
  ggsave(paste0("C:/Users/Merith/OneDrive - Furore Informatica B.V/Documents/RStudio/thesis/Plaatjes/p_qq_tr_", deparse(substitute(df)), ".png"),p_qq, width = 3, height = 3)
  }

# calc_metrics(df = fc_33_data, n_geb = n_geb, prognose = mean)


# R squared
rsq <- function(x, y) summary(lm(y~x))$r.squared
