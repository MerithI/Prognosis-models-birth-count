
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * 6. Additional *
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' 
#' 

###############################################
#' * 6a Hospital births with model 3*
#' 

# Input: weekly prognosis model 3c


######################
## Preparation

# Realized hospital births
df_hospital <- df %>%
  filter(geboorteplek_naam == "Ziekenhuis (2e lijn)" |
           geboorteplek_naam == "Ziekenhuis (1e lijn)") %>%
  group_by(jaarweek) %>%
  summarize(n_hospital = n()) %>%
  ungroup()

# Create train dataset: Prognosis 3c for all births + realized hospital births
data_hospital_train <- fc_33_12 %>%
  filter(jaarweek > yearweek("2012 w04") & 
           jaarweek < yearweek("2018 w01")) %>%
  left_join(df_hospital %>% select(jaarweek, n_hospital), by = "jaarweek")

# Pearsons correlation coefficient
correlation <- cor.test(data_hospital_train$mean, data_hospital_train$n_hospital)
cor_coef <- correlation$estimate
cor_p <- correlation$p.value
ifelse(cor_p < 0.01, "< 0.01", as.character(round(cor_p), 3))

p5 <- ggplot(data = data_hospital_train, aes(x = mean, y = n_hospital)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_text(x = 3350, y = 2470, label = paste("Pearson's r = ", round(cor_coef, 2)), size = 3) +
  geom_text(x = 3350, y = 2430, label = paste("p =", cor_p), size = 3) +
  ggtitle("Correlation", subtitle = "Model 3c vs hospital births") + xlab("Prognosis M3c (weekly)") + ylab("Realized hospital births (weekly)")

p5


#######################
## Fitting model

lm_hospital_births <- lm(n_hospital ~ mean, data = data_hospital_train)

data_hospital_train <- data_hospital_train %>%
  mutate(fc_33_lm_hospital = predict(lm_hospital_births, newdata = data_hospital_train))


###############################
### Predicting for test set 2018-21

data_hospital_test <- fc_33_data %>% 
  select(jaarweek, n_geb, mean) %>% 
  mutate(isojaar = isoyear(jaarweek)) %>%
  filter(jaarweek < yearweek("2021 w50")) %>%
  left_join(df_hospital %>% select(jaarweek, n_hospital), by = "jaarweek") %>%
  select(jaarweek, isojaar, n_hospital, mean)

fc_33_lm_hospital_95 <- predict(lm_hospital_births, newdata = data_hospital_test, interval = "prediction")
fc_33_lm_hospital_80 <- predict(lm_hospital_births, newdata = data_hospital_test, interval = "prediction", level = 0.80)

data_hospital_test <- data_hospital_test %>%
  mutate(
    fc_33_lm_hospital = fc_33_lm_hospital_95[,"fit"],
    ci_lower_80 = fc_33_lm_hospital_80[,"lwr"],
    ci_upper_80 = fc_33_lm_hospital_80[,"upr"],
    ci_lower_95 = fc_33_lm_hospital_95[,"lwr"],
    ci_upper_95 = fc_33_lm_hospital_95[,"upr"], 
    residual    = n_hospital - fc_33_lm_hospital
  )



##############################################################################
#'* 6b. Daily births *
#'
#'2 parts: part 1: daily version of fc_duedate_help & fc_duedate_window
#'         part 3: linear model

###########################################################
###### daily version of model 3c

fc_duedate_help_daily <- function(
    data,                                 # data: nr of due births per date
    pdf,                                  # empirical probability distribution function of test set
    date_start = as_date("2017-12-01"),   # first possible due date with influence on prognosis
    date_end = as_date("2021-12-31"),     # last possible due date
    date_start2 = as_date("2018-01-01"),  # start of prognosis
    date_end2 = as_date("2021-12-12"),    # end of prognosis
    nRep = 10000){
  
  # Period to be forecasted
  date_seq <- seq.Date(date_start2, date_end2, by = "day")
  
  # Determine which data to use 
  test_data_dag <- data %>%
    filter(datum >= date_start &
             datum <= date_end)
  
  a_terme_datum = rep(test_data_dag$datum, test_data_dag$n_at)
  n_geboorten = length(a_terme_datum)
  
  ### SIMULATION
  plan(multisession)
  realisations <- future_replicate(nRep, {
    
    # Draw a realised dif (# days after due date) from empirical distribution with probability p
    s = sample(pdf$dif, size = n_geboorten, prob = pdf$p, replace = TRUE)
    
    # Add given due dates to get bevallingsdatum
    realisation = a_terme_datum + s                      
    
    # Count number of births per date
    realisation <- table(realisation)
    
    # add dates with 0 observations that are in date_seq
    realisation_vec <- as.numeric(realisation)
    names(realisation_vec) <- names(realisation)
    missing_dates <- setdiff(as.character(date_seq), names(realisation_vec))
    if (length(missing_dates) > 0 ) {
      realisation_vec[missing_dates] <- 0
    }
    
    realisation_vec[as.character(date_seq)]
    
  }, simplify = TRUE)
  
  # Transform to long format
  realisations <- as_tibble(t(realisations))
  
  result <- realisations %>%
    mutate(rep = 1:nrow(realisations)) %>%
    pivot_longer(cols = -rep, names_to = "datum", values_to = "n_sim") %>%
    mutate(
      datum = lubridate::ymd(datum)
    )
  
  # Summarize per day and calculate quantile prediction interval thresholds
  result_summarized <- result %>%
    group_by(datum) %>%
    summarise (
      q025 = quantile(n_sim, 0.025),
      q100 = quantile(n_sim, 0.1),
      q900 = quantile(n_sim, 0.9),
      q975 = quantile(n_sim, 0.975),
      mean = mean(n_sim)
    )
  
  return(result_summarized)
}


fc_duedate_window_daily <- function(
    data_individual,            # data, each row: 1 pregnancy with a due date & birth date
    pdf,                        # empirical distribution for national level or that region
    step_size = 4,              # for how many weeks you want to forecast
    forecast_window = c(17, 20),# Forecast window in weeks. c(a, b), where b-a == stepsize
    forecast_start,             # yearweek
    forecast_end,               # yearweek
    nr_weeks_pregnant,          # include pregnancies that are already x+ weeks at forecasting moment
    weeks_to_add,               # added nr of weeks for correction of unknown recent pregnancies
    nRep,                       # nr of simulations
    correction){                # If TRUE, apply mean imputation for recent pregnancies
  
  # Preparation
  container <- tibble(
    datum    = as.Date(character()), # Changed to store daily dates
    q025     = numeric(), 
    q100     = numeric(),
    q900     = numeric(), 
    q975     = numeric(),
    mean     = numeric()
  )
  
  forecast_start <- as_date(forecast_start)
  forecast_end   <- as_date(forecast_end)
  
  date_prognosismaking_first <- forecast_start - (7 * (min(forecast_window)) - 6)
  date_prognosismaking_last  <- as_date(forecast_end) - 14*7
  
  # Loop: every step_size
  time_start <- Sys.time()
  for (date_prognosismaking in seq(date_prognosismaking_first, date_prognosismaking_last, by = (step_size *7 ))){
    date_prognosismaking <- as_date(date_prognosismaking)
    
    # set start and end date for the to be forecasted period
    date_start2 = date_prognosismaking + 7 * (min(forecast_window)) - 6
    date_end2 =   date_prognosismaking + 7 * max(forecast_window)
    print(paste("Making prognosis from date", date_start2, "to date", date_end2))
    
    # Get group of pregnancies of x weeks or longer
    data_tmp <- data_individual %>% 
      filter(
        geboorte_datum >= date_prognosismaking &             
          date_prognosismaking - start_pregnancy > nr_weeks_pregnant*7 & 
          date_start2 - a_terme_datum <= 35 
      )
    
    # Group by due date
    data_tmp <- data_tmp %>%
      group_by(a_terme_datum) %>%
      summarize(n_at = n()) %>%
      ungroup() %>%
      dplyr::rename(datum = a_terme_datum)
    
    # If correction for unavailable pregnancies is applied
    if(correction){
      extra_dates <- tibble(
        datum = seq(max(data_tmp$datum) + 1, by = "day", length.out = weeks_to_add * 7),
        n_at = mean(data_tmp$n_at)
      )
      data_tmp <- bind_rows(data_tmp, extra_dates)
    }
    
    # Simulate births based on nr of pregnancies on each due date + empirical distribution
    result <- fc_duedate_help(data_tmp,
                              date_start = min(data_tmp$datum),
                              date_end = max(data_tmp$datum),
                              date_start2 = date_start2,
                              date_end2 = date_end2,
                              nRep = nRep,
                              pdf = pdf)
    
    container <- rbind(container, result)
  }
  
  # Time registration end loop
  print(paste("Starttime:", time_start, " Endtime:", Sys.time(), " Duration:", (Sys.time()-time_start)))
  
  return(container)
}


###########################################################
###### linear model with weekday + model 3c output


# Test & train data
data_dag_1217 <- data_dag %>% filt(f = 12)
data_dag_1821 <- data_dag %>% filt(f = 18)

# Fit linear model on training data
lm_weekday <- lm(n_geb ~ n_prognose + weekdag, data = data_dag_1217)

residuals <- residuals(lm_weekday)

# Get fitted values for test data
fitted_values <- predict(lm_weekday, newdata = data_dag_1821)

# Define the bootstrapping function for prediction intervals
boot_predict <- function(data, indices) {
  # Resample residuals
  boot_residuals <- residuals[indices]
  
  # Make sure to sample the same number of residuals as there are fitted values
  resampled_residuals <- sample(boot_residuals, length(fitted_values), replace = TRUE)
  
  # Add resampled residuals to the fitted values to simulate new outcomes
  simulated_outcome <- fitted_values + resampled_residuals
  
  return(simulated_outcome)
}

# Set number of bootstrap replications
num_bootstraps <- 10000

# Apply bootstrapping to get simulated predictions for the new dataset (data_dag_1821)
boot_results <- boot(data = data_dag_1217, statistic = boot_predict, R = num_bootstraps)

# Extract bootstrap predictions
boot_predictions <- boot_results$t  # Matrix of simulated predictions (rows = bootstrap replications, columns = observations)

# Calculate 95% and 80% prediction intervals based on percentiles
pi_lower_95 <- apply(boot_predictions, 2, quantile, probs = 0.025)  # 2.5th percentile for 95% lower prediction interval
pi_upper_95 <- apply(boot_predictions, 2, quantile, probs = 0.975)  # 97.5th percentile for 95% upper prediction interval

pi_lower_80 <- apply(boot_predictions, 2, quantile, probs = 0.10)   # 10th percentile for 80% lower prediction interval
pi_upper_80 <- apply(boot_predictions, 2, quantile, probs = 0.90)   # 90th percentile for 80% upper prediction interval

# Calculate the mean predicted value
fc_lm_weekday_mean <- apply(boot_predictions, 2, mean)

# Add the predictions and prediction intervals to the original data
data_dag_1821 <- data_dag_1821 %>%
  mutate(fc_lm_weekday = fc_lm_weekday_mean,  # Mean of bootstrap predictions
         ci_lower_80   = pi_lower_80,         # 80% prediction interval
         ci_upper_80   = pi_upper_80,
         ci_lower_95   = pi_lower_95,         # 95% prediction interval
         ci_upper_95   = pi_upper_95)



##############################################################################
#'*6c Parity as predictor (added to 3c DueDate+) *
# 
# Due to time constraints, this is performed as model 3c but ran for the 3 different groups
# Ideally fc_duedate_window would be adjusted so that it could work with the parity as variable
# It would then, within each simulation, sample the birth dates from the corresponding parity of the pregnancy 


# Emperical distribution for each parity type
df_Nullipara <- df %>% filter(pariteit_mn == "Nullipara")
edf_Nullipara <- make_edf(df_Nullipara, dif)

df_Multipara <- df %>% filter(pariteit_mn == "Multipara")
edf_Multipara <- make_edf(df_Multipara, dif)

df_parity_unknown <- df %>% filter(is.na(pariteit_mn))
edf_parity_unknown <- make_edf(df_parity_unknown, dif)

# 16 wks pregnant + correction + parity
fc_33_Nullipara3 <- fc_duedate_window(data_individual = df_Nullipara,
                                      forecast_start = yearweek("2018 w01"),             
                                      forecast_end = yearweek("2021 w49"),              
                                      nr_weeks_pregnant = 16,  
                                      nRep = 10000,
                                      weeks_to_add = 16,
                                      correction = TRUE,
                                      pdf = edf_Nullipara)

fc_33_Multipara3 <- fc_duedate_window(data_individual = df_Multipara,
                                      forecast_start = yearweek("2018 w01"),             
                                      forecast_end = yearweek("2021 w49"),              
                                      nr_weeks_pregnant = 16,  
                                      nRep = 10000,
                                      weeks_to_add = 16,
                                      correction = TRUE,
                                      pdf = edf_Multipara)

fc_33_parity_unknown3 <- fc_duedate_window(data_individual = df_parity_unknown,
                                           forecast_start = yearweek("2018 w01"),             
                                           forecast_end = yearweek("2021 w49"),              
                                           nr_weeks_pregnant = 16,  
                                           nRep = 10000,
                                           weeks_to_add = 16,
                                           correction = TRUE,
                                           pdf = edf_parity_unknown)

fc_33_parity3 <- fc_33_Nullipara3 %>%
  left_join(fc_33_Multipara3, by = "jaarweek") %>%
  left_join(fc_33_parity_unknown3, by = "jaarweek") %>%
  mutate(mean_parity = mean + mean.x + mean.y) %>%
  select(jaarweek, mean_parity)
