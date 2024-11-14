
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' *3: Due date + distribution models*
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# Consists of 3 functions:
# function fc_duedate_analytical: analytical approach
# function fc_duedate_help: simulation approach. Is model 3a1 when default values are used
# function fc_duedate_window: is used for model 3b and 3c. 
#                    It uses fc_due_date_help for the forecasting, 
#                    but applies a loop to enable rolling forecasts and include only
#                    pregnancies < x weeks,
#                    and apply mean imputation for younger pregnancies


#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' *Function model 3a2 (analytical approach)**
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Uses df (each row is one birth)

fc_duedate_analytical <- function(df){
  
  
  #############################################################################  
  ############ make probability distribution based on train dataset
  
  
  #' timeframe train
  date_start_train <- as_date("2012-01-01")
  date_end_train   <- as_date("2017-12-31")
  date_seq_train   <- seq.Date(date_start_train, date_end_train, by = "day")
  
  df_train <- df %>%
    split_df(date_start_train,date_end_train)
  
  
  #' Tibble probablity distribution around due date 
  df_tmp <- df_train %>%
    make_edf(dif)         
  
  df_edf <- tibble(dif = min(df_tmp$dif):max(df_tmp$dif))
  
  df_edf <- df_edf %>%
    left_join(df_tmp, by = "dif") %>%
    replace(is.na(.), 0)
  
  
  
  #############################################################################  
  ############ Prepare tibbles for realized and due births 
  
  
  #' timeframe total prognosis = 1-12-2012 tm 31-12-2021  
  date_start_prog  <- min(df$geboorte_datum)
  date_end_prog    <- max(df$geboorte_datum)
  date_seq_prog    <- seq.Date(date_start_prog, date_end_prog, by = "day")
  
  df_prog <- df %>%
    split_df(date_start_prog, date_end_prog)
  
  
  #' timeframe range = earliest possible due data for first date in timeframe 
  #'            and last possible due date for last date in timeframe  
  date_start_range  <- min(df$geboorte_datum) - abs(max(df_train$dif))
  date_end_range    <- max(df$geboorte_datum) + abs(min(df_train$dif))
  date_seq_range    <- seq.Date(date_start_range, date_end_range, by = "day")
  
  df_range <- df %>%
    split_df(date_start_range, date_end_range)
  
  
  #' df_n_tmp: Tibble with per date the number of realized and due births
  df_n_geb <- df_prog %>% 
    count(geboorte_datum, name = "n_geb")
  
  df_n_at <- df_prog %>%
    count(a_terme_datum, name = "n_at")
  
  df_n_tmp <- tibble(
    datum    = date_seq_prog) %>% 
    
    left_join(df_n_geb, by = c("datum" = "geboorte_datum")) %>%
    left_join(df_n_at, by = c("datum" = "a_terme_datum")) %>%
    
    replace(is.na(.), 0)
  
  
  #############################################################################  
  ############ Prepare probability matrix and tibble for prognosis 
  # df_prog: temporary tibble, to be multiplied with  probabilities
  #          columns per date, filled with the number of due births for that date. 
  #          and a column 'datum' for the dates in the prognosis dataset, needed for joining the table later 
  
  # Tibble nr of due births
  df_prog <- tibble(
    datum    = date_seq_prog, 
    weekdag = weekdays(datum))
  
  for (date in date_seq_range){
    date_col          <- as.character(as.Date(date,origin = "1970-01-01"))
    print(date_col)
    ifelse(as.Date(date,origin = "1970-01-01") %in% df_n_tmp$datum, 
           df_prog[[date_col]] <- as.double(df_n_tmp$n_at[date_col == df_n_tmp$datum]), 
           df_prog[[date_col]] <- as.double(0))
  }
  
  
  # Matrix with probabilities for multiplying
  # In this format:
  # .          |  2014/1/1 - max(dif)   |   2014/1/1 - max(dif) + 1   |  ...  |   2014/1/1    | ...
  # 2014/1/1   |  p[dif = max(dif)]     |   p[dif = max(dif) + 1]     |  ...  |   p[dif = 0]  | ...
  # 2014/1/2   | 
  # 2014/1/3   |
  
  # v_edf_rev = probability distribution mirrored: from max(dif) to min(dif)
  # m_edf     = repeated by row (as many times as the length of date_prog_range)
  # shift_rows_right shifts every subsequent row one step to the right compared to the previous
  
  v_edf_rev <- rev(df_edf$p)
  m_edf <- matrix(
    rep(v_edf_rev, times = length(date_seq_prog)), 
    ncol= length(v_edf_rev), 
    byrow = TRUE) %>%
    
    shift_rows_right()
  
  
  #############################################################################  
  ############ tibble prognosis births
  
  # multiply probability matrix with the tibble with the nr of due births per date
  df_prog <- df_prog %>%
    mutate(
      across(-any_of(c("datum", "weekdag")) , ~ .) * m_edf)
  
  
  # Make new tibble with the prognosis
  df_n <- df_prog %>%
    rowwise() %>%
    mutate(n_prognose = sum(c_across(-any_of(c("datum", "weekdag"))))) %>%
    select(datum, weekdag, n_prognose) %>%
    ungroup() %>%
    
    left_join(df_n_tmp, by = "datum")
  
  return(df_n)
  
  
}





##################################################
#' *3a1: Duedate + distribution: simulation  model using all pregnancies*

# uses daily nr of due births & emperical probability distribution
# outputs weekly prognosis


fc_duedate_help <- function(
    data,                                 # data: nr of due births per date
    pdf,                                  # empirical probability distribution function of test set
    date_start = as_date("2017-12-01"),   # first possible due date with influence on prognosis
    date_end = as_date("2021-12-31"),     # last possible due date " " " 
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
    
    #Draw a realised dif (# days after due date) from emperical distribution with probability p
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
      realisation_vec[missing_dates] <- 0}
    
    realisation_vec[as.character(date_seq)]
    
  }, simplify = TRUE)
  
  
  #transform to long format
  realisations <- as_tibble(t(realisations))
  
  result <- realisations %>%
    mutate(rep = 1:nrow(realisations)) %>%
    pivot_longer(cols = -rep, names_to = "datum", values_to = "n_sim") %>%
    mutate(
      datum = lubridate::ymd(datum),
      jaarweek = tsibble::yearweek(datum)
    )
  
  # summarize per yearweek and calculate quantile prediction interval thresholds 
  result_summarized <- result %>%
    group_by(rep, jaarweek) %>%
    summarize(n_sim = sum(n_sim)  ) %>%
    
    group_by(jaarweek) %>%
    summarise (
      q025 = quantile(n_sim, 0.025),
      q100 = quantile(n_sim, 0.1),
      q900 = quantile(n_sim, 0.9),
      q975 = quantile(n_sim, 0.975),
      mean = mean(n_sim))
}



##################################################
#' * 3b and 3c DueDate+. Loop function* 
#' This model is used for both 3b and 3c. 
#' At each step, it selects the "currently known pregnancies with gestational age >=16 weeks"
#' and then calls the function fc_duedate_help for the actual forecast

# Uses individual data (each row 1 pregnancy)
# Returns weekly prognosis

fc_duedate_window <- function(
    data_individual,            # data, each row: 1 pregnancy with a due date & birth date
    pdf,                        # empirical distribution for national level or that region
    step_size = 4,                # for how many weeks you want to forecast
    forecast_window = c(17, 20),  # Forecast window in weeks. c(a, b), where b-a == stepsize
    forecast_start,             # yearweek
    forecast_end,               # yearweek
    nr_weeks_pregnant,          # include pregnancies that are already x+ weeks at forecasting moment
    weeks_to_add,               # added nr of weeks for correction of unknown recent pregnancies
    nRep,                       # nr of simulations
    correction){                # If TRUE, apply mean imputation for recent pregnancies
  
  # Preparation
  container <- tibble(
    jaarweek = yearweek(character()),
    q025     = numeric(), 
    q100     = numeric(),
    q900     = numeric(), 
    q975     = numeric(),
    mean     = numeric()
  )
  
  forecast_start <- as_date(forecast_start)
  forecast_end   <- as_date(forecast_end)
  
  date_prognosismaking_first <- forecast_start - (7 * (min(forecast_window)) - 6) # fc start - 113
  date_prognosismaking_last  <- as_date(forecast_end) - 14*7
  
  # Loop: every step_size
  time_start <- Sys.time()
  for (date_prognosismaking in seq(date_prognosismaking_first, date_prognosismaking_last, by = (step_size *7 ))){
    date_prognosismaking <- as_date(date_prognosismaking)
    
    # Set start and end date for the to be forecasted period
    date_start2 = date_prognosismaking + 7 * (min(forecast_window)) - 6 # e.g. 113 days ahead
    date_end2 =   date_prognosismaking + 7 * max(forecast_window)       # e.g. 140 days ahead
    print(paste("Making prognosis from date", date_start2, "to date", date_end2))
    
    
    # Get group of pregnancies of x weeks or longer
    data_tmp <- data_individual %>% 
      filter(
        geboorte_datum >= date_prognosismaking &             # not born yet (on the previous day)
          date_prognosismaking - start_pregnancy > nr_weeks_pregnant*7 &  # 16 weeks or longer pregnant
          date_start2 - a_terme_datum <= 35 # birth is max 35 days after the due date
      )
    
    
    # Group by due date
    data_tmp <- data_tmp %>%
      group_by(a_terme_datum) %>%
      summarize(n_at = n()) %>%
      ungroup() %>%
      dplyr::rename(datum = a_terme_datum)
    
    # If correction for unavailable pregnancies is applied:
    #    add [weeks_to_add] * 7 days with average daily nr of due births from the whole subset
    if(correction){
      extra_dates <- tibble(
        datum = seq(max(data_tmp$datum) + 1, by = "day", length.out = weeks_to_add * 7),
        n_at = mean(data_tmp$n_at)
      )
      
      data_tmp <- bind_rows(data_tmp, extra_dates)
      # print("correction applied")
    }
    
    
    # Make forecast by simulating birth counts based on nr of pregnancies on each due date 
                    # + empirical probability distribution pdf
    result <- fc_duedate_help(data_tmp,
                              date_start = min(data_tmp$datum),  # use all due dates in subset
                              date_end = max(data_tmp$datum),
                              date_start2 = date_start2,        
                              date_end2 = date_end2,
                              nRep = nRep,
                              pdf = pdf)
    
    container <- rbind(container, result)
    
  }
  
  # Time registration end loop
  print(paste("Starttime:", time_start,"  Endtime:", Sys.time(),"  Duration:", (Sys.time()-time_start)))
  
  return(container)
  
}


