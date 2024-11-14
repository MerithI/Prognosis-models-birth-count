
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * REFERENCE MODELS *
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * 1a. Last year's mean *
#' Uses the mean of last years weekly birth count 
#' as prognosis for all weeks in the coming year 
#' "prediction intervals" are the quantile values of the previous year around its mean

# data_week

fc_baseline <- function(data) {
  
  # tibble with last year's mean
  data_lastyear <- data %>%
    group_by(isojaar) %>%
    summarise(mean_lastyear = mean(n),
              ci_lower_80 = quantile(n, (1 - 0.80) / 2),
              ci_upper_80 = quantile(n, 1 - (1 - 0.80) / 2),
              ci_lower_95 = quantile(n, (1 - 0.95) / 2),
              ci_upper_95 = quantile(n, 1 - (1 - 0.95) / 2)) %>%
    mutate(isojaar = isojaar + 1)
  
  # join last year's mean tibble to data
  data_bl <- data %>%
    left_join(data_lastyear, by = c("isojaar" = "isojaar")) %>%
    filter(!is.na(mean_lastyear))
  
  return(data_bl)
}


#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' * #1b. Due dates only *
# "Prediction intervals" are the quantiles of the test set residuals

# Weekly due dates
tmp_at <- df %>%
  count(a_terme_datum, name = "n_at")

tmp_due <- df %>%
  count(geboorte_datum, name = "n_geb")

data_dag <- full_join(tmp_at, tmp_due, by = c("a_terme_datum" = "geboorte_datum")) %>%
  replace(is.na(.), 0)

data_week <- data_dag %>% 
  group_by(isoweek, isojaar) %>% 
  summarize(n_at = sum(n_at),
            n_geb = sum(n_geb))

# Calculate residuals for the "test data"
data_1217 <- data_week %>%
  filter(jaarweek >= yearweek("2012 w05"),
         jaarweek < yearweek("2018 w01")) %>%
  mutate(residuals = n - n_at)

# Spread of residuals
spread_dd <- quantile(data_1217$residuals, probs = c(0.1, 0.9, 0.025, 0.975))

# Apply interval to expected due birth counts
data_1821 <- data_1821 %>%
  mutate(ci_lower_80 = n_at + spread_dd[1],
         ci_upper_80 = n_at + spread_dd[2],
         ci_lower_95 = n_at + spread_dd[3],
         ci_upper_95 = n_at + spread_dd[4])
