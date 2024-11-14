This contains scripts for birth count prognosis models as developed for thesis, as well as several functions used along the way.

Each model group (1 up to 6) has its own script:
1) Reference models (last year's mean and due date only)
2) SARIMA (historical birth patterns)
3) Due date + distribution (simulation, analytical aproach, only currently known pregnancies, and with imputation for unknown pregnancies)
4) SARIMAX (combi of 2 and 3)
5) 2 and 3 at regional level
6) Additional variations (hospital births, daily births, parity)








Analyses were performed in Rstudio. Most analyses were done in the following version, with the following packages:
sessionInfo()
> sessionInfo()
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=Dutch_Netherlands.utf8  LC_CTYPE=Dutch_Netherlands.utf8    LC_MONETARY=Dutch_Netherlands.utf8
[4] LC_NUMERIC=C                       LC_TIME=Dutch_Netherlands.utf8

time zone: Europe/Amsterdam
tzcode source: internal

attached base packages:
  [1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] future.apply_1.11.2 future_1.33.2       Metrics_0.1.4       tseries_0.10-56     forecast_8.22.0     tsibble_1.1.4
[7] feasts_0.3.2        fable_0.3.4         fabletools_0.4.2    writexl_1.5.0       RColorBrewer_1.1-3  MASS_7.3-60
[13] zoo_1.8-12          lubridate_1.9.3     forcats_1.0.0       stringr_1.5.1       dplyr_1.1.3         purrr_1.0.2
[19] readr_2.1.4         tidyr_1.3.0         tibble_3.2.1        ggplot2_3.5.1       tidyverse_2.0.0
