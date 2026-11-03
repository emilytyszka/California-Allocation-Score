### Diagnose CU-select on 12-18 3 week - why are there 56 counties only?

models_to_drop <- c("COVIDhub_CDC-ensemble",
                    "CU-nochange", "CU-scenario_low", "CU-scenario_mid")
datestopull <- tibble(forecast_dates = as.character(seq.Date(as.Date("2021-11-06"), as.Date("2022-03-12"), by = "7 days"))) %>% drop_na()
reqd_locs <- hub_locations |>
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) |>
  dplyr::pull(fips)

## load and filter forecasts
alldata <- load_forecasts(
  dates = datestopull$forecast_dates,
  date_window_size = 6,
  types = c("quantile"),
  targets = paste(1:3, "wk ahead inc case"),
  locations = reqd_locs,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")) |>
  align_forecasts()

tar_load("alloscores3wk")

CU_Dec18_3wk_scores <- alloscores3wk %>% dplyr::filter(target_end_date=="2021-12-18" & model=="CU-select") 
scorecountbyK <- CU_Dec18_3wk_scores %>% group_by(K) %>% summarise(count = n()) # 56 at all Ks for 12/18
scoredcounties <- CU_Dec18_3wk_scores %>% distinct(full_location_name)

CU_3wk_forecast <- alldata %>% dplyr::filter(model=="CU-select" & quantile==0.75 & horizon == 3)
forecastcountbydate <- CU_3wk_forecast %>% group_by(target_end_date) %>% summarise(count = n()) #goes from 56 to 57 on the 25th - this is a true difference
