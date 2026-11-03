library(covidHubUtils)
library(tidyverse)


forecast_dates <- as.character(seq.Date(as.Date("2021-12-18"), as.Date("2022-03-12"), by = "7 days"))
locations <- hub_locations |>
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) |>
  dplyr::pull(fips)


## Drop several covidhub ensembles: COVIDhub-4_week_ensemble, COVIDhub_CDC-ensemble (keep trained and COVIDhub-ensemble)
## drop CU non-primary models: CU-nochange, CU-scenario_low, CU-scenario_mid
models_to_drop <- c("COVIDhub_CDC-ensemble",
                    "CU-nochange", "CU-scenario_low", "CU-scenario_mid")

## load and filter forecasts
alldata <- load_forecasts(
  dates = forecast_dates,
  date_window_size = 6,
  types = c("quantile"),
  targets = paste(1:3, "wk ahead inc case"),
  locations = locations,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")) |>
  align_forecasts() 
alldata_n <- unique(alldata$model) # 17 models

# drop the models we explicitly don't want 
alldata_drop <- alldata |>
  dplyr::filter(
    !(model %in% models_to_drop)) 
alldata_drop_n <- unique(alldata_drop$model) # 13 models

# all 57 locations
alldata_completeloc <- alldata_drop |>
  group_by(model, reference_date, horizon) |>
  summarize(nlocs = length(unique(location_name))) |>
  ungroup()

alldata_completeloc_n <- unique(alldata_completeloc$model) # 10 models - dropped PandemicCentral-COVIDForest, JHU_UNC_GAS-StatMechPool, FRBSF_Wilson-Econometric

# get final list
modellist <- alldata_completeloc_n

