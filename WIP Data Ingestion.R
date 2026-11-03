
library(covidHubUtils)
forecast_dates <- as.character(seq.Date(as.Date("2021-12-04"), as.Date("2022-03-12"), by = "7 days"))
target_dates <- as.character(seq.Date(as.Date("2021-12-11"), as.Date("2022-03-19"), by = "7 days"))
locations <- hub_locations |>
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) |>
  dplyr::pull(fips)
timehorizon <- 1

# Forecast Data Pull
forecast_data <- load_forecasts(                                                 # Only changed the target weeks. NB: timehorizon and target_end_date filtering turned off here
    dates = forecast_dates,
    date_window_size = 6,
    models = models_to_include,
    locations = locations,
    types = c("quantile"),
    targets = paste(1:3, "wk ahead inc case"),
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter(horizon == timehorizon) |> #|> # use "horizon" for JHU data. >1 horizon creates alloscore() merge errors
    select(-forecast_date) |>                    # removing "forecast_date" because it isn't used after this and it creates a headache in ensembling
    #dplyr::filter(target_end_date == date_of_interest) # adding the option to narrow the data being processed to just the dates where we want N week horizon
    mutate(model_date_horizon = paste(model, target_end_date, horizon)) |>
    dplyr::filter(model_date_horizon != "CU-select 2021-12-18 3") |>
    ungroup() |>
    select(-model_date_horizon)

# Create Ensemble of Available Forecasts for a Given Day
forecast_data <- forecast_data %>% 
  dplyr::filter(!str_detect(model, "ensemble")) %>% 
  group_by(reference_date, location, horizon, relative_horizon, temporal_resolution, target_variable, 
                                                target_end_date, type, quantile, location_name, population, geo_type, geo_value, abbreviation, 
                                                full_location_name) %>%                                 # everything is grouped except for "model" and "value"
                                       summarise(value=mean(value)) %>%                                 # calculates mean estimate for the quantile + date + location
                                       mutate(model = "eligible-models-ensemble") %>% 
                                       ungroup() %>% rbind(forecast_data)


# Truth Data Pull
truth_test <- load_truth(                                                        # TWEAKED THE FILTERING
  truth_source = "JHU",
  target_variable = "inc case"
) %>%
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) %>%
  dplyr::mutate(value = pmax(value, 0))


# Scores
get_forecast_scores <- function(forecast_data, truth_data){                      # NO ISSUES
  require(covidHubUtils)
  score_forecasts(
    forecasts = forecast_data,
    truth = truth_data,
    use_median_as_point = TRUE
  )
}


# Look for weeks with incorrect amounts of data - ONLY IN CU SELECT AT 3 WEEK ON 12-18
tar_load("forecast_data1wk_raw")
tar_load("forecast_data3wk_raw")
test_1wk <- forecast_data1wk_raw %>% group_by(model, target_end_date, quantile) %>% summarise(count = n()) %>%
  dplyr::filter(count !=57) %>% mutate(model_date = paste(model, target_end_date)) %>% distinct(model_date) %>% ungroup() %>%
  select("model_date")
test_3wk <- forecast_data3wk_raw %>% group_by(model, target_end_date, quantile) %>% summarise(count = n()) %>%
  dplyr::filter(count !=57) %>% mutate(model_date = paste(model, target_end_date)) %>% distinct(model_date) %>% ungroup() %>%
  select("model_date")


# Old "Filter Problematic Data" function
filter_problematic_data <- function(forecast_data){                              # THIS IS NO LONGER NEEDED
  require(tidyverse)
  forecast_data |>
    ## this removes two state-week-model that has a deterministically zero forecast
    filter(!(reference_date=="2021-12-18" & model=="CU-select" & horizon == "3"))
}

# Hubverse data
tar_load(eligible_models)
### I have confirmed that, for the dates of interest, the hub (filtered to target date) and zoltar (using the existing forecast date method) pulls give the same number of predictions for the dates of interest 
### These datasets are agonizing to align, since the "forecast date" is the way zoltar pulls are requested, and that doesn't work for hub pulls as far as I can tell.
library(hubUtils)
library(hubData)
#### Populations - move the pops2022 to earlier in the pipeline
pops2022 <- readxl::read_excel("data/CA_DeptOfFinance_PopEstim_2021-2025.xlsx", sheet = "Table 1 County State", skip = 2) |>
  drop_na(`1/1/2021`) |>
  mutate(full_location_name = ifelse(County == "State Total", "California",
                                     paste(`County`, "County, CA")),
         POPESTIMATE2021 = `1/1/2021`) |> select(full_location_name, POPESTIMATE2021) |>
  dplyr::inner_join(hub_locations, by = join_by(full_location_name))
####
require(hubUtils)
require(hubData)
bucket_name <- "covid19-forecast-hub-archive"
hub_bucket <- s3_bucket(bucket_name)
hub_con <- hubData::connect_hub(hub_bucket, file_format = "parquet", skip_checks = TRUE)
forecast_data_hub <- hub_con %>%
  dplyr::filter(location %in% locations, model_id %in% eligible_models, target_end_date %in% target_dates, 
                horizon==timehorizon, output_type == "quantile") %>%
  hubData::collect_hub()
forecast_data_hub2 <- forecast_data_hub %>% rename(model = model_id, # rename model_id to model
                                                   target_variable = target, # rename target to target_variable
                                                   type = output_type,
                                                   quantile = output_type_id) %>%
                                            mutate("temporal_resolution" = "wk", 
                                                   "geo_value" = location)  %>% 
  merge(pops2022, by.x="location", by.y= "geo_value") %>% 
  select(model, forecast_date, location, horizon, temporal_resolution, target_variable, 
         target_end_date, type, quantile, value, location_name, population, geo_type,
         geo_value, abbreviation, full_location_name) %>%
  select(-forecast_date) |>                    # removing "forecast_date" because it isn't used after this and it creates a headache in ensembling
  #dplyr::filter(target_end_date == date_of_interest) # adding the option to narrow the data being processed to just the dates where we want N week horizon
  mutate(model_date_horizon = paste(model, target_end_date, horizon)) |>
  dplyr::filter(model_date_horizon != "CU-select 2021-12-18 3") |>
  ungroup() |>
  select(-model_date_horizon)

####### TESTING
generate_ensemble(get_forecast_data(values$forecast_dates, models = models_to_include, 
                  locations = reqd_locs, timehorizon=timehorizon1))
