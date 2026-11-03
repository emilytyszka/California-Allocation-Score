
# Making the output from the previous code a default model list
models_to_include <- c("COVIDhub-baseline", "COVIDhub-ensemble", "COVIDhub-trained_ensemble","CU-select", 
                       "CUBoulder-COVIDLSTM", "FAIR-NRAR", "JHUAPL-Bucky", "UChicagoCHATTOPADHYAY-UnIT", "UVA-Ensemble" )  

# Forecast Data Pull
get_forecast_data <- function(forecast_dates, models = models_to_include, locations, timehorizon, dates_of_interest){
  require(covidHubUtils)
  load_forecasts(
    dates = forecast_dates,
    date_window_size = 6,
    models = models,
    locations = locations,
    types = c("quantile"),
    targets = paste(1:3, "wk ahead inc case"),
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter(target_end_date %in% dates_of_interest) |>
    dplyr::filter(horizon == timehorizon) |> # use "horizon" for JHU data. >1 horizon creates alloscore() merge errors
    mutate(model_date_horizon = paste(model, target_end_date, horizon)) |>
    dplyr::filter(model_date_horizon != "CU-select 2021-12-18 3") |>
    ungroup() |>
    select(-model_date_horizon)
  }

# Create Ensemble of Available Forecasts for a Given Day
generate_ensemble <- function(forecast_data){
  require(dplyr)
  forecast_data_new <- forecast_data %>% dplyr::filter(!str_detect(model, "ensemble")) %>% #REMOVE ensembles (but leave UVA-Ensemble - it's an ensemble of something else)
    dplyr::group_by(reference_date, location, horizon, relative_horizon, temporal_resolution, target_variable, 
                           target_end_date, type, quantile, location_name, population, geo_type, geo_value, abbreviation, 
                           full_location_name) %>%                                 # everything is grouped except for "model" and "value"
                    dplyr::summarise(value=mean(value),                            # calculates mean estimate of value for all models for given quantile + date + location
                                     forecast_date = max(forecast_date)) %>%       # we need a forecast date for scoring - using the latest forecast date appearing in the constituent models
                    mutate(model = "eligible-models-ensemble") %>% 
                    ungroup() 
  forecast_data <- forecast_data %>% rbind(forecast_data_new)
}


# Truth Data Pull
get_truth_data <- function(){
  require(covidHubUtils)
  truth_data <- load_truth(
    truth_source = "JHU",
    target_variable = "inc case"
  ) %>%
    dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) %>%
    dplyr::mutate(value = pmax(value, 0))
  return(truth_data)
}


# Scores
get_forecast_scores <- function(forecast_data, truth_data){
  require(covidHubUtils)
  score_forecasts(
    forecasts = forecast_data,
    truth = truth_data,
    use_median_as_point = TRUE
  )
}

# Hubverse version of the get_forecast_data function
get_forecast_data_hubverse <- function(target_dates, models = models_to_include, locations, timehorizon, dates_of_interest){
require(hubUtils)
require(hubData)
bucket_name <- "covid19-forecast-hub-archive"
hub_bucket <- s3_bucket(bucket_name)
hub_con <- hubData::connect_hub(hub_bucket, file_format = "parquet", skip_checks = TRUE)
forecast_data_hub <- hub_con %>%
  dplyr::filter(location %in% locations, model_id %in% models_to_include, target_end_date %in% target_dates, 
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
}