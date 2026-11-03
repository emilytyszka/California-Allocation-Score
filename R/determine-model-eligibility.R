determine_eligible_models <- function(forecast_dates, locations){
  require(covidHubUtils)
  require(tidyverse)

  ## Drop covidhub ensembles not of interest: COVIDhub_CDC-ensemble, COVIDhub-ensemble, COVIDhub-trained_ensemble (keep COVIDhub-4_week_ensemble)
  ## drop CU non-primary models: CU-nochange, CU-scenario_low, CU-scenario_mid
  
  models_to_drop <- c("COVIDhub_CDC-ensemble", "COVIDhub-ensemble", "COVIDhub-trained_ensemble",
                      "CU-nochange", "CU-scenario_low", "CU-scenario_mid", "UChicagoCHATTOPADHYAY-UnIT", 
                      "FAIR-NRAR") # UChicago and FAIR dropped from pipeline due to completeness - only report 3 and 5 times respectively

  ## load and filter forecasts
  load_forecasts(
    dates = forecast_dates,
    date_window_size = 6,
    types = c("quantile"),
    targets = paste(1:3, "wk ahead inc case"),
    locations = locations,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>                   # After running this line: 17 models
    dplyr::filter(
      !(model %in% models_to_drop)) |>    # After running this line: 10 models 
    dplyr::group_by(model, reference_date) |>
    dplyr::summarize(nlocs = length(unique(location_name))) |>
    ungroup() |>
    dplyr::filter(nlocs == length(locations)) |>  # After running this line: 7 models (drops PandemicCentral-COVIDForest, JHU_UNC_GAS-StatMechPool, FRBSF_Wilson-Econometric)
    pull(model) |>
    unique()
}
