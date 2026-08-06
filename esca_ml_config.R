# ===============================================================================
# ESCA ML PIPELINE CONFIGURATION
# 
# Centralized configuration for Esca epidemiological ML analysis
# ===============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidymodels)
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(corrplot)
  library(probably)
  library(vip)
  library(finetune)  # For racing methods
  library(lightgbm)  # Alternative to gbm
  library(doParallel) # For parallel processing
  library(xgboost)   # Alternative boosting engine
  # TabPFN support via reticulate (requires Python virtualenv with tabpfn installed)
  library(reticulate)
  use_virtualenv("../datascience-env/")
  if (requireNamespace("tabpfn", quietly = TRUE)) {
    library(tabpfn)
  }
})

# Set global options
tidymodels_prefer() #  uses the conflicted package to handle common conflicts with tidymodels and other packages.
options(tidymodels.dark = TRUE)

# =============================================================================
# PIPELINE CONFIGURATION
# =============================================================================

#' Create base ESCA configuration
#' @param filter_variety Character string or NULL for variety filtering
#' @param filter_years Numeric vector or NULL for year filtering  
#' @param exclude_year Numeric vector or NULL for year exclusion
#' @param group_year Character string or NULL for group year filtering; 
#' Can be either "2004-2022", "2012-2022" or NULL (no filtering)
#' @param stable_group Character string or NULL for stable_group filtering;
#' Can be either "2004-2022", "2012-2022" or NULL (no filtering)
#' @param exclude_variety Character vector or NULL for variety exclusion
#' @param weather_data_source Character string for weather data source: "merged", "meteoswiss", "vitimeteo", or "both"
#' @param vitimeteo_max_distance_km Maximum distance (km) for Vitimeteo eligibility (default: 30)
#' @param season_agg_meteo_data Logical. If TRUE, only use seasonal aggregate climate variables (SeasXX) and exclude monthly variables.
#' @param compute_contrast_vpd Logical. If TRUE, compute Contrast_MayJune_VPD = VPD_June - VPD_May.
#' @return Complete configuration list
create_esca_config <- function(filter_variety = NULL,
                              filter_years = NULL, 
                              exclude_year = NULL,
                              group_year = NULL,
                              stable_group = NULL,
                              exclude_variety = NULL,
                              weather_data_source = c("merged", "vitimeteo", "meteoswiss", "both"),
                              vitimeteo_max_distance_km = 30,
                              season_agg_meteo_data = FALSE,
                              compute_contrast_vpd = FALSE) {
  
  # Validate and resolve weather_data_source parameter
  weather_data_source <- match.arg(weather_data_source)
  
  list(
    # Random seed for reproducibility
    seed = 1,
    
    # Data configuration
    data = list(
      response_var = "SumSymptApo",
      weather_data_source = weather_data_source,
      vitimeteo_max_distance_km = vitimeteo_max_distance_km,
      exclude_vars = c("plot", "date_observation", "commune", "E", "N", 
                       "percentage_apoplexy", "percentage_symptoms", "RU (mm)", 
                       "meteoswiss_station_abbr", "vitimeteo_station_id", "altitude", "latitude", 
                       "longitude", "station_name",
                       "vitimeteo_distance_km", "distance_to_meteoswiss_km", "vitimeteo_eligible",
                       "vitimeteo_station_name",
                       "source_Temperature", "source_Humidity", "source_Precipitation",
                       "source_Radiation", "source_Evapotranspiration", "source_VPD",
                       "group_year", "stable_group", "region", "Year"),
      # Filtering parameters
      filter_variety = filter_variety,
      filter_years = filter_years,
      exclude_year = exclude_year,
      group_year = group_year,
      stable_group = stable_group,
      exclude_variety = exclude_variety,
      season_agg_meteo_data = season_agg_meteo_data,
      compute_contrast_vpd = compute_contrast_vpd
    ),
    
    # Data splitting
    splits = list(
      prop_train = 0.75,
      prop_val = 0.25,
      cv_folds = 10,
      cv_repeats = 1,
      strata = "SumSymptApo"
    ),
    
    # Preprocessing configuration
    preprocessing = list(
      missing_threshold = 0.5,  # Keep variables with >50% non-missing (allow up to 50% missing)
      near_zero_var = TRUE,     # Remove near-zero variance predictors
      # correlation_cutoff = 0.95, # Remove highly correlated features
      normalize = TRUE,         # Center and scale numeric variables
      dummy_vars = TRUE,        # Create dummy variables for factors
      pca_threshold = NULL      # PCA threshold (NULL = no PCA)
    ),
    
    # Model specifications and hyperparameter grids
    models = list(
      
      # Random Forest
      random_forest = list(
        engine = "ranger",
        mode = "regression",
        grid_size = 10,  # Reduced for stability
        race = FALSE,    # Disable racing for now
        tune_params = list(
          # Parameters to tune (will use tune() in model spec)
          mtry = "tune",
          trees = "tune",
          min_n = "tune"
        ),
        static_params = list(
          importance = "impurity"
        ),
        param_ranges = list(
          mtry = c(2, 10),      # More conservative range
          trees = c(100, 1000), # More conservative range
          min_n = c(2, 20)      # More conservative range
        )
      ),
      
      # Gradient Boosting (using xgboost for better performance)
      gradient_boosting = list(
        engine = "xgboost",
        mode = "regression", 
        grid_size = 15,  # Reduced for stability
        race = FALSE,    # Disable racing for now
        tune_params = list(
          # Parameters to tune (will use tune() in model spec)
          trees = "tune",
          tree_depth = "tune",
          learn_rate = "tune",
          mtry = "tune",
          min_n = "tune",
          loss_reduction = "tune"
        ),
        static_params = list(
          stop_iter = 10
        ),
        param_ranges = list(
          trees = c(50, 500),          # More conservative range
          tree_depth = c(3, 8),        # More conservative range
          learn_rate = c(0.01, 0.2),   # More conservative range
          mtry = c(5, 20),             # More conservative range
          min_n = c(2, 15),            # More conservative range
          loss_reduction = c(0, 1)     # More conservative range
        )
      ),
      
      # LASSO Regression
      lasso = list(
        engine = "glmnet",
        mode = "regression",
        grid_size = 20,  # Reduced for stability
        race = FALSE,
        tune_params = list(
          # Parameters to tune (will use tune() in model spec)
          penalty = "tune",
          mixture = 1  # Pure LASSO - static value
        ),
        static_params = list(),
        param_ranges = list(
          penalty = c(0.001, 1.0)
        )
      ),
      
      # TabPFN (Tabular Prior-Fitted Networks)
      # Note: TabPFN is pre-trained and doesn't require hyperparameter tuning
      tabpfn = list(
        engine = "tabpfn",
        mode = "regression", 
        grid_size = 1,  # No tuning needed
        race = FALSE,
        tune_params = list(),  # No hyperparameters to tune
        static_params = list(
          n_estimators = 32,  # Number of ensemble members
          max_samples = 10000  # Maximum samples for training
        ),
        param_ranges = list(),
        enabled = FALSE  # Requires Python virtualenv with tabpfn + HF_TOKEN (huggingface.co)
      )
    ),
    
    # Performance metrics
    metrics = list(
      primary = "rmse",
      secondary = c("rsq", "mae", "mape")
    ),
    
    # Parallel processing
    parallel = list(
      enabled = FALSE,
      cores = min(8, parallel::detectCores() - 1)  # Use max 8 cores to avoid connection limits
    ),
    
    # Output configuration
    output = list(
      results_dir = "results",
      figures_dir = "plots",
      save_models = TRUE,
      save_predictions = TRUE,
      save_importance = TRUE
    )
  )
}

# =============================================================================
# PREDEFINED CONFIGURATIONS FOR COMMON SCENARIOS
# =============================================================================

#' Configuration 1: Stable plots (2004-2022) with Chasselas only
#' @return Configuration for stable plots analysis
config_stable_chasselas <- function() {
  create_esca_config(
    filter_variety = "chasselas",
    stable_group = "2004-2022"
  )
}

#' Configuration 2: All observations except Sauvignon Blanc
#' @return Configuration excluding Sauvignon Blanc variety
config_exclude_sauvignon <- function() {
  create_esca_config(
    exclude_variety = "sauvignon blanc"
  )
}

#' Configuration 3: Chasselas variety only
#' @return Configuration with only Chasselas variety
config_chasselas_only <- function() {
  create_esca_config(
    filter_variety = "chasselas"
  )
}

#' Configuration 4: No filtering (full dataset)
#' @return Configuration with no data filtering
config_full_dataset <- function() {
  create_esca_config()
}

# =============================================================================
# HELPER FUNCTIONS FOR CUSTOM CONFIGURATIONS
# =============================================================================

#' Modify existing configuration with new filtering parameters
#' @param base_config Base configuration to modify
#' @param ... Named filtering parameters to update
#' @return Modified configuration
modify_config_filters <- function(base_config, ...) {
  new_filters <- list(...)
  for (param_name in names(new_filters)) {
    base_config$data[[param_name]] <- new_filters[[param_name]]
  }
  return(base_config)
}

#' Create configuration with specific varieties
#' @param varieties Character vector of varieties to include
#' @param exclude_varieties Character vector of varieties to exclude
#' @return Configuration with variety filtering
config_by_varieties <- function(varieties = NULL, exclude_varieties = NULL) {
  if (!is.null(varieties) && length(varieties) == 1) {
    return(create_esca_config(filter_variety = varieties[1]))
  } else if (!is.null(exclude_varieties)) {
    return(create_esca_config(exclude_variety = exclude_varieties))
  } else {
    return(create_esca_config())
  }
}

#' Create configuration with specific year range
#' @param year_start Starting year
#' @param year_end Ending year
#' @param exclude_years Years to exclude
#' @return Configuration with year filtering
config_by_years <- function(year_start = NULL, year_end = NULL, exclude_years = NULL) {
  filter_years <- if (!is.null(year_start) && !is.null(year_end)) {
    year_start:year_end
  } else {
    NULL
  }
  
  create_esca_config(
    filter_years = filter_years,
    exclude_year = exclude_years
  )
}

# =============================================================================
# DEFAULT CONFIGURATIONS
# =============================================================================

# Default configuration (no filtering)
ESCA_CONFIG <- create_esca_config()

# Predefined configurations for the user's specific scenarios
ESCA_CONFIG_STABLE_CHASSELAS <- config_stable_chasselas()
ESCA_CONFIG_EXCLUDE_SAUVIGNON <- config_exclude_sauvignon()  
ESCA_CONFIG_CHASSELAS_ONLY <- config_chasselas_only()
# ESCA_CONFIG_FULL <- config_full_dataset() # not usefull

# Validation function to check configuration
validate_config <- function(config) {
  required_sections <- c("data", "splits", "preprocessing", "models", "metrics", "output")
  missing_sections <- setdiff(required_sections, names(config))
  
  if (length(missing_sections) > 0) {
    stop("Missing configuration sections: ", paste(missing_sections, collapse = ", "))
  }
  
  # Check if required models are specified
  if (length(config$models) == 0) {
    stop("No models specified in configuration")
  }
  
  # Check data paths
  required_files <- c(
    "data/Esca_CH_2001-2022.csv",
    "data/meteoswiss_weather_data_1995_2024.csv", 
    "data/plot_meteoswiss_associations.csv",
    "data/plot_station_associations.csv",
    "data/weather_data_Agrometeo_Esca_plots_monthly.csv",
    "data/RU_data.csv"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    warning("Missing data files: ", paste(missing_files, collapse = ", "))
  }
  
  cat("Configuration validation complete.\n")
  return(TRUE)
}

# Validate the default configuration
validate_config(ESCA_CONFIG)
