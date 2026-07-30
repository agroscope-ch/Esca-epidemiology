# ==============================================================================
# FORD Variable Selection Analysis
#
# Agnostic variable selection using the FORD (Feature Ordering by Random 
# Distributions) algorithm. This provides a model-free ranking of predictor
# variables, complementing the model-based importance from RF, XGBoost, and LASSO.
#
# Usage: source("ford_analysis.R")
# Output: results/ford_variable_selection.csv
# ==============================================================================

source("esca_ml_config.R")
source("esca_ml_pipeline.R")

library(FORD)

# Resolve data.table vs dplyr/lubridate conflicts (FORD loads data.table)
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflicts_prefer(dplyr::first)
  conflicted::conflicts_prefer(dplyr::between)
  conflicted::conflicts_prefer(dplyr::last)
  conflicted::conflicts_prefer(dplyr::filter)
  conflicted::conflicts_prefer(dplyr::lag)
  conflicted::conflicts_prefer(lubridate::hour)
  conflicted::conflicts_prefer(lubridate::isoweek)
  conflicted::conflicts_prefer(lubridate::isoyear)
  conflicted::conflicts_prefer(lubridate::mday)
  conflicted::conflicts_prefer(lubridate::minute)
  conflicted::conflicts_prefer(lubridate::month)
  conflicted::conflicts_prefer(lubridate::quarter)
  conflicted::conflicts_prefer(lubridate::second)
  conflicted::conflicts_prefer(lubridate::wday)
  conflicted::conflicts_prefer(lubridate::week)
  conflicted::conflicts_prefer(lubridate::yday)
  conflicted::conflicts_prefer(lubridate::year)
  conflicted::conflicts_prefer(purrr::transpose)
}

# --- Helper function to run FORD on a given config ---
run_ford <- function(config, label = "default") {
  cat("\n--- FORD analysis:", label, "---\n\n")
  
  raw_data <- load_esca_data(config)
  prepared_data <- prepare_modeling_data(raw_data, config)
  
  cat("Dataset: ", prepared_data$summary$n_observations, " observations, ",
      prepared_data$summary$n_predictors, " predictors\n")
  
  modeling_df <- prepared_data$data
  response_var <- prepared_data$response
  
  numeric_predictors <- modeling_df |>
    select(-all_of(response_var)) |>
    select(where(is.numeric)) |>
    names()
  
  cat("Numeric predictors for FORD:", length(numeric_predictors), "\n")
  
  X <- modeling_df |>
    select(all_of(numeric_predictors)) |>
    as.matrix()
  Y <- modeling_df[[response_var]] |>
    as.matrix()
  
  cat("Running FORD...\n")
  ford_result <- ford(Y = Y, X = X, stop = TRUE)
  
  # Extract results
  ford_df <- NULL
  if (!is.null(ford_result$selectedVar) && nrow(ford_result$selectedVar) > 0) {
    ford_df <- tibble(
      rank = seq_len(nrow(ford_result$selectedVar)),
      variable_index = ford_result$selectedVar$index,
      Variable = ford_result$selectedVar$names
    )
    if (!is.null(ford_result$step_nu)) {
      ford_df$step_nu <- ford_result$step_nu[seq_len(nrow(ford_df))]
    }
  } else if (!is.null(ford_result$order)) {
    ford_df <- tibble(
      rank = seq_along(ford_result$order),
      variable_index = ford_result$order,
      Variable = numeric_predictors[ford_result$order]
    )
    if (!is.null(ford_result$scores)) {
      ford_df$score <- ford_result$scores[ford_result$order]
    }
  }
  
  if (!is.null(ford_df) && nrow(ford_df) > 0) {
    cat("FORD selected", nrow(ford_df), "variables:\n")
    print(ford_df, n = min(30, nrow(ford_df)))
  } else {
    cat("FORD did not select any variables.\n")
    ford_df <- tibble(rank = integer(), variable_index = integer(),
                      Variable = character(), step_nu = numeric())
  }
  
  ford_df
}

# ==============================================================================
# Run FORD on both monthly and seasonal configurations
# ==============================================================================
cat("===============================================================================\n")
cat("FORD VARIABLE SELECTION ANALYSIS (Monthly & Seasonal)\n")
cat("===============================================================================\n\n")

# --- Monthly (default) ---
config_monthly <- create_esca_config(season_agg_meteo_data = FALSE)
ford_monthly <- run_ford(config_monthly, label = "Monthly")

output_monthly <- file.path(config_monthly$output$results_dir, "ford_variable_selection.csv")
dir.create(config_monthly$output$results_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(ford_monthly, output_monthly)
cat("\nSaved:", output_monthly, "\n")

# --- Seasonal ---
config_seasonal <- create_esca_config(season_agg_meteo_data = TRUE)
ford_seasonal <- run_ford(config_seasonal, label = "Seasonal")

output_seasonal <- file.path(config_seasonal$output$results_dir, "ford_variable_selection_seasonal.csv")
write_csv(ford_seasonal, output_seasonal)
cat("\nSaved:", output_seasonal, "\n")

cat("\n===============================================================================\n")
cat("FORD ANALYSIS COMPLETE\n")
cat("===============================================================================\n")
