# ==============================================================================
# Run all predefined configurations and store results separately
# ==============================================================================

source("esca_ml_config.R")
source("esca_ml_pipeline.R")

# Define configurations with labels and output directories
configs <- list(
  # list(
  #   name = "full_dataset",
  #   label = "Full Dataset (no filtering)",
  #   config = create_esca_config()
  # ),
  list(
    name = "chasselas_only",
    label = "Chasselas Only",
    config = create_esca_config(filter_variety = "chasselas")
  ),
  list(
    name = "exclude_sauvignon",
    label = "Exclude Sauvignon Blanc",
    config = create_esca_config(exclude_variety = "sauvignon_blanc")
  ),
  list(
    name = "stable_chasselas",
    label = "Stable Chasselas (2004-2022)",
    config = create_esca_config(filter_variety = "chasselas", group_year = "2004-2022")
  ),
  list(
    name = "main_varieties",
    label = "Main varieties",
    config = create_esca_config(filter_variety = c("chasselas", "gamay", "pinot noir", "pinot gris") )
  ),
  list(
    name = "chasselas_seasonal",
    label = "Chasselas Only (Seasonal Agg)",
    config = create_esca_config(filter_variety = "chasselas", season_agg_meteo_data = TRUE)
  ),
  list(
    name = "exclude_sauvignon_seasonal",
    label = "Exclude Sauvignon Blanc (Seasonal Agg)",
    config = create_esca_config(exclude_variety = "sauvignon_blanc", season_agg_meteo_data = TRUE)
  ),
  list(
    name = "stable_chasselas_seasonal",
    label = "Stable Chasselas (2004-2022, Seasonal Agg)",
    config = create_esca_config(filter_variety = "chasselas", group_year = "2004-2022", season_agg_meteo_data = TRUE)
  ),
  list(
    name = "main_varieties_seasonnal",
    label = "Main varieties (Seasonal Agg)",
    config = create_esca_config(filter_variety = c("chasselas", "gamay", "pinot noir", "pinot gris") , season_agg_meteo_data = TRUE)
  )
)

# Store all results for later use
all_results <- list()

for (cfg in configs) {
  cat("\n\n")
  cat("################################################################\n")
  cat("## RUNNING:", cfg$label, "\n")
  cat("################################################################\n\n")
  
  # Set output directory per configuration
  cfg$config$output$results_dir <- file.path("results", cfg$name)
  cfg$config$output$figures_dir <- file.path("figures", cfg$name)
  
  tryCatch({
    result <- run_esca_ml_pipeline(config = cfg$config, save_results = TRUE)
    all_results[[cfg$name]] <- result
    cat("\n>> SUCCESS:", cfg$label, "\n")
  }, error = function(e) {
    cat("\n>> ERROR in", cfg$label, ":", conditionMessage(e), "\n")
    all_results[[cfg$name]] <<- list(error = conditionMessage(e))
  })
}

# ==============================================================================
# Create combined summary table across all configurations
# ==============================================================================

cat("\n\n")
cat("################################################################\n")
cat("## COMBINED RESULTS SUMMARY\n")
cat("################################################################\n\n")

summary_rows <- list()

for (cfg in configs) {
  res <- all_results[[cfg$name]]
  if (!is.null(res$error)) {
    cat("SKIPPED (error):", cfg$label, "\n")
    next
  }
  
  # Extract test performance
  test_perf <- res$performance$test
  
  for (model_name in unique(test_perf$model)) {
    for (metric_name in unique(test_perf$.metric)) {
      val <- test_perf |> 
        filter(model == model_name, .metric == metric_name) |> 
        pull(.estimate)
      
      summary_rows[[length(summary_rows) + 1]] <- tibble(
        configuration = cfg$label,
        config_id = cfg$name,
        model = model_name,
        metric = metric_name,
        value = val
      )
    }
  }
  
  # Extract top 5 variable importance per model
  for (model_name in names(res$importance)) {
    imp <- res$importance[[model_name]]
    if (!is.null(imp) && nrow(imp) > 0) {
      top5 <- head(imp, 5)
      write_csv(
        top5 |> mutate(configuration = cfg$label, config_id = cfg$name),
        file.path("results", cfg$name, paste0("top5_importance_", model_name, ".csv"))
      )
    }
  }
}

# Combine into a single summary table
combined_summary <- bind_rows(summary_rows)

# Save combined summary
dir.create("results", showWarnings = FALSE)
write_csv(combined_summary, "results/combined_performance_summary.csv")

# Print nicely
cat("\n--- Test Set Performance (all configurations) ---\n\n")
combined_wide <- combined_summary |>
  pivot_wider(names_from = metric, values_from = value) |>
  arrange(configuration, model)
print(combined_wide, n = 50)

# Save the full results object for later use
saveRDS(all_results, "results/all_configurations_results.rds")

cat("\n\nAll results saved to:\n")
cat("  - results/combined_performance_summary.csv  (comparison table)\n")
cat("  - results/all_configurations_results.rds    (full R objects)\n")
cat("  - results/<config_name>/                    (per-config outputs)\n")

# ==============================================================================
# Compute combined variable importance (weighted by model performance)
# ==============================================================================
cat("\n\n")
cat("################################################################\n")
cat("## COMBINED VARIABLE IMPORTANCE\n")
cat("################################################################\n\n")

source("combined_variable_importance.R")
compute_combined_importance()

cat("\nDONE.\n")
