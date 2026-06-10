# ==============================================================================
# Combined Variable Importance
# 
# Methodology:
# 1. Rescale each model's importance to [0, 1] (divide by max)
# 2. Compute model weights as relative MAE improvement over baseline:
#    w_m = max(0, (MAE_baseline - MAE_m) / MAE_baseline)
# 3. Weighted average across models:
#    Combined_i = sum(w_m * Scaled_i^m) / sum(w_m)
#
# Output: results/combined_variable_importance.csv
# ==============================================================================

library(tidyverse)

compute_combined_importance <- function(results_dir = "results") {
  
  # Load performance summary
  perf <- read_csv(file.path(results_dir, "combined_performance_summary.csv"),
                   show_col_types = FALSE)
  
  # Get baseline MAE per config
  baseline_mae <- perf |>
    filter(model == "baseline_year_region", metric == "mae") |>
    select(config_id, configuration, baseline_mae = value)
  
  # Get model MAEs (exclude baseline)
  model_mae <- perf |>
    filter(model != "baseline_year_region", metric == "mae") |>
    select(config_id, configuration, model, mae = value) |>
    left_join(baseline_mae |> select(config_id, baseline_mae), by = "config_id")
  
  # Compute model weights: relative improvement over baseline (clamped at 0)
  model_weights <- model_mae |>
    mutate(weight = pmax(0, (baseline_mae - mae) / baseline_mae)) |>
    select(config_id, configuration, model, weight)
  
  cat("Model weights (MAE improvement over baseline):\n")
  print(model_weights, n = 30)
  
  # Load variable importance for each config
  config_ids <- unique(perf$config_id)
  
  all_importance <- map_dfr(config_ids, function(cid) {
    f <- file.path(results_dir, cid, "variable_importance_tidymodels.csv")
    if (file.exists(f)) {
      read_csv(f, show_col_types = FALSE) |> mutate(config_id = cid)
    } else {
      NULL
    }
  })
  
  if (nrow(all_importance) == 0) {
    stop("No variable importance files found in results/")
  }
  
  # Step 1: Rescale to [0, 1] per model per config
  # Guard against max = 0 (e.g. lasso with all-zero coefficients)
  scaled_importance <- all_importance |>
    group_by(config_id, model) |>
    mutate(max_imp = max(Importance, na.rm = TRUE),
           scaled = if_else(max_imp > 0, Importance / max_imp, 0)) |>
    select(-max_imp) |>
    ungroup()
  
  # Step 2: Join weights
  scaled_with_weights <- scaled_importance |>
    left_join(model_weights, by = c("config_id", "model")) |>
    filter(!is.na(weight), weight > 0)  # Exclude models that don't beat baseline
  
  # Step 3: Weighted combination per config
  combined_per_config <- scaled_with_weights |>
    group_by(config_id, configuration, Variable) |>
    summarise(
      combined_importance = sum(weight * scaled) / sum(weight),
      n_models = n(),
      models_contributing = paste(model, collapse = ", "),
      .groups = "drop"
    ) |>
    arrange(config_id, desc(combined_importance))
  
  # Also compute an overall combined importance (across all configs)
  # Using same approach: weight by config's best model improvement
  best_per_config <- model_weights |>
    group_by(config_id, configuration) |>
    slice_max(weight, n = 1) |>
    ungroup() |>
    select(config_id, config_weight = weight)
  
  combined_overall <- combined_per_config |>
    left_join(best_per_config, by = "config_id") |>
    group_by(Variable) |>
    summarise(
      overall_importance = sum(config_weight * combined_importance) / sum(config_weight),
      n_configs = n(),
      .groups = "drop"
    ) |>
    arrange(desc(overall_importance))
  
  # Save per-config results
  write_csv(combined_per_config,
            file.path(results_dir, "combined_variable_importance.csv"))
  cat("\nSaved:", file.path(results_dir, "combined_variable_importance.csv"), "\n")
  
  # Save overall results
  write_csv(combined_overall,
            file.path(results_dir, "overall_variable_importance.csv"))
  cat("Saved:", file.path(results_dir, "overall_variable_importance.csv"), "\n")
  
  # Print top 15 overall
  cat("\n--- Top 15 Variables (overall weighted importance) ---\n\n")
  print(head(combined_overall, 15))
  
  # Print top 10 per config
  cat("\n--- Top 10 Variables per Configuration ---\n\n")
  combined_per_config |>
    group_by(config_id) |>
    slice_head(n = 10) |>
    print(n = 60)
  
  invisible(list(
    per_config = combined_per_config,
    overall = combined_overall,
    weights = model_weights
  ))
}

# Run if called directly
if (sys.nframe() == 0 || !interactive()) {
  compute_combined_importance()
}
