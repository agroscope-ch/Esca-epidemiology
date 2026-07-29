# ===============================================================================
# VARIABLE IMPORTANCE ANALYSIS FOR ESCA SYMPTOMS - TIDYMODELS IMPLEMENTATION
# 
# This script identifies the most influential variables affecting Esca symptoms
# using a modern tidymodels pipeline with hyperparameter tuning and robust
# cross-validation for reproducible variable selection
# ===============================================================================

# Load the new tidymodels pipeline
source("esca_ml_pipeline.R")

cat("===============================================================================\n")
cat("VARIABLE IMPORTANCE ANALYSIS FOR ESCA SYMPTOMS - TIDYMODELS PIPELINE\n")
cat("===============================================================================\n")

# ===============================================================================
# 1. RUN MAIN TIDYMODELS PIPELINE
# ===============================================================================

cat("\n1. RUNNING TIDYMODELS PIPELINE\n")
cat("==============================\n")

# Execute the complete pipeline with default configuration
results <- run_esca_ml_pipeline(
  data_subset = NULL,  # Use full dataset
  config = ESCA_CONFIG,
  save_results = TRUE
)

cat("\nPipeline execution completed successfully!\n")

# ===============================================================================
# 2. DETAILED RESULTS ANALYSIS AND VISUALIZATION
# ===============================================================================

cat("\n2. DETAILED RESULTS ANALYSIS\n")
cat("============================\n")

# Extract key results for detailed analysis
cv_performance <- results$performance$cv
test_performance <- results$performance$test
variable_importance <- results$importance
predictions <- results$predictions

# Model comparison summary
cat("\nCross-Validation Performance Summary:\n")
cv_summary <- cv_performance |>
  filter(.metric == "rmse") |>
  arrange(mean) |>
  mutate(
    model = factor(model, levels = model),
    rank = row_number()
  )
print(cv_summary)

cat("\nTest Set Performance Summary:\n")
test_summary <- test_performance |>
  filter(.metric == "rmse") |>
  arrange(.estimate) |>
  mutate(
    model = factor(model, levels = model),
    rank = row_number()
  )
print(test_summary)

# Identify best performing model
best_model <- test_summary$model[1]
cat("\nBest performing model:", as.character(best_model), "\n")

# ===============================================================================
# 3. VARIABLE IMPORTANCE ANALYSIS AND RANKING
# ===============================================================================

cat("\n3. VARIABLE IMPORTANCE ANALYSIS\n")
cat("===============================\n")

# Combine importance from all models
if (length(variable_importance) > 0) {
  combined_importance <- bind_rows(variable_importance) |>
    group_by(Variable) |>
    summarise(
      Mean_Importance = mean(Importance, na.rm = TRUE),
      N_Models = n(),
      Models = paste(unique(model), collapse = ", "),
      .groups = "drop"
    ) |>
    arrange(desc(Mean_Importance))
  
  cat("\nTop 15 Most Important Variables (Unified Ranking):\n")
  top_variables <- head(combined_importance, 15)
  print(top_variables)
  
  # Save unified ranking
  write_csv(combined_importance, "results/variable_importance_unified_ranking_tidymodels.csv")
  
} else {
  cat("Variable importance could not be extracted from models.\n")
  top_variables <- tibble()
}

# ===============================================================================
# 4. CREATE ENHANCED VISUALIZATIONS
# ===============================================================================

cat("\n4. CREATING VISUALIZATIONS\n")
cat("==========================\n")

library(ggplot2)
library(patchwork)

# Performance comparison plot
p_performance <- bind_rows(
  cv_performance |> filter(.metric == "rmse") |> mutate(type = "Cross-Validation"),
  test_performance |> filter(.metric == "rmse") |> mutate(type = "Test Set", mean = .estimate, std_err = 0)
) |>
  ggplot(aes(x = reorder(model, mean), y = mean, fill = type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), 
                position = position_dodge(width = 0.9), width = 0.2) +
  coord_flip() +
  labs(
    title = "Model Performance Comparison",
    subtitle = "RMSE: Cross-Validation vs Test Set Performance",
    x = "Model",
    y = "RMSE",
    fill = "Evaluation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

ggsave("figures/tidymodels_performance_comparison.png", p_performance, 
       width = 10, height = 6, dpi = 300)

cat("- Performance comparison plot saved\n")

# Variable importance plot (if available)
if (nrow(top_variables) > 0) {
  p_importance <- top_variables |>
    head(10) |>
    ggplot(aes(x = reorder(Variable, Mean_Importance), y = Mean_Importance)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Top 10 Most Important Variables for Esca Symptoms",
      subtitle = "Average importance across Random Forest, Gradient Boosting, and LASSO",
      x = "Variables",
      y = "Mean Importance Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )
  
  ggsave("figures/tidymodels_variable_importance.png", p_importance, 
         width = 10, height = 6, dpi = 300)
  
  cat("- Variable importance plot saved\n")
}

# Predicted vs Observed plot
p_predictions <- predictions |>
  ggplot(aes(x = !!sym(results$data$prepared$response), y = .pred, color = model)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~model, scales = "free") +
  labs(
    title = "Predicted vs Observed Esca Symptom Severity",
    subtitle = "Test Set Predictions from Tuned Models",
    x = "Observed Symptom Score",
    y = "Predicted Symptom Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

ggsave("figures/tidymodels_predictions.png", p_predictions, 
       width = 12, height = 4, dpi = 300)

cat("- Predictions plot saved\n")

# ===============================================================================
# 5. COMPARISON WITH PREVIOUS APPROACH (OPTIONAL)
# ===============================================================================

cat("\n5. PIPELINE COMPARISON AND SUMMARY\n")
cat("==================================\n")

# Print summary of improvements
cat("Tidymodels Pipeline Improvements:\n")
cat("- Hyperparameter tuning with cross-validation\n")
cat("- Reproducible preprocessing with recipes\n")
cat("- Standardized model interface with parsnip\n")
cat("- Robust data splitting with stratification\n")
cat("- Parallel processing for efficiency\n")
cat("- Comprehensive performance evaluation\n")

# Performance summary
if (exists("best_model")) {
  best_performance <- test_performance |>
    filter(model == best_model, .metric %in% c("rmse", "rsq", "mae"))
  
  cat("\nBest Model Performance Summary:\n")
  cat("Model:", as.character(best_model), "\n")
  for (i in 1:nrow(best_performance)) {
    metric <- best_performance$.metric[i]
    value <- best_performance$.estimate[i]
    cat(toupper(metric), ":", round(value, 4), "\n")
  }
}

# Data summary
data_summary <- results$data$prepared$summary
cat("\nDataset Summary:\n")
cat("- Total observations:", data_summary$n_observations, "\n")
cat("- Total predictors:", data_summary$n_predictors, "\n")
cat("- Response variable range:", paste(round(data_summary$response_range, 2), collapse = " to "), "\n")
cat("- Training set:", nrow(results$data$splits$train_data), "observations\n")
cat("- Test set:", nrow(results$data$splits$test_data), "observations\n")

# ===============================================================================
# 6. EXPORT RESULTS FOR FURTHER ANALYSIS
# ===============================================================================

cat("\n6. SAVING COMPREHENSIVE RESULTS\n")
cat("===============================\n")

# Save a comprehensive summary
analysis_summary <- list(
  timestamp = Sys.time(),
  config_used = ESCA_CONFIG,
  data_summary = data_summary,
  model_performance = list(
    cv = cv_performance,
    test = test_performance
  ),
  best_model = if(exists("best_model")) as.character(best_model) else NA,
  top_variables = if(nrow(top_variables) > 0) top_variables else NA,
  total_models_trained = length(results$models$finalized)
)

saveRDS(analysis_summary, "results/esca_analysis_summary_tidymodels.rds")

cat("Analysis summary saved to: results/esca_analysis_summary_tidymodels.rds\n")

# Save session info for reproducibility
sessionInfo() |> saveRDS("results/session_info_tidymodels.rds")

cat("\nTidymodels analysis complete! All results saved to 'results/' and 'figures/' directories.\n")

cat("\n===============================================================================\n")
cat("VARIABLE IMPORTANCE ANALYSIS COMPLETE - TIDYMODELS IMPLEMENTATION\n")
cat("===============================================================================\n")

# Return the results for interactive use
results
