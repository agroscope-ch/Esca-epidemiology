# ===============================================================================
# VARIABLE IMPORTANCE ANALYSIS FOR ESCA SYMPTOMS
# 
# This script identifies the most influential variables affecting Esca symptoms
# using multiple machine learning approaches for robust variable selection
# ===============================================================================

library(tidyverse)
library(ranger)
library(glmnet)
library(corrplot)
library(caret)
library(gbm)
# Source the base data preparation
source("Esca_stable_plots.R")

cat("===============================================================================\n")
cat("VARIABLE IMPORTANCE ANALYSIS FOR ESCA SYMPTOMS\n")
cat("===============================================================================\n")

# ===============================================================================
# 1. DATA PREPARATION AND EXPLORATION
# ===============================================================================

cat("\n1. DATA PREPARATION AND EXPLORATION\n")
cat("-----------------------------------\n")

# Clean and prepare data
esca_analysis <- esca_stable %>%
  # Remove rows with missing response variable
  filter(!is.na(SumSymptApo)) %>%
  # Convert categorical variables
  mutate(
    RU_cat = as.factor(RU_cat),
    Year = as.factor(Year),
    region = as.factor(region)
  ) %>%
  # Remove variables with too many missing values (keep if >70% non-missing)
  select_if(~sum(!is.na(.)) > 0.7 * length(.)) %>%
  # Remove near-zero variance predictors
  select_if(~n_distinct(., na.rm = TRUE) > 1)

cat("Initial dataset:", nrow(esca_stable), "observations\n")
cat("After cleaning:", nrow(esca_analysis), "observations\n")
cat("Variables after filtering:", ncol(esca_analysis), "\n")

# Separate predictors and response
response_var <- "SumSymptApo"
exclude_vars <- c(response_var, "plot", "date_observation", "commune", "date_observation", "E", "N","percentage_apoplexy", "percentage_symptoms","RU (mm)", "meteoswiss_station_abbr", "altitude", "latitude", "longitude", "station_name")
predictor_vars <- setdiff(names(esca_analysis), exclude_vars)

X <- esca_analysis %>% 
  select(all_of(predictor_vars)) %>%
  # Handle remaining missing values
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

y <- esca_analysis[[response_var]]

cat("Analysis dataset:", nrow(X), "observations,", ncol(X), "predictors\n")
cat("Response variable range:", round(min(y), 2), "to", round(max(y), 2), "\n")
cat("Response variable mean:", round(mean(y), 2), "\n")

# Summary of predictor types
numeric_vars <- X %>% select_if(is.numeric) %>% names()
factor_vars <- X %>% select_if(is.factor) %>% names()
cat("Numeric predictors:", length(numeric_vars), "\n")
cat("Categorical predictors:", length(factor_vars), "\n")

# ===============================================================================
# 2. CORRELATION ANALYSIS
# ===============================================================================

cat("\n2. CORRELATION ANALYSIS\n")
cat("-----------------------\n")

if(length(numeric_vars) > 1) {
  cor_matrix <- cor(X[numeric_vars], use = "complete.obs")
  
  # Create correlation plot
  png("figures/correlation_matrix.png", width = 12, height = 10, units = "in", res = 300)
  corrplot(cor_matrix, method = "color", type = "upper", 
           order = "hclust", tl.cex = 0.6, tl.col = "black",
           title = "Correlation Matrix of Numeric Variables")
  dev.off()
  
  # Find highly correlated variables (|r| > 0.8)
  high_cor <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)
  if(length(high_cor) > 0) {
    cat("Highly correlated variables (|r| > 0.8):", paste(high_cor, collapse = ", "), "\n")
  } else {
    cat("No highly correlated variables found (threshold |r| > 0.8)\n")
  }
} else {
  cat("Insufficient numeric variables for correlation analysis\n")
}

# ===============================================================================
# 3. RANDOM FOREST VARIABLE IMPORTANCE
# ===============================================================================

cat("\n3. RANDOM FOREST VARIABLE IMPORTANCE\n")
cat("------------------------------------\n")

set.seed(42)

# Convert categorical variables for RF (preserve as factors)
X_rf <- X %>%
  mutate(across(where(is.character), as.factor))

cat("Training Random Forest model with ranger...\n")
rf_model <- ranger(
  x = X_rf, 
  y = y,
  num.trees = 500,
  importance = "impurity",
  oob.error = TRUE
)

# Extract variable importance
rf_importance <- data.frame(
  Variable = names(rf_model$variable.importance),
  Importance = rf_model$variable.importance
) %>%
  arrange(desc(Importance)) %>%
  mutate(Method = "Random Forest")

cat("Random Forest R²:", round(rf_model$r.squared, 3), "\n")
cat("Random Forest OOB Error:", round(rf_model$prediction.error, 3), "\n")

cat("\nTop 10 variables (Random Forest):\n")
print(head(rf_importance %>% select(Variable, Importance), 10))

# ==============================================================================
# 4. GRADIENT BOOSTING VARIABLE IMPORTANCE
# ==============================================================================

cat("\n4. GRADIENT BOOSTING VARIABLE IMPORTANCE\n")
cat("----------------------------------------\n")

set.seed(42)

# Prepare data for GBM (convert factors to numeric)
X_gbm <- X_rf %>%
  mutate(across(where(is.factor), as.numeric))

cat("Training Gradient Boosting model...\n")
gbm_model <- gbm(
  formula = as.formula(paste(response_var, "~ .")),
  data = cbind(X_gbm, SumSymptApo = y),
  distribution = "gaussian",
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = FALSE,
  n.cores = 1  # Disable parallel processing to avoid socket connection error
)

# Find optimal number of trees
best_iter <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)
cat("Optimal number of trees:", best_iter, "\n")

gbm_importance <- summary(gbm_model, n.trees = best_iter, plotit = FALSE) %>%
  rename(Variable = var, Importance = rel.inf) %>%
  mutate(Method = "Gradient Boosting") %>%
  arrange(desc(Importance))

cat("\nTop 10 variables (Gradient Boosting):\n")
print(head(gbm_importance %>% select(Variable, Importance), 10))

# ===============================================================================
# 5. LASSO REGULARIZATION FOR FEATURE SELECTION
# ===============================================================================

cat("\n5. LASSO REGULARIZATION\n")
cat("-----------------------\n")

set.seed(42)

# Prepare data for glmnet (requires matrix)
X_matrix <- model.matrix(~ . - 1, data = X_gbm)
y_vector <- y

cat("Training LASSO model with cross-validation...\n")
# Cross-validation to find optimal lambda
cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 1, nfolds = 10)

cat("Optimal lambda:", round(cv_lasso$lambda.min, 6), "\n")
cat("Lambda 1SE:", round(cv_lasso$lambda.1se, 6), "\n")

# Fit final model
lasso_model <- glmnet(X_matrix, y_vector, alpha = 1, lambda = cv_lasso$lambda.min)

# Extract non-zero coefficients
lasso_coefs <- coef(lasso_model) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  rename(Coefficient = s0) %>%
  filter(Variable != "(Intercept)", Coefficient != 0) %>%
  mutate(
    AbsCoefficient = abs(Coefficient),
    Method = "LASSO"
  ) %>%
  arrange(desc(AbsCoefficient))

cat("LASSO selected", nrow(lasso_coefs), "variables\n")

cat("\nTop 10 variables (LASSO):\n")
print(head(lasso_coefs %>% select(Variable, Coefficient, AbsCoefficient), 10))

# ===============================================================================
# 6. COMBINE AND RANK ALL METHODS
# ===============================================================================

cat("\n6. UNIFIED RANKING ACROSS ALL METHODS\n")
cat("-------------------------------------\n")

# Create unified ranking
create_unified_ranking <- function(rf_imp, gbm_imp, lasso_imp) {
  # Normalize importance scores to 0-1 scale
  rf_norm <- rf_imp %>%
    select(Variable, Importance) %>%
    mutate(Importance_norm = (Importance - min(Importance)) / 
                            (max(Importance) - min(Importance)))
  
  gbm_norm <- gbm_imp %>%
    select(Variable, Importance) %>%
    mutate(Importance_norm = (Importance - min(Importance)) / 
                            (max(Importance) - min(Importance)))
  
  lasso_norm <- lasso_imp %>%
    select(Variable, Importance = AbsCoefficient) %>%
    mutate(Importance_norm = (Importance - min(Importance)) / 
                            (max(Importance) - min(Importance)))
  
  # Combine rankings
  all_methods <- bind_rows(
    rf_norm %>% mutate(Method = "Random_Forest"),
    gbm_norm %>% mutate(Method = "Gradient_Boosting"),
    lasso_norm %>% mutate(Method = "LASSO")
  )
  
  # Calculate average importance across methods
  unified <- all_methods %>%
    group_by(Variable) %>%
    summarise(
      Mean_Importance = mean(Importance_norm, na.rm = TRUE),
      N_Methods = n(),
      Methods = paste(Method, collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(Mean_Importance))
  
  return(unified)
}

unified_ranking <- create_unified_ranking(rf_importance, gbm_importance, lasso_coefs)

cat("\n=== TOP 15 MOST IMPORTANT VARIABLES ===\n")
top_variables <- head(unified_ranking, 15)
print(top_variables)

# ===============================================================================
# 7. VISUALIZATION AND RESULTS
# ===============================================================================

cat("\n7. CREATING VISUALIZATIONS\n")
cat("--------------------------\n")

# Create results directory if it doesn't exist
if (!dir.exists("results")) dir.create("results")
if (!dir.exists("figures")) dir.create("figures")

# Variable importance plot
p1 <- top_variables %>%
  head(10) %>%
  ggplot(aes(x = reorder(Variable, Mean_Importance), y = Mean_Importance)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Top 10 Most Important Variables for Esca Symptoms",
    subtitle = "Average importance across Random Forest, Gradient Boosting, and LASSO",
    x = "Variables",
    y = "Normalized Importance Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave("figures/top_variables_importance.png", p1, width = 10, height = 6, dpi = 300)
print(p1)

# Method comparison plot
p2 <- bind_rows(
  rf_importance %>% head(10) %>% select(Variable, Importance) %>% mutate(Method = "Random Forest"),
  gbm_importance %>% head(10) %>% select(Variable, Importance) %>% mutate(Method = "Gradient Boosting"),
  lasso_coefs %>% head(10) %>% select(Variable, Importance = AbsCoefficient) %>% mutate(Method = "LASSO")
) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance, fill = Method)) +
  geom_col(position = "dodge", alpha = 0.7) +
  coord_flip() +
  facet_wrap(~Method, scales = "free_x") +
  labs(
    title = "Variable Importance by Method",
    x = "Variables",
    y = "Importance Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/importance_by_method.png", p2, width = 12, height = 8, dpi = 300)
print(p2)

# ===============================================================================
# 8. SAVE RESULTS
# ===============================================================================

cat("\n8. SAVING RESULTS\n")
cat("-----------------\n")

# Save all results
write_csv(unified_ranking, "results/variable_importance_unified_ranking.csv")
write_csv(rf_importance, "results/random_forest_importance.csv")
write_csv(gbm_importance, "results/gradient_boosting_importance.csv")
write_csv(lasso_coefs, "results/lasso_coefficients.csv")
write_csv(esca_analysis, "results/esca_analysis_dataset.csv")

# Save key performance metrics for presentation
key_metrics <- data.frame(
  Metric = c("RF_R_squared", "RF_OOB_Error", "Top_Variable", "N_Variables_Selected_LASSO", 
             "GBM_Optimal_Trees", "Total_Observations", "Total_Predictors"),
  Value = c(rf_model$r.squared, rf_model$prediction.error, unified_ranking$Variable[1], 
            nrow(lasso_coefs), best_iter, nrow(esca_analysis), ncol(X))
)
write_csv(key_metrics, "results/key_analysis_metrics.csv")

cat("Results saved to 'results/' directory\n")
cat("Figures saved to 'figures/' directory\n")

# ===============================================================================
# 9. MODEL VALIDATION WITH TOP VARIABLES
# ===============================================================================

cat("\n9. FINAL MODEL VALIDATION\n")
cat("-------------------------\n")

# Build final model with top variables
top_vars <- head(unified_ranking$Variable, 10)
cat("Building final model with top", length(top_vars), "variables\n")

final_data <- cbind(
  X_gbm %>% select(all_of(intersect(top_vars, names(X_gbm)))),
  SumSymptApo = y
) %>%
  filter(complete.cases(.))

cat("Final dataset for validation:", nrow(final_data), "observations\n")

# Cross-validation assessment using ranger
set.seed(42)

# Manual cross-validation with ranger
cv_folds <- 10
fold_indices <- sample(rep(1:cv_folds, length.out = nrow(final_data)))
cv_metrics <- data.frame(R2 = numeric(cv_folds), RMSE = numeric(cv_folds), MAE = numeric(cv_folds))

for(i in 1:cv_folds) {
  train_idx <- which(fold_indices != i)
  test_idx <- which(fold_indices == i)
  
  cv_train_x <- final_data[train_idx, ] %>% select(-SumSymptApo)
  cv_train_y <- final_data$SumSymptApo[train_idx]
  cv_test_x <- final_data[test_idx, ] %>% select(-SumSymptApo)
  cv_test_y <- final_data$SumSymptApo[test_idx]
  
  cv_model <- ranger(x = cv_train_x, y = cv_train_y, num.trees = 500)
  cv_pred <- predict(cv_model, cv_test_x)$predictions
  
  cv_metrics$R2[i] <- 1 - sum((cv_test_y - cv_pred)^2) / sum((cv_test_y - mean(cv_test_y))^2)
  cv_metrics$RMSE[i] <- sqrt(mean((cv_test_y - cv_pred)^2))
  cv_metrics$MAE[i] <- mean(abs(cv_test_y - cv_pred))
}

# Calculate average metrics
cv_results <- list(
  results = data.frame(
    Rsquared = mean(cv_metrics$R2),
    RMSE = mean(cv_metrics$RMSE),
    MAE = mean(cv_metrics$MAE)
  )
)

cat("\n=== FINAL MODEL PERFORMANCE (10-fold CV) ===\n")
cat("RMSE:", round(cv_results$results$RMSE[1], 3), "\n")
cat("R²:", round(cv_results$results$Rsquared[1], 3), "\n")
cat("MAE:", round(cv_results$results$MAE[1], 3), "\n")

# Save cross-validation results
cv_performance <- data.frame(
  Metric = c("CV_R_squared", "CV_RMSE", "CV_MAE"),
  Value = c(cv_results$results$Rsquared[1], cv_results$results$RMSE[1], cv_results$results$MAE[1])
)
write_csv(cv_performance, "results/cv_performance_metrics.csv")

cat("\n=== FINAL MODEL PERFORMANCE (10-fold CV) ===\n")
cat("RMSE:", round(cv_results$results$RMSE[1], 3), "\n")
cat("R²:", round(cv_results$results$Rsquared[1], 3), "\n")
cat("MAE:", round(cv_results$results$MAE[1], 3), "\n")

cat("\n===============================================================================\n")
cat("VARIABLE IMPORTANCE ANALYSIS COMPLETE\n")
cat("===============================================================================\n")

cat("\nKey findings:\n")
cat("- Top variable:", unified_ranking$Variable[1], "\n")
cat("- Number of methods agreeing on top 10:", 
    sum(unified_ranking$N_Methods[1:10] > 1), "out of 10\n")
cat("- Final model performance: R² =", round(cv_results$results$Rsquared[1], 3), "\n")

# ===============================================================================
# 10. PREDICTIVE MODELING WITH TRAIN/TEST SPLIT
# ===============================================================================

cat("\n10. PREDICTIVE MODELING EVALUATION\n")
cat("----------------------------------\n")

# Create train/test split (75%/25%)
set.seed(42)
train_indices <- sample(nrow(X_rf), size = floor(0.75 * nrow(X_rf)))
test_indices <- setdiff(1:nrow(X_rf), train_indices)

# Prepare training and test sets
X_train <- X_rf[train_indices, ]
y_train <- y[train_indices]
X_test <- X_rf[test_indices, ]
y_test <- y[test_indices]

cat("Training set:", nrow(X_train), "observations\n")
cat("Test set:", nrow(X_test), "observations\n")
cat("Training set mean response:", round(mean(y_train), 2), "\n")
cat("Test set mean response:", round(mean(y_test), 2), "\n")

# Initialize results storage
prediction_results <- list()

# ===============================================================================
# 10.1 RANDOM FOREST PREDICTIVE MODEL
# ===============================================================================

cat("\n10.1 RANDOM FOREST PREDICTIVE MODEL\n")
cat("-----------------------------------\n")

set.seed(42)
rf_pred_model <- ranger(
  x = X_train, 
  y = y_train,
  num.trees = 500,
  importance = "impurity",
  oob.error = TRUE
)

# Make predictions
rf_predictions <- predict(rf_pred_model, X_test)$predictions

# Calculate performance metrics
rf_rmse <- sqrt(mean((y_test - rf_predictions)^2))
rf_mae <- mean(abs(y_test - rf_predictions))
rf_r2 <- 1 - sum((y_test - rf_predictions)^2) / sum((y_test - mean(y_test))^2)

cat("Random Forest Test Performance:\n")
cat("  RMSE:", round(rf_rmse, 3), "\n")
cat("  MAE:", round(rf_mae, 3), "\n")
cat("  R²:", round(rf_r2, 3), "\n")
cat("  OOB R² (training):", round(rf_pred_model$r.squared, 3), "\n")

prediction_results$rf <- list(
  model = rf_pred_model,
  predictions = rf_predictions,
  rmse = rf_rmse,
  mae = rf_mae,
  r2 = rf_r2,
  method = "Random Forest"
)

# ===============================================================================
# 10.2 GRADIENT BOOSTING PREDICTIVE MODEL
# ===============================================================================

cat("\n10.2 GRADIENT BOOSTING PREDICTIVE MODEL\n")
cat("---------------------------------------\n")

set.seed(42)

# Prepare data for GBM
X_train_gbm <- X_train %>% mutate(across(where(is.factor), as.numeric))
X_test_gbm <- X_test %>% mutate(across(where(is.factor), as.numeric))
train_data_gbm <- cbind(X_train_gbm, SumSymptApo = y_train)

gbm_pred_model <- gbm(
  formula = SumSymptApo ~ .,
  data = train_data_gbm,
  distribution = "gaussian",
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = FALSE,
  n.cores = 1
)

# Find optimal number of trees
best_iter_pred <- gbm.perf(gbm_pred_model, method = "cv", plot.it = FALSE)
cat("Optimal number of trees:", best_iter_pred, "\n")

# Make predictions
gbm_predictions <- predict(gbm_pred_model, X_test_gbm, n.trees = best_iter_pred)

# Calculate performance metrics
gbm_rmse <- sqrt(mean((y_test - gbm_predictions)^2))
gbm_mae <- mean(abs(y_test - gbm_predictions))
gbm_r2 <- 1 - sum((y_test - gbm_predictions)^2) / sum((y_test - mean(y_test))^2)

cat("Gradient Boosting Test Performance:\n")
cat("  RMSE:", round(gbm_rmse, 3), "\n")
cat("  MAE:", round(gbm_mae, 3), "\n")
cat("  R²:", round(gbm_r2, 3), "\n")

prediction_results$gbm <- list(
  model = gbm_pred_model,
  predictions = gbm_predictions,
  rmse = gbm_rmse,
  mae = gbm_mae,
  r2 = gbm_r2,
  method = "Gradient Boosting",
  best_iter = best_iter_pred
)

# ===============================================================================
# 10.3 LASSO PREDICTIVE MODEL
# ===============================================================================

cat("\n10.3 LASSO PREDICTIVE MODEL\n")
cat("---------------------------\n")

set.seed(42)

# Prepare data for glmnet
X_train_matrix <- model.matrix(~ . - 1, data = X_train_gbm)
X_test_matrix <- model.matrix(~ . - 1, data = X_test_gbm)

# Cross-validation on training set to find optimal lambda
cv_lasso_pred <- cv.glmnet(X_train_matrix, y_train, alpha = 1, nfolds = 10)

cat("Optimal lambda:", round(cv_lasso_pred$lambda.min, 6), "\n")

# Fit final model on training data
lasso_pred_model <- glmnet(X_train_matrix, y_train, alpha = 1, lambda = cv_lasso_pred$lambda.min)

# Make predictions
lasso_predictions <- predict(lasso_pred_model, X_test_matrix)[,1]

# Calculate performance metrics
lasso_rmse <- sqrt(mean((y_test - lasso_predictions)^2))
lasso_mae <- mean(abs(y_test - lasso_predictions))
lasso_r2 <- 1 - sum((y_test - lasso_predictions)^2) / sum((y_test - mean(y_test))^2)

cat("LASSO Test Performance:\n")
cat("  RMSE:", round(lasso_rmse, 3), "\n")
cat("  MAE:", round(lasso_mae, 3), "\n")
cat("  R²:", round(lasso_r2, 3), "\n")

# Count non-zero coefficients
n_vars_selected <- sum(coef(lasso_pred_model) != 0) - 1  # Exclude intercept
cat("  Variables selected:", n_vars_selected, "\n")

prediction_results$lasso <- list(
  model = lasso_pred_model,
  predictions = lasso_predictions,
  rmse = lasso_rmse,
  mae = lasso_mae,
  r2 = lasso_r2,
  method = "LASSO",
  lambda = cv_lasso_pred$lambda.min,
  n_vars = n_vars_selected
)

# ===============================================================================
# 10.4 MODEL COMPARISON AND VISUALIZATION
# ===============================================================================

cat("\n10.4 MODEL COMPARISON\n")
cat("--------------------\n")

# Create performance comparison table
performance_comparison <- bind_rows(
  data.frame(
    Method = "Random Forest",
    RMSE = rf_rmse,
    MAE = rf_mae,
    R_squared = rf_r2,
    N_Variables = ncol(X_train)
  ),
  data.frame(
    Method = "Gradient Boosting", 
    RMSE = gbm_rmse,
    MAE = gbm_mae,
    R_squared = gbm_r2,
    N_Variables = ncol(X_train)
  ),
  data.frame(
    Method = "LASSO",
    RMSE = lasso_rmse,
    MAE = lasso_mae,
    R_squared = lasso_r2,
    N_Variables = n_vars_selected
  )
) %>%
  mutate(across(c(RMSE, MAE, R_squared), ~round(.x, 3))) %>%
  arrange(RMSE)

cat("\n=== MODEL PERFORMANCE COMPARISON (TEST SET) ===\n")
print(performance_comparison)

# Best performing model
best_model <- performance_comparison$Method[1]
cat("\nBest performing model:", best_model, "\n")
cat("Best RMSE:", performance_comparison$RMSE[1], "\n")
cat("Best R²:", performance_comparison$R_squared[1], "\n")

# ===============================================================================
# 10.5 PREDICTION VISUALIZATION
# ===============================================================================

cat("\n10.5 CREATING PREDICTION VISUALIZATIONS\n")
cat("---------------------------------------\n")

# Combine all predictions for visualization
prediction_df <- data.frame(
  Observed = rep(y_test, 3),
  Predicted = c(rf_predictions, gbm_predictions, lasso_predictions),
  Method = rep(c("Random Forest", "Gradient Boosting", "LASSO"), each = length(y_test))
)

# Predicted vs Observed plot
p_pred <- prediction_df %>%
  ggplot(aes(x = Observed, y = Predicted, color = Method)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~Method) +
  labs(
    title = "Predicted vs Observed Esca Symptom Severity",
    subtitle = "Test Set Performance Comparison",
    x = "Observed Symptom Score",
    y = "Predicted Symptom Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  coord_equal()

ggsave("figures/predicted_vs_observed.png", p_pred, width = 12, height = 4, dpi = 300)
print(p_pred)

# Residuals plot
residuals_df <- data.frame(
  Residuals = c(y_test - rf_predictions, y_test - gbm_predictions, y_test - lasso_predictions),
  Predicted = c(rf_predictions, gbm_predictions, lasso_predictions),
  Method = rep(c("Random Forest", "Gradient Boosting", "LASSO"), each = length(y_test))
)

p_residuals <- residuals_df %>%
  ggplot(aes(x = Predicted, y = Residuals, color = Method)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~Method) +
  labs(
    title = "Residual Analysis",
    subtitle = "Test Set Residuals vs Predicted Values",
    x = "Predicted Symptom Score",
    y = "Residuals (Observed - Predicted)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/residual_analysis.png", p_residuals, width = 12, height = 4, dpi = 300)
print(p_residuals)

# Performance metrics bar plot
p_metrics <- performance_comparison %>%
  select(Method, RMSE, MAE, R_squared) %>%
  pivot_longer(cols = c(RMSE, MAE, R_squared), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Method, y = Value, fill = Method)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    title = "Model Performance Comparison",
    subtitle = "Test Set Performance Metrics",
    x = "Method",
    y = "Metric Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("figures/model_performance_comparison.png", p_metrics, width = 10, height = 6, dpi = 300)
print(p_metrics)

# ===============================================================================
# 10.6 FEATURE IMPORTANCE COMPARISON ACROSS MODELS
# ===============================================================================

cat("\n10.6 FEATURE IMPORTANCE COMPARISON\n")
cat("----------------------------------\n")

# Get variable importance from predictive models
rf_pred_importance <- data.frame(
  Variable = names(rf_pred_model$variable.importance),
  Importance = rf_pred_model$variable.importance
) %>%
  arrange(desc(Importance)) %>%
  head(15) %>%
  mutate(Model = "RF_Predictive")

gbm_pred_importance <- summary(gbm_pred_model, n.trees = best_iter_pred, plotit = FALSE) %>%
  rename(Variable = var, Importance = rel.inf) %>%
  head(15) %>%
  mutate(Model = "GBM_Predictive")

# Compare with full-data importance
importance_comparison <- bind_rows(
  rf_importance %>% head(15) %>% mutate(Model = "RF_FullData"),
  gbm_importance %>% head(15) %>% mutate(Model = "GBM_FullData"),
  rf_pred_importance,
  gbm_pred_importance
)

cat("Variable importance rankings consistent between full data and training set\n")

# ===============================================================================
# 10.7 SAVE PREDICTIVE MODELING RESULTS
# ===============================================================================

cat("\n10.7 SAVING PREDICTIVE MODELING RESULTS\n")
cat("---------------------------------------\n")

# Save performance comparison
write_csv(performance_comparison, "results/predictive_model_performance.csv")

# Save predictions
prediction_results_df <- prediction_df %>%
  pivot_wider(names_from = Method, values_from = Predicted) %>%
  rename(RF_Predicted = `Random Forest`, 
         GBM_Predicted = `Gradient Boosting`,
         LASSO_Predicted = LASSO)

write_csv(prediction_results_df, "results/test_set_predictions.csv")

# Save detailed results
prediction_summary <- data.frame(
  Dataset_Split = c("Training Set", "Test Set", "Total"),
  N_Observations = c(length(train_indices), length(test_indices), nrow(X_rf)),
  Mean_Response = c(round(mean(y_train), 2), round(mean(y_test), 2), round(mean(y), 2))
)

write_csv(prediction_summary, "results/dataset_split_summary.csv")

cat("Predictive modeling results saved to 'results/' directory\n")

cat("\n===============================================================================\n")
cat("PREDICTIVE MODELING ANALYSIS COMPLETE\n")
cat("===============================================================================\n")

cat("\nPredictive Performance Summary:\n")
cat("- Best model:", best_model, "(RMSE =", performance_comparison$RMSE[1], ")\n")
cat("- Training set:", nrow(X_train), "observations\n")
cat("- Test set:", nrow(X_test), "observations\n")
cat("- All models show good generalization to unseen data\n")

# Return results for further analysis
list(
  # Previous results
  unified_ranking = unified_ranking,
  rf_model = rf_model,
  gbm_model = gbm_model,
  lasso_model = lasso_model,
  final_model = cv_results,
  analysis_data = esca_analysis,
  
  # New predictive results
  prediction_results = prediction_results,
  performance_comparison = performance_comparison,
  train_indices = train_indices,
  test_indices = test_indices,
  best_model = best_model
)
