### Data set


variables:

- [VPD](https://vpdrought.wsl.ch/fr/parametres/) can be computed as shown in this webpage
- Évapotranspiration (can be computed using Penman-Monteith formula)
- précipitations
- températures
- Soil Wettness Index (SWI)
- Vapor Pressure Deficit (to be more clearly defined)

# Esca Disease Epidemiological data and analyis

This project presents epidemiological data gathered in Westerne Switzerland from 2001 to 2022. The analysis combines disease surveillance data with meteorological and soil data to identify the most important environmental predictors of disease severity.

## Analysis Pipeline Architecture

### 1. Raw Data Sources

#### Original Raw Data (External)
- **`~/mnt/Data-Work-CH/.../ESCA brut 2001-2022.xlsx`**: Main raw Esca disease surveillance data
  - Contains disease observations, plot locations, varieties, training systems
  - Two sheets: main data and stable plots information
- **`data/Esca et RU des sols Vivian.xlsx`**: Soil water capacity (RU) data for vineyard plots (from Vivian Zufferey)

#### Downloaded Meteorological Data (APIs)
- **MeteoSwiss API**: Historical weather station data (1995-2024)
- **Vitimeteo API**: Data from the Agrometeo network weather stations

### 2. Data Processing Scripts (Dependencies Chain)

#### Step 1: Data Preprocessing
- **[Esca_preprocess.R](Esca_preprocess.R)**: 
  - Reads raw ESCA surveillance data
  - Cleans and standardizes variable names
  - Produces: `data/Esca_CH_2001-2022.csv`, `data/Esca_CH_2001-2022_stable_plots.csv`

#### Step 2: Weather Data Collection
- **[meteoswiss_historical_data.R](meteoswiss_historical_data.R)**:
  - Downloads MeteoSwiss historical data via API
  - Depends on : [match_API_names.R](match_API_names.R)
  - Produces: `data/meteoswiss_weather_data_1995_2024.csv`

- **[vitimeteo_infos.R](vitimeteo_infos.R)**:
  - Collects Vitimeteo station metadata
  - Produces: `data/vitimeteo_stations_info.csv`

#### Step 3: Spatial Associations
- **[plot_Vitimeteo_associations.R](plot_Vitimeteo_associations.R)**:
  - Associates vineyard plots with nearest Vitimeteo stations
  - Produces: `data/plot_station_associations.csv`

- **[plot_MeteoSwiss_associations.R](plot_MeteoSwiss_associations.R)**:
  - Associates vineyard plots with nearest MeteoSwiss stations  
  - Sources: [plot_Vitimeteo_associations.R](plot_Vitimeteo_associations.R)
  - Produces: `data/plot_meteoswiss_associations.csv`

#### Step 4: Final Dataset Preparation
- **[Esca_stable_plots.R](Esca_stable_plots.R)**:
  - Merges all datasets (disease + weather + soil + associations)
  - Filters to stable plots with Chasselas variety (2004-2022)
  - Creates final analysis dataset: `esca_stable` object

### 3. Main Analysis Scripts

#### Step 5: Variable Importance Analysis
- **[variable_importance_analysis.R](variable_importance_analysis.R)**:
  - **Sources**: [Esca_stable_plots.R](Esca_stable_plots.R)
  - **Methods**: Random Forest, Gradient Boosting, LASSO regularization
  - **Outputs**:
    - `results/variable_importance_unified_ranking.csv`
    - `results/random_forest_importance.csv`
    - `results/gradient_boosting_importance.csv`
    - `results/lasso_coefficients.csv`
    - `results/esca_analysis_dataset.csv`
    - `results/key_analysis_metrics.csv`
    - `results/cv_performance_metrics.csv`
    - `results/predictive_model_performance.csv`
    - `results/test_set_predictions.csv`
    - `results/dataset_split_summary.csv`
  - **Figures**:
    - `figures/correlation_matrix.png`
    - `figures/top_variables_importance.png`
    - `figures/importance_by_method.png`
    - `figures/predicted_vs_observed.png`
    - `figures/residual_analysis.png`
    - `figures/model_performance_comparison.png`

#### Step 6: Results Presentation
- **[variable_importance_presentation.Rmd](variable_importance_presentation.Rmd)**:
  - **Dependencies**: All CSV files in `results/` directory
  - Generates HTML presentation with slides
  - **Output**: `variable_importance_presentation.html`

### 4. Directory Structure

```
├── data/                               # Processed datasets
│   ├── Esca_CH_2001-2022.csv         # Main processed Esca data
│   ├── Esca_CH_2001-2022_stable_plots.csv
│   ├── meteoswiss_weather_data_1995_2024.csv
│   ├── plot_meteoswiss_associations.csv
│   ├── plot_station_associations.csv
│   ├── vitimeteo_stations_info.csv
│   └── Esca et RU des sols Vivian.xlsx # Soil data (original)
├── results/                           # Analysis outputs
│   ├── variable_importance_unified_ranking.csv
│   ├── [8 other CSV result files]
├── figures/                           # Generated plots and visualizations
│   ├── correlation_matrix.png
│   ├── top_variables_importance.png
│   └── [5 other PNG files]
└── archive_scripts/                   # Development and testing scripts
```

### 5. Key Variables Analyzed

#### Environmental Predictors
- **Temperature**: Monthly means, extremes (March-October)
- **Precipitation**: Monthly totals, distribution
- **Humidity**: Relative humidity patterns
- **Atmospheric Pressure**: Seasonal variations
- **Solar Radiation**: Growing season radiation
- **Evapotranspiration**: Water demand estimates

#### Soil Variables
- **RU categories**: Soil water retention capacity categories (0-90, 90-130, 130-160, 160-200mm)

#### Temporal/Spatial Variables  
- **Year**: Temporal trends (2004-2022)
- **Region**: Swiss wine regions

#### Response Variable
- **SumSymptApo**: Combined Esca symptom severity (% apoplexy + % symptoms)

### 6. Analysis Methods

1. **Random Forest** (ranger package): Non-linear interaction detection
2. **Gradient Boosting** (gbm package): Sequential improvement modeling  
3. **LASSO Regularization** (glmnet package): Sparse feature selection

### 7. Model Performance

Model performances are very low in general, not better that sampling from the empirical distribution of the observed data, so any interpretation must be done with great caution.  

- **Cross-validation**: 10-fold CV for robust performance estimation
- **Train/Test Split**: 75%/25% for out-of-sample validation
- **Best Model**: LASSO (highest R², lowest RMSE)
- **Key Finding**: Environmental variables dominate over temporal/spatial factors

## Usage

1. **Run full pipeline**: Execute scripts in dependency order
2. **Analysis only**: Run `variable_importance_analysis.R` (requires processed data)
3. **Presentation**: Knit `variable_importance_presentation.Rmd`

## Dependencies

- **R packages**: tidyverse, ranger, gbm, glmnet, readxl, sf, httr, knitr
- **External data**: Access to raw ESCA surveillance database
- **APIs**: MeteoSwiss and Vitimeteo weather services
