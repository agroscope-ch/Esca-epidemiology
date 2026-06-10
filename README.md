# Esca Disease Epidemiological Data and Analysis

This project presents epidemiological data gathered in Western Switzerland from 2001 to 2022. The analysis combines disease surveillance data with meteorological and soil data to identify the most important environmental predictors of Esca disease severity.

## Repository Structure

```
├── data/                              # All data files (original + derived)
├── results/                           # ML pipeline outputs per configuration
├── figures/                           # Generated plots and figures
├── plots/                             # Additional plot outputs
└── archive_scripts/                   # Old/exploratory/test scripts
```

### Scripts by Role

| Category | Script | Description |
|---|---|---|
| **Data pipeline** | `Esca_preprocess.R` | [1] Preprocess raw disease data → CSV |
| | `vitimeteo_infos.R` | [2a] Download Vitimeteo station metadata |
| | `plot_Vitimeteo_associations.R` | [3a] Spatial matching: plots ↔ Vitimeteo stations |
| | `API_Vitimeteo_download.R` | [4a] Download hourly weather from Vitimeteo |
| | `efficient_vitimeteo_download.R` | [4a] Download monthly weather from Vitimeteo |
| | `MaxDailyVPD.R` | [5a] Compute VPD from hourly Vitimeteo data |
| | `meteoswiss_historical_data.R` | [2b] Download MeteoSwiss monthly weather |
| | `plot_MeteoSwiss_associations.R` | [3b] Spatial matching: plots ↔ MeteoSwiss stations |
| | `compute_meteoswiss_vpd.R` | [5b] Compute VPD from hourly MeteoSwiss data |
| **ML pipeline** | `esca_ml_config.R` | [6] Configuration definitions (4 scenarios) |
| | `esca_ml_pipeline.R` | [7] Core ML pipeline (tidymodels, 3 models) |
| | `run_all_configurations.R` | [8] Run pipeline across all scenarios |
| **Utilities** | `meteoswiss_download_funs.R` | Reusable MeteoSwiss OGD download functions |
| | `match_API_names.R` | Plot name → Vitimeteo station ID mapping |
| **Post-hoc analysis** | `combined_variable_importance.R` | Combined variable importance across configs |
| | `variable_importance_analysis.R` | Standalone importance ranking analysis |
| | `run_variable_importance_scenarios.R` | Importance analysis across scenarios |
| | `export_figures.R` | Generate publication-ready figures |
| **Reports** | `Esca_Epidemiology.Rmd` | Main analysis report (HTML output) |
| | `results_presentation.Rmd` | Results presentation slides |
| | `variable_importance_presentation.Rmd` | Variable importance slides |

## Original Data Sources

Only two datasets are truly original — everything else is either downloaded from APIs or derived by scripts:

| File | Description | Origin |
|---|---|---|
| **`ESCA brut 2001-2022.xlsx`** | Raw Esca disease surveillance data (plot locations, varieties, symptoms, apoplexy) | External (network mount, not in repo) |
| **`data/Esca et RU des sols Vivian.xlsx`** | Soil water reserve capacity (RU) per vineyard plot | Provided by Vivian Zufferey |

Two external APIs provide meteorological data:

- **Vitimeteo API** (`vitimeteo.ch/api/`): Agrometeo network — ~41 stations, closer to plots but with data gaps (no data before 2003)
- **MeteoSwiss OGD** (`data.geo.admin.ch`): Federal weather stations — 8 stations in the study area, complete coverage 2001–2024

## Script Dependency Tree

The pipeline has a clear execution order. Each script depends on data produced by earlier steps.

```
ORIGINAL DATA
│
├── ESCA brut 2001-2022.xlsx ──→ [1] Esca_preprocess.R
│                                     ├──→ data/Esca_CH_2001-2022.csv ★
│                                     └──→ data/Esca_CH_2001-2022_stable_plots.csv
│
├── Esca et RU des sols Vivian.xlsx ★ (read directly by the ML pipeline)
│
│  VITIMETEO API
│  ├──→ [2a] vitimeteo_infos.R
│  │         └──→ data/vitimeteo_stations_info.csv
│  │
│  ├──→ [3a] plot_Vitimeteo_associations.R
│  │         reads: data/Esca_CH_2001-2022.csv, data/vitimeteo_stations_info.csv
│  │         └──→ data/plot_station_associations.csv ★
│  │
│  ├──→ [4a] API_Vitimeteo_download.R (or efficient_vitimeteo_download.R)
│  │         reads: data/plot_station_associations.csv
│  │         ├──→ data/weather_data_Agrometeo_Esca_plots.csv (hourly)
│  │         └──→ data/weather_data_Agrometeo_Esca_plots_monthly.csv ★ (monthly)
│  │
│  └──→ [5a] MaxDailyVPD.R
│             reads: data/weather_data_Agrometeo_Esca_plots.csv (hourly),
│                    data/plot_station_associations.csv
│             ├──→ data/VPD_data_Agrometeo.csv
│             └──→ data/VPD_data_Agrometeo_monthly.csv ★
│
│  METEOSWISS OGD
│  ├──→ [2b] meteoswiss_historical_data.R
│  │         sources: match_API_names.R, meteoswiss_download_funs.R
│  │         └──→ data/meteoswiss_weather_data_1995_2024.csv ★
│  │
│  ├──→ [3b] plot_MeteoSwiss_associations.R
│  │         sources: plot_Vitimeteo_associations.R (for coord conversion)
│  │         reads: data/Esca_CH_2001-2022.csv, data/meteoswiss_weather_data_1995_2024.csv
│  │         └──→ data/plot_meteoswiss_associations.csv ★
│  │
│  └──→ [5b] compute_meteoswiss_vpd.R
│             sources: meteoswiss_download_funs.R
│             reads: data/plot_meteoswiss_associations.csv
│             downloads: hourly T + RH from MeteoSwiss OGD
│             └──→ data/VPD_data_MeteoSwiss_monthly.csv ★
│
│  ML PIPELINE (all ★ files above feed into this)
│  ├──→ [6] esca_ml_config.R (configuration definitions)
│  ├──→ [7] esca_ml_pipeline.R
│  │         sources: esca_ml_config.R
│  │         reads: all 8 ★ data files
│  │         └──→ results/<config>/cv_performance_tidymodels.csv
│  │              results/<config>/test_performance_tidymodels.csv
│  │              results/<config>/variable_importance_tidymodels.csv
│  │              results/<config>/finalized_models_tidymodels.rds
│  │              results/<config>/tuning_results_tidymodels.rds
│  │
│  └──→ [8] run_all_configurations.R
│             sources: esca_ml_config.R, esca_ml_pipeline.R
│             runs 8 scenarios (monthly + seasonal variants)
│             └──→ results/combined_performance_summary.csv
│                  results/all_configurations_results.rds
│                  results/combined_variable_importance.csv
│                  results/overall_variable_importance.csv
│                  results/{chasselas_only,exclude_sauvignon,stable_chasselas,main_varieties,
│                           chasselas_seasonal,exclude_sauvignon_seasonal,
│                           stable_chasselas_seasonal,main_varieties_seasonnal}/
```

## Data File Provenance

| Data File | Status | Created By |
|---|---|---|
| `data/Esca et RU des sols Vivian.xlsx` | **ORIGINAL** | External (soil data) |
| `data/Esca_CH_2001-2022.csv` | Derived | `Esca_preprocess.R` ← raw Excel |
| `data/Esca_CH_2001-2022_stable_plots.csv` | Derived | `Esca_preprocess.R` ← raw Excel |
| `data/vitimeteo_stations_info.csv` | Downloaded | `vitimeteo_infos.R` ← Vitimeteo API |
| `data/plot_station_associations.csv` | Derived | `plot_Vitimeteo_associations.R` |
| `data/weather_data_Agrometeo_Esca_plots.csv` | Downloaded | `API_Vitimeteo_download.R` ← Vitimeteo API (hourly) |
| `data/weather_data_Agrometeo_Esca_plots_monthly.csv` | Downloaded | `efficient_vitimeteo_download.R` ← Vitimeteo API (monthly) |
| `data/VPD_data_Agrometeo.csv` | Derived | `MaxDailyVPD.R` ← hourly Vitimeteo data |
| `data/VPD_data_Agrometeo_monthly.csv` | Derived | `MaxDailyVPD.R` ← hourly Vitimeteo data |
| `data/meteoswiss_weather_data_1995_2024.csv` | Downloaded | `meteoswiss_historical_data.R` ← MeteoSwiss OGD |
| `data/plot_meteoswiss_associations.csv` | Derived | `plot_MeteoSwiss_associations.R` |
| `data/VPD_data_MeteoSwiss_monthly.csv` | Derived | `compute_meteoswiss_vpd.R` ← MeteoSwiss OGD (hourly T+RH) |

## Weather Data Merging Strategy

The ML pipeline (`load_merged_weather_data()` in `esca_ml_pipeline.R`) uses a **Vitimeteo-first strategy with MeteoSwiss fallback**:

- For each (plot, Year), if the nearest Vitimeteo station is ≤ 30 km and has data → use Vitimeteo
- Otherwise → fall back to the nearest MeteoSwiss station (always complete)
- **Provenance is tracked** per variable group via `source_*` columns (e.g. `source_Temperature = "vitimeteo"`)
- Pressure is always from MeteoSwiss (not measured by Vitimeteo)
- VPD is computed as max daily VPD from hourly data (Tetens formula), then averaged monthly

Typical provenance: ~96% of Temperature/Humidity from Vitimeteo, ~71% of VPD from Vitimeteo (more gaps in hourly data).

## Execution Order (Full Reproduction)

To reproduce from scratch:

1. **`Esca_preprocess.R`** — requires raw Excel on network mount
2. **`vitimeteo_infos.R`** — queries Vitimeteo API for station metadata
3. **`plot_Vitimeteo_associations.R`** — spatial matching plots ↔ Vitimeteo stations
4. **`API_Vitimeteo_download.R`** (hourly) then **`efficient_vitimeteo_download.R`** (monthly) — download weather data
5. **`MaxDailyVPD.R`** — compute VPD from hourly Vitimeteo data
6. **`meteoswiss_historical_data.R`** — download MeteoSwiss monthly data
7. **`plot_MeteoSwiss_associations.R`** — spatial matching plots ↔ MeteoSwiss stations
8. **`compute_meteoswiss_vpd.R`** — compute VPD from hourly MeteoSwiss data
9. **`run_all_configurations.R`** — run the full ML pipeline across 8 configurations

Steps 2–5 and 6–8 can be run in parallel (independent data sources).

## Analysis Configurations

Eight predefined configurations are run by `run_all_configurations.R`:

| Configuration | Filter | Observations |
|---|---|---|
| **Chasselas Only** | `variety == "chasselas"` | Chasselas plots only |
| **Exclude Sauvignon** | `variety != "sauvignon blanc"` | All except Sauvignon Blanc |
| **Stable Chasselas** | `variety == "chasselas"`, group `2004-2022` | Stable observation plots only |
| **Main Varieties** | `variety in {chasselas, gamay, pinot noir, pinot gris}` | Main varietal subset |
| **Chasselas Only (Seasonal Agg)** | Chasselas, seasonal climate aggregates only | Seasonal feature set |
| **Exclude Sauvignon (Seasonal Agg)** | Exclude Sauvignon, seasonal climate aggregates only | Seasonal feature set |
| **Stable Chasselas (Seasonal Agg)** | Stable Chasselas + seasonal climate aggregates | Seasonal feature set |
| **Main Varieties (Seasonal Agg)** | Main varieties + seasonal climate aggregates | Seasonal feature set |

## Final-Step Dependency Scope (run_all_configurations.R)

For the final analysis commit phase, `run_all_configurations.R` depends directly on:

- `run_all_configurations.R`
- `esca_ml_config.R`
- `esca_ml_pipeline.R`
- `combined_variable_importance.R`

Transitive input data dependencies read by the pipeline:

- `data/Esca_CH_2001-2022.csv`
- `data/Esca et RU des sols Vivian.xlsx`
- `data/plot_station_associations.csv`
- `data/plot_meteoswiss_associations.csv`
- `data/weather_data_Agrometeo_Esca_plots_monthly.csv`
- `data/meteoswiss_weather_data_1995_2024.csv`
- Optional (if present): `data/VPD_data_Agrometeo_monthly.csv`
- Optional (if present): `data/VPD_data_MeteoSwiss_monthly.csv`

Primary outputs produced by this final step:

- `results/combined_performance_summary.csv`
- `results/all_configurations_results.rds`
- `results/combined_variable_importance.csv`
- `results/overall_variable_importance.csv`
- `results/<config_name>/` per-configuration files (performance, predictions, importance, fitted models)

## Analysis Methods

Three models are tuned via 10-fold cross-validation:

1. **Random Forest** (ranger): 27 hyperparameter combinations (mtry, trees, min_n)
2. **Gradient Boosting** (xgboost): 64 combinations (trees, depth, learn_rate, mtry, min_n, loss_reduction)
3. **LASSO Regression** (glmnet): 20 penalty values (pure L1 regularization)

## Key Variables

#### Environmental Predictors (March–October monthly values)
- **Temperature**, **Humidity**, **Precipitation**, **Evapotranspiration**, **Radiation**, **Pressure**
- **VPD** (Vapor Pressure Deficit): computed from hourly T + RH as max daily VPD, then monthly average
- Seasonal aggregates: `SeasTemp`, `SeasPrecipitation`, `SeasVPD`, etc.

#### Soil Variables
- **RU**: Soil water reserve capacity categories (from `Esca et RU des sols Vivian.xlsx`)

#### Response Variable
- **SumSymptApo**: Combined Esca symptom severity (% apoplexy + % leaf symptoms)

## R Dependencies

**Core**: tidymodels, tidyverse, readxl, lubridate, xgboost, ranger, glmnet, vip, finetune, doParallel  
**Utilities**: corrplot, probably, httr, data.table, lightgbm  
**External**: Access to raw ESCA Excel file (network mount), Vitimeteo API, MeteoSwiss OGD
