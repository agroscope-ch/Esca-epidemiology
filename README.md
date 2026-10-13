# Esca Epidemiology — Supplementary Material

> Supplementary analysis code for the Esca epidemiology paper.
> Reproduces the machine-learning analyses, FORD variable selection, and manuscript report from the data in this repository, using a data set hosted on Zenodo.

---

## Quick Start

Note that the original data set is hosted on Zenodo:

``
https://zenodo.org/records/21704548
``

and that a description of the variables is available both on Zenodo and in the file [description dataset.md](description_dataset.md)

### 0. One-time setup (package environment)

The exact package versions used for the paper are recorded in `renv.lock` (R 4.6.1).
Open `esca_epidemiologie_paper.Rproj` in RStudio (or start R in the repository root), which
automatically activates the project's private library via `renv`, then run **once**:

```r
install.packages("renv")   # only if renv is not yet available; it is bootstrapped automatically otherwise
renv::restore()            # answer "y" when asked; installs all packages at the locked versions
```

Packages are installed into the project (`renv/library/`), not into your global R library, so
nothing else on your machine is affected. This can take 10-30 minutes the first time. Some packages
(`sf`, `xgboost`, `lightgbm`) may need system libraries on Linux (e.g. GDAL/GEOS/PROJ); on Windows/macOS
binaries are used and nothing else is required.

If `library(vip)` (or any other package) is reported as unavailable, `renv::restore()` has not been
run, or was interrupted: run `renv::status()` to see what is missing, then `renv::restore()` again.

### 1-3. Run the analysis

Run the three steps below from the repository root in an R session:

```r
# 1. Run all ML configurations (8 scenarios × 3 models)
source("run_all_configurations.R")

# 2. Run FORD variable selection
source("ford_analysis.R")

# 3. Knit the manuscript report
rmarkdown::render("Esca_Epidemiology.Rmd")
```

This reproduces the main performance tables, variable-importance rankings, and FORD sections of the manuscript. Map rendering requires external GIS assets (see [External Dependencies](#external-dependencies)); without them, the report knits with a fallback message.

---

## Repository Structure

Once the different scripts have been run, the structure of the folder is the following: 

```
├── data/                       # Tracked input data (disease, weather, soil, climate)
├── results/                    # ML pipeline outputs per configuration
└── plots/                      # Report-generated plots
```

### Core Analysis Scripts

| Script | Role |
|---|---|
| `run_all_configurations.R` | Orchestrates the ML pipeline across 8 configurations |
| `esca_ml_config.R` | Defines configuration scenarios (filters, feature sets) |
| `esca_ml_pipeline.R` | Core ML pipeline (tidymodels: Random Forest, XGBoost, LASSO) |
| `combined_variable_importance.R` | Aggregates variable importance across configurations |
| `ford_analysis.R` | FORD forward variable selection analysis |

### Report

| File | Description |
|---|---|
| `Esca_Epidemiology.Rmd` | Main manuscript report (HTML output) |

---

## Data Requirements

### Tracked Input Data

All files below are tracked in the repository — no external download is needed:

| File | Description |
|---|---|
| `data/RU_data.csv` | Soil water reserve capacity per plot |
| `data/plot_station_associations.csv` | Plot ↔ Vitimeteo station spatial associations |
| `data/plot_meteoswiss_associations.csv` | Plot ↔ MeteoSwiss station spatial associations |
| `data/weather_data_Agrometeo_Esca_plots_monthly.csv` | Monthly Vitimeteo weather data |
| `data/meteoswiss_weather_data_1995_2024.csv` | Monthly MeteoSwiss weather data |
| `data/VPD_data_Agrometeo_monthly.csv` | Monthly VPD from Vitimeteo (optional) |
| `data/VPD_data_MeteoSwiss_monthly.csv` | Monthly VPD from MeteoSwiss (optional) |
| `data/regional_climate_P_ET_summary.csv` | Regional climate summary (optional, for report) |
| `data/regional_climate_P_ET_yearly.csv` | Regional climate yearly data (optional, for report) |

### Generated Outputs

The report (`Esca_Epidemiology.Rmd`) reads outputs produced by steps 1 and 2:

- `results/combined_performance_summary.csv`
- `results/combined_variable_importance.csv`
- `results/ford_variable_selection.csv`
- `results/ford_variable_selection_seasonal.csv`

---

## Dependency Graph

```
run_all_configurations.R
 ├── sources: esca_ml_config.R, esca_ml_pipeline.R
 ├── reads:   Esca_CH_2001-2022.csv (on Zenodo), data/RU_data.csv,
 │            data/plot_station_associations.csv, data/plot_meteoswiss_associations.csv,
 │            data/weather_data_Agrometeo_Esca_plots_monthly.csv,
 │            data/meteoswiss_weather_data_1995_2024.csv,
 │            (optional) data/VPD_data_*.csv
 ├── writes:  results/<config>/ (per-config outputs)
 │            results/combined_performance_summary.csv
 └── triggers: combined_variable_importance.R

ford_analysis.R
 ├── sources: esca_ml_config.R, esca_ml_pipeline.R
 └── writes:  results/ford_variable_selection.csv
              results/ford_variable_selection_seasonal.csv

Esca_Epidemiology.Rmd
 ├── reads:   Esca_CH_2001-2022.csv (on Zenodo),
 │            results/combined_performance_summary.csv,
 │            results/combined_variable_importance.csv,
 │            results/ford_variable_selection*.csv
 └── writes:  plots/*.png, Esca_Epidemiology.html
```

---

## Analysis Configurations

`run_all_configurations.R` executes 8 scenarios (4 variety filters × 2 feature sets):

| Configuration | Variety Filter | Feature Set |
|---|---|---|
| Chasselas Only | `chasselas` | Monthly |
| Exclude Sauvignon | All except `sauvignon blanc` | Monthly |
| Stable Chasselas | `chasselas`, plots observed 2004–2022 | Monthly |
| Main Varieties | `chasselas, gamay, pinot noir, pinot gris` | Monthly |
| Chasselas Seasonal | `chasselas` | Seasonal aggregates |
| Exclude Sauvignon Seasonal | All except `sauvignon blanc` | Seasonal aggregates |
| Stable Chasselas Seasonal | `chasselas`, plots observed 2004–2022 | Seasonal aggregates |
| Main Varieties Seasonal | `chasselas, gamay, pinot noir, pinot gris` | Seasonal aggregates |

Three models are tuned via 10-fold cross-validation per configuration:
**Random Forest** (ranger), **Gradient Boosting** (XGBoost), **LASSO** (glmnet).

---

## External Dependencies

Optional components that affect only map rendering in the report:

- `map_Esca_sampling_plots.R` (local script, not tracked)
- Swiss GIS assets expected under `../swiss-maps/`

If absent, map chunks in `Esca_Epidemiology.Rmd` fall back gracefully and the report can still be knitted.

---

## R Package Dependencies

Dependencies are managed with [`renv`](https://rstudio.github.io/renv/):

- `renv.lock` – exact versions of all packages (committed; do not edit by hand)
- `renv/`, `.Rprofile` – activation scripts (committed); `renv/library/` is local and git-ignored
- `renv_engine_dependencies.R` – declares packages used only indirectly as tidymodels engines (`ranger`, `glmnet`) so that `renv` records them; it is never executed
- `.renvignore` – folders that are not scanned for dependencies (archived scripts, outputs, LaTeX)

Main packages: `tidymodels`, `tidyverse`, `readxl`, `lubridate`, `xgboost`, `ranger`, `glmnet`, `lightgbm`, `vip`, `finetune`, `doParallel`,
`FORD`, `qgam`, `plotly`, `patchwork`, `kableExtra`, `sf`, `corrplot`, `probably`.

The optional `tabpfn` model (GitHub-only, needs Python) is deliberately **not** part of the lockfile; the pipeline
skips it when the package is absent.

**Maintainers:** after adding or upgrading a package, run `renv::snapshot()` and commit the updated `renv.lock`.

---

## Compiling the paper (LaTeX)

The manuscript lives in `paper_tex/`:

- `main.tex` – preamble; each section is pulled in with `\input{sections/...}`
- `sections/` – one file per section (abstract, introduction, methods, results, discussion, acknowledgements, supplementary)
- `Esca_Epidemiology.bib` – bibliography database (BibTeX, `natbib` + `plainnat`)
- figures are read from `plots/paper/*.pdf` (produced by the `paper_figures` chunk of `Esca_Epidemiology.Rmd`), so keep `paper_tex/` and `plots/` side by side

Compile from inside `paper_tex/` (BibTeX, **not** biber):

```bash
cd paper_tex
pdflatex main.tex && bibtex main && pdflatex main.tex && pdflatex main.tex
```

or, equivalently, `latexmk -pdf main.tex`. Both give `main.pdf`.

If the bibliography looks wrong (missing references, `I found no \citation commands`), check that `main.tex` loads
`\usepackage[round]{natbib}` and ends with `\bibliographystyle{plainnat}` / `\bibliography{Esca_Epidemiology}`
(not `biblatex` / `\printbibliography`), then clear stale files with `latexmk -C` and rebuild.

---

## Notes

- The seasonal main-varieties output folder is named `main_varieties_seasonnal` (legacy typo preserved for consistency).
- `prepare_ru_data.R` regenerates `data/RU_data.csv` from the original soil workbook when needed.
