# ==============================================================================
# Regional climate characterization
#
# For each study region, download precipitation and evapotranspiration
# from a representative MeteoSwiss station (1996-2025), then compute:
#   - yearly ratio (native yearly variables), and/or
#   - seasonal ratio from monthly sums over a configurable month window.
# ==============================================================================

library(tidyverse)
library(lubridate)
source("meteoswiss_download_funs.R")

# --- 0. Run configuration ------------------------------------------------------

analysis_start_year <- 1996
analysis_end_year <- 2025

# Choose one or both modes: "yearly_native", "seasonal_monthly"
modes_to_run <- c("seasonal_monthly", "yearly_native")

# Seasonal monthly configuration (used only for "seasonal_monthly")
# Signed month convention for season_start_month:
#   1..12  = month in anchor year
#   -1..-12 = month in previous year (-1 = Dec prev year, -2 = Nov prev year)
season_start_month <- -4L # Last September
season_end_month <- 8L    # August of the current year

build_relative_month_window <- function(start_month, end_month) {
  if (start_month < 1L) {
    c(seq.int(start_month, -1L), seq.int(1L, end_month))
  } else {
    seq.int(start_month, end_month)
  }
}

build_season_window_definition <- function(start_month, end_month) {
  relative_months <- build_relative_month_window(start_month, end_month)

  tibble(relative_month = relative_months) |>
    mutate(
      year_offset = if_else(relative_month < 1L, -1L, 0L),
      calendar_month = if_else(relative_month < 1L,
                               relative_month + 13L,
                               relative_month),
      window_month_key = paste0(year_offset, "_", calendar_month)
    )
}

get_required_data_start_date <- function(mode_name,
                                         analysis_start_year,
                                         season_start_month) {
  if (mode_name != "seasonal_monthly") {
    return(as.Date(sprintf("%d-01-01", analysis_start_year)))
  }

  if (season_start_month < 1L) {
    return(as.Date(sprintf("%d-%02d-01",
                           analysis_start_year - 1L,
                           season_start_month + 13L)))
  }

  as.Date(sprintf("%d-%02d-01", analysis_start_year, season_start_month))
}

if (!all(modes_to_run %in% c("yearly_native", "seasonal_monthly"))) {
  stop("modes_to_run must contain only: 'yearly_native', 'seasonal_monthly'.")
}

if (season_start_month == 0L || season_start_month < -12L || season_start_month > 12L) {
  stop("season_start_month must be in [-12, -1] or [1, 12].")
}

if (season_end_month < 1L || season_end_month > 12L) {
  stop("season_end_month must be in [1, 12].")
}

if (season_end_month < season_start_month) {
  stop("season_end_month must be >= season_start_month (signed convention).")
}

season_window_definition <- build_season_window_definition(
  start_month = season_start_month,
  end_month = season_end_month
)

if (nrow(season_window_definition) > 12L) {
  stop("Season window must cover at most 12 months.")
}

mode_definitions <- tribble(
  ~mode,               ~precip_var, ~et_var,    ~description,
  "yearly_native",    "rre150y0", "erefaoy0", "Yearly precipitation/ET ratio",
  "seasonal_monthly", "rre150m0", "erefaom0", "Seasonal monthly sums (P/ET)"
) |> 
  filter(mode %in% modes_to_run)

if (nrow(mode_definitions) == 0) {
  stop("No valid mode selected in modes_to_run.")
}

compute_ratio_for_mode <- function(df_mode,
                                   mode_row,
                                   season_start_month,
                                   season_end_month,
                                   analysis_start_year,
                                   analysis_end_year) {
  precip_var <- mode_row$precip_var
  et_var <- mode_row$et_var
  mode_name <- mode_row$mode
  season_window <- build_season_window_definition(
    start_month = season_start_month,
    end_month = season_end_month
  )
  expected_month_count <- nrow(season_window)
  previous_year_months <- season_window |>
    filter(year_offset == -1L) |>
    pull(calendar_month)

  if (!all(c(precip_var, et_var) %in% names(df_mode))) {
    warning(sprintf(
      "Mode '%s': missing required columns (%s, %s). Returning empty table.",
      mode_name, precip_var, et_var
    ))
    return(tibble())
  }

  if (mode_name == "yearly_native") {
    return(
      df_mode |>
        transmute(
          region,
          station,
          year,
          Precipitation_mm = as.numeric(.data[[precip_var]]),
          Evapotranspiration_mm = as.numeric(.data[[et_var]])
        ) |>
        filter(!is.na(Precipitation_mm), !is.na(Evapotranspiration_mm)) |>
        mutate(
          Ratio_P_ET = Precipitation_mm / Evapotranspiration_mm,
          mode = mode_name,
          season_start_month = NA_integer_,
          season_end_month = NA_integer_
        )
    )
  }

  seasonal_input <- df_mode |>
    mutate(
      calendar_year = year(datetime),
      calendar_month = month(datetime),
      precip_mm = as.numeric(.data[[precip_var]]),
      et_mm = as.numeric(.data[[et_var]])
    ) |>
    mutate(
      year_offset = if_else(calendar_month %in% previous_year_months, -1L, 0L),
      season_year = calendar_year - year_offset,
      window_month_key = paste0(year_offset, "_", calendar_month)
    ) |>
    semi_join(
      season_window |>
        distinct(year_offset, calendar_month),
      by = c("year_offset", "calendar_month")
    ) |>
    filter(season_year >= analysis_start_year, season_year <= analysis_end_year)

  complete_years <- seasonal_input |>
    filter(!is.na(precip_mm), !is.na(et_mm)) |>
    group_by(region, station, season_year) |>
    summarise(n_complete_months = n_distinct(window_month_key), .groups = "drop") |>
    filter(n_complete_months == expected_month_count)

  dropped_years <- seasonal_input |>
    distinct(region, station, season_year) |>
    anti_join(complete_years, by = c("region", "station", "season_year"))

  if (nrow(dropped_years) > 0) {
    cat(sprintf(
      "\nMode '%s': dropped %d station-year rows with incomplete anchor-season coverage (%d-%d).\n",
      mode_name, nrow(dropped_years), season_start_month, season_end_month
    ))
  }

  seasonal_input |>
    inner_join(complete_years, by = c("region", "station", "season_year")) |>
    group_by(region, station, season_year) |>
    summarise(
      Precipitation_mm = sum(precip_mm, na.rm = FALSE),
      Evapotranspiration_mm = sum(et_mm, na.rm = FALSE),
      .groups = "drop"
    ) |>
    rename(year = season_year) |>
    mutate(
      Ratio_P_ET = Precipitation_mm / Evapotranspiration_mm,
      mode = mode_name,
      season_start_month = season_start_month,
      season_end_month = season_end_month
    )
}

# --- 1. Regions and representative stations -----------------------------------
# Regions from the Esca dataset and one MeteoSwiss station per region.
# Station selection based on proximity to the main vineyard areas in each region.

region_stations <- tribble(

  ~region,         ~station, ~station_name,
  "Chablais",      "AIG",   "Aigle",
  "La Cote",       "CGI",   "Changins (Nyon)",
  "Lavaux",        "PUY",   "Pully",

  "Neuchatel",     "NEU",   "Neuchâtel",
  "Nord Vaudois",  "MAH",   "Mathod",
  "Valais",        "SIO",   "Sion"
)

cat("Regions and representative MeteoSwiss stations:\n")
print(region_stations)

# --- 1b. Check data availability in MeteoSwiss inventory ----------------------

cat("\nChecking data availability in MeteoSwiss inventory...\n")

old_np <- fix_proxy()
datainventory_url <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_datainventory.csv"
data_inventory <- read_delim(datainventory_url, delim = ";",
                             locale = locale(encoding = "UTF-8"),
                             show_col_types = FALSE)
restore_proxy(old_np)

all_ratio_data <- map_dfr(seq_len(nrow(mode_definitions)), function(m_idx) {
  mode_row <- mode_definitions[m_idx, ]
  mode_name <- mode_row$mode[[1]]
  precip_var <- mode_row$precip_var[[1]]
  et_var <- mode_row$et_var[[1]]
  required_vars <- c(precip_var, et_var)
  is_monthly_mode <- mode_name == "seasonal_monthly"

  cat(sprintf("\n--- Mode: %s ---\n", mode_name))
  if (is_monthly_mode) {
    cat(sprintf(
      "Using monthly variables (%s, %s) and months %d-%d.\n",
      precip_var, et_var, season_start_month, season_end_month
    ))
  } else {
    cat(sprintf("Using yearly variables (%s, %s).\n", precip_var, et_var))
  }

  required_start_date <- get_required_data_start_date(
    mode_name = mode_name,
    analysis_start_year = analysis_start_year,
    season_start_month = season_start_month
  )

  availability <- data_inventory |>
    filter(
      station_abbr %in% region_stations$station,
      parameter_shortname %in% required_vars
    ) |>
    select(station_abbr, parameter_shortname, data_since) |>
    mutate(data_since = as.Date(data_since, format = "%Y-%m-%d")) |>
    mutate(covers_start_year = data_since <= required_start_date)

  cat("\nData inventory for required variables:\n")
  print(availability, n = Inf)

  expected <- expand.grid(
    station_abbr = region_stations$station,
    parameter_shortname = required_vars,
    stringsAsFactors = FALSE
  )
  missing <- anti_join(expected, availability, by = c("station_abbr", "parameter_shortname"))

  if (nrow(missing) > 0) {
    cat("\n*** WARNING: The following station/variable combinations are NOT available ***\n")
    print(missing)
    cat("\nConsider choosing alternative stations for affected regions.\n")

    cat("\nAvailable similar-granularity variables at problematic stations:\n")
    suffix_pattern <- ifelse(is_monthly_mode, "m0$", "y0$")
    problem_stations <- unique(missing$station_abbr)
    data_inventory |>
      filter(station_abbr %in% problem_stations,
             grepl(suffix_pattern, parameter_shortname)) |>
      select(station_abbr, parameter_shortname, data_since) |>
      print(n = Inf)
  } else {
    cat("\nAll required variables available at all stations.\n")
  }

  late_start <- availability |> filter(!covers_start_year)
  if (nrow(late_start) > 0) {
    cat(sprintf(
      "\n*** WARNING: Some stations have data starting AFTER required date %s ***\n",
      required_start_date
    ))
    print(late_start)
  }

  download_start_year <- if (is_monthly_mode && season_start_month < 1L) {
    analysis_start_year - 1L
  } else {
    analysis_start_year
  }

  cat(sprintf(
    "\nDownloading data (%d-%d) for mode '%s'...\n",
    download_start_year, analysis_end_year, mode_name
  ))

  mode_raw <- map_dfr(seq_len(nrow(region_stations)), function(i) {
    stn <- region_stations$station[i]
    reg <- region_stations$region[i]

    cat(sprintf("  Station %s (%s)...\n", stn, reg))

    df <- get_meteoswiss(
      station   = stn,
      variables = required_vars,
      start     = as.character(download_start_year),
      end       = as.character(analysis_end_year)
    )

    df |>
      mutate(
        region  = reg,
        station = stn,
        year    = year(datetime)
      )
  })

  compute_ratio_for_mode(
    df_mode = mode_raw,
    mode_row = mode_row,
    season_start_month = season_start_month,
    season_end_month = season_end_month,
    analysis_start_year = analysis_start_year,
    analysis_end_year = analysis_end_year
  )
})

cat("\n--- Precipitation / Evapotranspiration ratio rows ---\n")
print(all_ratio_data)

# --- 4. Summary statistics per region ----------------------------------------

summary_stats <- all_ratio_data |>
  group_by(mode, season_start_month, season_end_month, region, station) |>
  summarise(
    mean_precip_mm  = mean(Precipitation_mm),
    mean_ET_mm      = mean(Evapotranspiration_mm),
    mean_ratio_P_ET = mean(Ratio_P_ET),
    sd_ratio_P_ET   = sd(Ratio_P_ET),
    min_ratio       = min(Ratio_P_ET),
    max_ratio       = max(Ratio_P_ET),
    n_years         = n(),
    .groups = "drop"
  ) |>
  arrange(mode, mean_ratio_P_ET)

cat("\n--- Summary by mode and region ---\n")
print(summary_stats)

# --- 5. Export ----------------------------------------------------------------

write_csv(all_ratio_data,
          "data/regional_climate_P_ET_yearly.csv")
write_csv(summary_stats,
          "data/regional_climate_P_ET_summary.csv")

cat("\nResults saved to data/regional_climate_P_ET_yearly.csv and data/regional_climate_P_ET_summary.csv\n")
