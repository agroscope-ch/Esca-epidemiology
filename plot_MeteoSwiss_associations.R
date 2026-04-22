# ===============================================================================
# Plot-MeteoSwiss Station Associations for Climatological Norms
# 
# This script assigns each vineyard plot to its nearest MeteoSwiss station
# for calculating climatological baselines and anomalies
# Data period: 2001-2022
# ===============================================================================

library(tidyverse)
library(readr)
library(sf)         # for coordinate transformation

# Source coordinate functions from esca_analysis.R
source("plot_Vitimeteo_associations.R")

cat("PLOT-METEOSWISS STATION ASSOCIATIONS\n") 


## ----load-data----------------------------------------------------------------

# Load the main Esca dataset for plot coordinates
esca_data <- read_csv("data/Esca_CH_2001-2022.csv", 
                      locale = locale(encoding = "latin1"))

# Load MeteoSwiss daily data to get station information
meteoswiss_data <- read_csv("data/meteoswiss_weather_data_1995_2024.csv")

# Extract unique station information from MeteoSwiss data
meteoswiss_stations <- meteoswiss_data |> 
  select(stat_abbr, station_name, latitude, longitude, altitude) |> 
  distinct() |> 
  filter(!is.na(latitude), !is.na(longitude))

## ----plot-coordinates--------------------------------------------------------

# Get unique plots with coordinates (reusing logic from esca_analysis.R)
unique_plots <- esca_data %>%
  select(plot, E, N) %>%
  filter(!is.na(E), !is.na(N)) %>%
  distinct()

# Convert Swiss coordinates to WGS84 for distance calculations
coords_wgs84 <- swiss_to_wgs84(unique_plots$E, unique_plots$N)
unique_plots <- bind_cols(unique_plots, coords_wgs84)

## ----station-assignments-----------------------------------------------------

#' Assign plots to nearest MeteoSwiss stations
#' @param plots_data Data frame with plot coordinates (lat, long)
#' @param stations_data Data frame with station coordinates and metadata
#' @return Data frame with plot-station assignments and distances
assign_plots_to_meteoswiss_stations <- function(plots_data, stations_data) {
  
  assignments <- data.frame()
  
  for (i in 1:nrow(plots_data)) {
    if (i %% 25 == 0) cat("Processing plot", i, "of", nrow(plots_data), "...\n")
    
    plot_id <- plots_data$plot[i]
    plot_lat <- plots_data$lat[i]
    plot_long <- plots_data$long[i]
    plot_e <- plots_data$E[i]
    plot_n <- plots_data$N[i]
    
    # Calculate distances to all MeteoSwiss stations
    distances <- sapply(1:nrow(stations_data), function(idx) {
      distance_km_between_coordinates(
        plot_lat, plot_long,
        stations_data$latitude[idx], stations_data$longitude[idx]
      )
    })
    
    # Find closest station
    closest_idx <- which.min(distances)
    closest_station <- stations_data[closest_idx, ]
    
    # Create assignment record
    assignment <- data.frame(
      plot = plot_id,
      plot_e = plot_e,
      plot_n = plot_n,
      plot_lat = plot_lat,
      plot_long = plot_long,
      meteoswiss_station_abbr = closest_station$stat_abbr,
      meteoswiss_station_name = closest_station$station_name,
      meteoswiss_station_lat = closest_station$latitude,
      meteoswiss_station_long = closest_station$longitude,
      meteoswiss_station_altitude = closest_station$altitude,
      distance_to_meteoswiss_km = distances[closest_idx],
      stringsAsFactors = FALSE
    )
    
    assignments <- rbind(assignments, assignment)
  }
  
  return(assignments)
}

# Perform the assignments
cat("\n=== ASSIGNING PLOTS TO METEOSWISS STATIONS ===\n")
plot_meteoswiss_associations <- assign_plots_to_meteoswiss_stations(unique_plots, meteoswiss_stations)


# Add commune and region information from original dataset
plot_metadata <- esca_data %>%
  select(plot, commune, region) %>%
  distinct()

plot_meteoswiss_associations <- plot_meteoswiss_associations %>%
  left_join(plot_metadata, by = "plot")


# Export results
output_file <- "data/plot_meteoswiss_associations.csv"
write_csv(plot_meteoswiss_associations, output_file)
cat("\nResults saved to", output_file, "\n")
