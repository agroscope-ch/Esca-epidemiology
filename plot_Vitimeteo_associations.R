## ----setup--------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readr)
library(sf)         # for coordinate transformation

## ----load-data----------------------------------------------------------------

# Load the main Esca dataset
esca_data <- read_csv("data/Esca_CH_2001-2022.csv", 
                      locale = locale(encoding = "latin1"))

# Load the stable plots information
stable_plots <- read_csv("data/Esca_CH_2001-2022_stable_plots.csv", 
                         locale = locale(encoding = "latin1"))

## ----vitimeteo-station-association--------------------------------------------

#' Convert degrees to radians
#' @param degrees Value in degrees
#' @return Value in radians
degrees_to_radians <- function(degrees) {
  return(degrees * pi / 180)
}

#' Calculate distance in kilometers between two Earth coordinates using Haversine formula
#' @param lat1 Latitude of first point in degrees
#' @param lon1 Longitude of first point in degrees
#' @param lat2 Latitude of second point in degrees
#' @param lon2 Longitude of second point in degrees
#' @return Distance in kilometers
distance_km_between_coordinates <- function(lat1, lon1, lat2, lon2) {
  earth_radius_km <- 6371
  
  d_lat <- degrees_to_radians(lat2 - lat1)
  d_lon <- degrees_to_radians(lon2 - lon1)
  
  lat1_rad <- degrees_to_radians(lat1)
  lat2_rad <- degrees_to_radians(lat2)
  
  a <- sin(d_lat/2) * sin(d_lat/2) + 
       sin(d_lon/2) * sin(d_lon/2) * cos(lat1_rad) * cos(lat2_rad)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  return(earth_radius_km * c)
}

#' Convert Swiss coordinates (CH1903 / LV03) to WGS84 (lat/long)
#' @param e_coord Easting coordinate in Swiss system
#' @param n_coord Northing coordinate in Swiss system
#' @return data.frame with lat and long columns
swiss_to_wgs84 <- function(e_coord, n_coord) {
  # Create spatial points with Swiss coordinate system (EPSG:21781 - CH1903 / LV03)
  points_sf <- st_as_sf(
    data.frame(E = e_coord, N = n_coord), 
    coords = c("E", "N"), 
    crs = 21781
  )
  
  # Transform to WGS84 (EPSG:4326)
  points_wgs84 <- st_transform(points_sf, crs = 4326)
  
  # Extract coordinates
  coords <- st_coordinates(points_wgs84)
  return(data.frame(long = coords[,1], lat = coords[,2]))
}

#' Build plot-year-station associations using local station data
#' @param esca_data Main esca dataset
#' @param use_fallback_for_early_years If TRUE, use closest station regardless of date for years with no stations
#' @return data.frame with plot, year, station associations
build_plot_station_associations <- function(esca_data, use_fallback_for_early_years = FALSE) {
  stations_info <- read_csv("data/vitimeteo_stations_info.csv")
  
  # Get unique plots with coordinates
  unique_plots <- esca_data |>
    select(plot, E, N) |>
    filter(!is.na(E), !is.na(N)) |>
    distinct()
  
  # Convert coordinates to WGS84
  # function tested in a separate test
  coords_wgs84 <- swiss_to_wgs84(unique_plots$E, unique_plots$N)
  unique_plots <- bind_cols(unique_plots, coords_wgs84)
  
  # Get years available for each plot
  plot_years <- esca_data |>
    select(plot, Year) |>
    distinct() |>
    arrange(plot, Year)
  

  # Check station availability by year for reporting
  esca_years <- sort(unique(plot_years$Year))
  early_years_no_stations <- c()
  
  for (year in esca_years) {
    available_count <- stations_info |>
      filter(!is.na(puttingIntoService), 
             as.Date(puttingIntoService) <= as.Date(paste0(year, "-12-31"))) |>
      nrow()
    
    if (available_count == 0) {
      early_years_no_stations <- c(early_years_no_stations, year)
      cat("Year", year, ": 0 stations available\n")
    } else {
      cat("Year", year, ":", available_count, "stations available\n")
    }
  }
  
  if (length(early_years_no_stations) > 0) {
    cat("\nWARNING: Years", paste(early_years_no_stations, collapse=", "), 
        "have no weather stations available.\n")
    if (use_fallback_for_early_years) {
      cat("Using fallback: Will assign closest station regardless of service date for these years.\n")
    } else {
      cat("These plot-year combinations will have NA station assignments.\n")
      cat("To use closest available station for these years, set use_fallback_for_early_years=TRUE\n")
    }
  }
  cat("\n")
  
  # Create all plot-year-station associations
  all_associations <- data.frame()
  no_station_count <- 0
  no_coordinates_count <- 0
  
  for (i in 1:nrow(plot_years)) {
    if (i %% 100 == 0) cat("Processing", i, "of", nrow(plot_years), "combinations...\n")
    
    plot_id <- plot_years$plot[i]
    year <- plot_years$Year[i]
    
    # Get plot coordinates
    plot_coords <- unique_plots[unique_plots$plot == plot_id, ]
    
    if (nrow(plot_coords) == 0) {
      no_coordinates_count <- no_coordinates_count + 1
      next  # Skip plots without coordinates
    }
    
    # Handle multiple coordinate pairs for the same plot (use the first one)
    # For many plots, the actual location where data were collected did slighlty
    # change over the years. The plot index stayed the same, while
    # the location changed. But this should not make any difference 
    # in the attribution of the Vitimeteo station
    plot_lat <- plot_coords$lat[1]
    plot_long <- plot_coords$long[1]
    
    # Filter stations that have data for this year
    available_stations <- stations_info |>
      filter(
        !is.na(puttingIntoService),
        as.Date(puttingIntoService) <= as.Date(paste0(year, "-12-31"))
      )
    
    # If no stations available and fallback is enabled, use all stations
    if (nrow(available_stations) == 0 && use_fallback_for_early_years) {
      available_stations <- stations_info |>
        filter(!is.na(latitude), !is.na(longitude))
      
      if (nrow(available_stations) > 0) {
        cat("Using fallback for plot", plot_id, "year", year, "- assigned closest of", 
            nrow(available_stations), "total stations\n")
      }
    }
    
    if (nrow(available_stations) == 0) {
      # No stations available for this year - create NA association for debugging
      no_station_count <- no_station_count + 1
      if (no_station_count <= 5) {  # Only show first few for debugging
        cat("No stations available for plot", plot_id, "year", year, "\\n")
      }
      association <- data.frame(
        plot = plot_id,
        Year = year,
        station_id = NA,
        station_name = NA,
        distance_to_station = NA,
        plot_lat = plot_lat,
        plot_long = plot_long,
        station_lat = NA,
        station_long = NA,
        putting_into_service = NA,
        stringsAsFactors = FALSE
      )
      all_associations <- rbind(all_associations, association)
      next
    }
    
    # Calculate distances to all available stations using Haversine formula
    distances <- sapply(1:nrow(available_stations), function(idx) {
      distance_km_between_coordinates(
        plot_lat, plot_long,
        available_stations$latitude[idx], available_stations$longitude[idx]
      )
    })
    
    # Find closest station
    closest_idx <- which.min(distances)
    closest_station <- available_stations[closest_idx, ]
    
    # Add to associations
    association <- data.frame(
      plot = plot_id,
      Year = year,
      station_id = closest_station$station_id,
      station_name = closest_station$station_name,
      distance_to_station = distances[closest_idx],
      plot_lat = plot_lat,
      plot_long = plot_long,
      station_lat = closest_station$latitude,
      station_long = closest_station$longitude,
      putting_into_service = closest_station$puttingIntoService,
      stringsAsFactors = FALSE
    )
    
    all_associations <- rbind(all_associations, association)
  }
  
  cat("Created", nrow(all_associations), "plot-year-station associations\n")
  cat("Plot-year combinations skipped (no coordinates):", no_coordinates_count, "\n")
  cat("Associations with no available stations:", no_station_count, "\n")
  
  # Summary of results
  valid_associations <- sum(!is.na(all_associations$station_id))
  na_associations <- sum(is.na(all_associations$station_id))
  
  cat("\n=== ASSOCIATION RESULTS SUMMARY ===\n")
  cat("Total plot-year combinations processed:", nrow(plot_years), "\n")
  cat("Combinations with coordinates:", nrow(all_associations), "\n")
  cat("Combinations skipped (no coordinates):", no_coordinates_count, "\n")
  cat("Valid station assignments:", valid_associations, "\n")
  cat("NA station assignments:", na_associations, "\n")
  
  if (na_associations > 0) {
    # Show which years have NA assignments
    na_years <- all_associations |>
      filter(is.na(station_id)) |>
      pull(Year) |>
      unique() |>
      sort()
    
    cat("Years with NA assignments:", paste(na_years, collapse=", "), "\n")
    cat("Suggestion: Consider using use_fallback_for_early_years=TRUE if needed\n")
  }
  
  # Add commune information by joining with esca_data
  plot_commune <- esca_data |>
    select(plot, commune) |>
    distinct()
  
  all_associations <- all_associations |>
    left_join(plot_commune, by = "plot")
  

  return(all_associations)
}


# Build the associations using local station data
# Note: Set use_fallback_for_early_years=TRUE if you want to assign closest station 
# even for years before stations were operational
plot_station_associations <- build_plot_station_associations(esca_data, use_fallback_for_early_years = FALSE)

# Save the results
write_csv(plot_station_associations, "data/plot_station_associations.csv")
cat("Results saved to data/plot_station_associations.csv\n")

# Display summary
cat("\n=== ASSOCIATION SUMMARY ===\n")
cat("Total plot-year-station associations:", nrow(plot_station_associations), "\n")
cat("Unique plots with stations:", length(unique(plot_station_associations$plot)), "\n")
cat("Unique stations used:", length(unique(plot_station_associations$station_id)), "\n")
cat("Years covered:", min(plot_station_associations$Year), "-", max(plot_station_associations$Year), "\n")

# Show stations used
cat("\nStations used:\n")
stations_summary <- plot_station_associations |>
  group_by(station_id, station_name) |>
  summarise(
    n_associations = n(),
    min_year = min(Year),
    max_year = max(Year),
    n_plots = n_distinct(plot),
    .groups = "drop"
  ) |>
  arrange(desc(n_associations))

print(stations_summary)

cat("\n=== STATION ASSOCIATION COMPLETE ===\n")
