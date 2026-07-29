# MeteoSwiss Historical Weather Data Integration
# Purpose: Download daily weather data (2001-2022) for stations near Vitimeteo locations
# Author: Generated script for Esca epidemiology project
# Date: April 2026

# =============================================================================
# SETUP AND CONFIGURATION
# =============================================================================

# Load required libraries
library(httr)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(scales)

# Configuration
VITIMETEO_URL <- "https://vitimeteo.ch/api/"
VITIMETEO_AUTH <- authenticate(Sys.getenv("VITIMETEO_USER"), Sys.getenv("VITIMETEO_PASSWORD"), type = 'basic')

METEOSWISS_BASE_URL <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/"
METEOSWISS_STATIONS_URL <- paste0(METEOSWISS_BASE_URL, "ogd-smn_meta_stations.csv")
METEOSWISS_DATAINVENTORY <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_datainventory.csv"
METEOSWISS_PARAMETERS_URL <- paste0(METEOSWISS_BASE_URL, "ogd-smn_meta_parameters.csv")

# Target time period
Start_date <- as.Date("1995-01-01")
End_date <- as.Date("2024-12-31")

# Buffer distance around convex hull (30km in meters)
BUFFER_DISTANCE_M <- 30000

source("match_API_names.R") ## creates the object station_mapping which contains the indices of the agrometeo stations of interest
source("meteoswiss_download_funs.R") ## loads MeteoSwiss download functions

cat("=============================================================================\n")
cat("METEOSWISS HISTORICAL WEATHER DATA DOWNLOAD AND CLIMATIC NORMS DETERMINATION\n")
cat("=============================================================================\n")
cat("Target period:", as.character(Start_date), "to", as.character(End_date), "\n")
cat("Buffer distance:", BUFFER_DISTANCE_M/1000, "km\n")



# =============================================================================
# STEP 1: EXTRACT VITIMETEO STATION COORDINATES 
# =============================================================================

cat("STEP 1: Extracting Vitimeteo station coordinates...\n")
cat("-----------------------------------------------------\n")

# Load existing station mapping
cat("Loading Vitimeteo station mapping...\n")

vitimeteo_coordinates <- data.frame(
  station_id = numeric(),
  station_name = character(),
  latitude = numeric(),
  longitude = numeric(),
  altitude = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(station_mapping)) {
  station_id <- station_mapping$API_ID[i]
  station_name <- station_mapping$API_Station[i]
  
  cat("  Processing station", i, "/", nrow(station_mapping), ":", station_name, "(ID:", station_id, ")\n")
  
  # Get station details
  station_url <- paste0(VITIMETEO_URL, "stations/", station_id,".json", sep = "")
  tryCatch({
    response <- GET(station_url, VITIMETEO_AUTH)
    
    if (response$status_code == 200) {
      station_data <- content(response)
      station_info <- station_data$results[[1]]
      latitude <- station_info$lat_dec
      longitude <- station_info$long_dec
      altitude <- station_info$altitude
      
      if (!is.null(latitude) && !is.null(longitude)) {
        vitimeteo_coordinates <- rbind(vitimeteo_coordinates, data.frame(
          station_id = station_id,
          station_name = station_name,
          latitude = latitude,
          longitude = longitude,
          altitude = ifelse(is.null(altitude), NA, altitude),
          stringsAsFactors = FALSE
        ))
        cat("    ✓ Coordinates:", latitude, ",", longitude, "\n")
      } else {
        cat("    ✗ No coordinates found\n")
      }
    } else {
      cat("    ✗ HTTP error:", response$status_code, "\n")
    }
  }, error = function(e) {
    cat("    ✗ Error:", e$message, "\n")
  })
  
  # Small delay to be respectful to API
  Sys.sleep(0.1)
}

cat("\nSuccessfully extracted coordinates for", nrow(vitimeteo_coordinates), "stations\n")


# =============================================================================
# STEP 2: CREATE CONVEX HULL AND BUFFER
# =============================================================================

cat("\nSTEP 2: Creating convex hull with buffer...\n")
cat("-----------------------------------------------------\n")

# Create spatial points from Vitimeteo coordinates
vitimeteo_sf <- st_as_sf(vitimeteo_coordinates, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)  # WGS84

# Create convex hull
convex_hull <- st_convex_hull(st_union(vitimeteo_sf))

# Get more detailed Swiss boundaries using rnaturalearth
CH_sf <- ne_countries(scale = "large", country = "Switzerland", returnclass = "sf")

# Also get Swiss cantons for more detail
CH_cantons <- ne_states(country = "Switzerland", returnclass = "sf")

# Transform to Swiss coordinate system for accurate buffering
swiss_crs <- 2056  # CH1903+ / LV95
convex_hull_swiss <- st_transform(convex_hull, swiss_crs)

# Add buffer
buffered_area <- st_buffer(convex_hull_swiss, dist = BUFFER_DISTANCE_M)

# Transform back to WGS84 for coordinate filtering
buffered_area_wgs84 <- st_transform(buffered_area, 4326)

# Create the improved map with station points
map_plot <- ggplot() +
  # Swiss country boundary
  geom_sf(data = CH_sf, fill = "white", color = "black", size = 0.8) +
  # Swiss cantons for more detail
  geom_sf(data = CH_cantons, fill = NA, color = "gray60", size = 0.3) +
  # Study area boundaries
  geom_sf(data = convex_hull, fill = "lightblue", color = "blue", alpha = 0.3, size = 1) +
  geom_sf(data = buffered_area_wgs84, fill = "darkblue", alpha = 0.2, size = 1) +
  # Vitimeteo stations
  geom_sf(data = vitimeteo_sf, color = "red", size = 3, shape = 16) +
  # Add labels and styling
  labs(
    title = "Weather Stations in Swiss Study Area",
    subtitle = paste0("Study region with ", BUFFER_DISTANCE_M/1000, "km buffer around Vitimeteo stations"),
    caption = "Data sources: MeteoSwiss OGD, Vitimeteo API"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, color = "gray60"),
    panel.grid = element_line(color = "gray90", size = 0.3)
  )

# Add legend for station types
map_plot <- map_plot +
  annotate("point", x = Inf, y = Inf, color = "red", size = 3) +
  annotate("text", x = Inf, y = Inf, label = "Vitimeteo Stations", 
           size = 3.5, color = "red")


# =============================================================================
# STEP 3: DOWNLOAD METEOSWISS METADATA
# =============================================================================

cat("\nSTEP 3: Downloading MeteoSwiss metadata...\n")
cat("-----------------------------------------------------\n")

# Download station metadata
cat("Downloading MeteoSwiss station metadata...\n")

# Temporarily narrow no_proxy so data.geo.admin.ch goes through the proxy
old_no_proxy <- Sys.getenv("no_proxy")
Sys.setenv(no_proxy = "localhost,127.0.0.1,0.0.0.0,.evdad.admin.ch,.agsad.admin.ch,10.183.0.0/16,10.0.0.0/8")
Sys.setenv(NO_PROXY = "localhost,127.0.0.1,0.0.0.0,.evdad.admin.ch,.agsad.admin.ch,10.183.0.0/16,10.0.0.0/8")

meteoswiss_stations <- read_delim(METEOSWISS_STATIONS_URL, delim = ";", locale = locale(encoding = "UTF-8"))
meteoswiss_datainventory <- read_delim(METEOSWISS_DATAINVENTORY, delim = ";", locale = locale(encoding = "UTF-8"))
# Download parameter metadata  
meteoswiss_parameters <- read_delim(METEOSWISS_PARAMETERS_URL, delim = ";", locale = locale(encoding = "UTF-8"))

# Restore original no_proxy
Sys.setenv(no_proxy = old_no_proxy)
Sys.setenv(NO_PROXY = old_no_proxy)

# Filter for monthly resolution parameters
monthly_params <- meteoswiss_parameters |>
  pull(parameter_description_en) |> 
  grep(pattern = "monthly"  , x =_, ignore.case = T, value = T) 

target_desc <- grep(pattern = "Precipitation; monthly total|Air temperature 2 m above ground; monthly mean|Relative air humidity 2 m above ground; monthly mean|Atmospheric pressure at barometric|Global radiation; monthly mean|Reference evaporation from FAO; monthly total", x = monthly_params, value = T)



desc <- data.frame(target_desc = c("Precipitation; monthly total", "Air temperature 2 m above ground; monthly mean", 
                                   "Relative air humidity 2 m above ground; monthly mean","Atmospheric pressure at barometric altitude (QFE); monthly mean",
 "Global radiation; monthly mean", "Reference evaporation from FAO; monthly total"),
short_desc =c("Precipitation", "Temperature", "Humidity","Pressure", "Radiation", "Evapotranspiration" )) 

target_params <- meteoswiss_parameters |> filter(parameter_description_en %in% target_desc) |> select(parameter_shortname, parameter_description_en)
target_params <- left_join(target_params, desc, by = c("parameter_description_en" = "target_desc"))

## consider only stations with all the interesting variables
station_available_params <- meteoswiss_datainventory |> 
  filter(parameter_shortname %in% target_params$parameter_shortname) |> 
  select(station_abbr, parameter_shortname) |> 
  mutate(Dummy = 1) |> 
  pivot_wider(names_from = parameter_shortname, values_from = Dummy) |> 
  drop_na() |> # only keeps the stations with all the variables (NA is present at least once if there is a missing variable when pivoting wider)
  pull(station_abbr) |> 
  unique()
meteoswiss_stations <- meteoswiss_stations |> filter(station_abbr %in% station_available_params)
# =============================================================================
# STEP 4: FILTER METEOSWISS STATIONS BY GEOGRAPHIC AREA
# =============================================================================

cat("\nSTEP 4: Keeping MeteoSwiss stations in the area of interest...\n")
cat("-----------------------------------------------------\n")

# Convert to spatial data for precise filtering
  stations_sf <- st_as_sf(meteoswiss_stations,
                          coords = c("station_coordinates_wgs84_lon", "station_coordinates_wgs84_lat"),
                          crs = 4326)
  
  # Find stations within buffered area
  stations_within <- st_within(stations_sf, buffered_area_wgs84, sparse = FALSE) |> c()
  meteoswiss_stations <- meteoswiss_stations |> 
    filter(stations_within) |> 
    filter(station_height_masl < max(vitimeteo_coordinates$altitude))

# Create sf object for filtered MeteoSwiss stations
meteoswiss_stations_sf <- st_as_sf(meteoswiss_stations,
                                   coords = c("station_coordinates_wgs84_lon", "station_coordinates_wgs84_lat"),
                                   crs = 4326)

cat("Found", nrow(meteoswiss_stations), "MeteoSwiss stations in study area\n")
cat("Available", nrow(vitimeteo_coordinates), "Vitimeteo stations\n")

# =============================================================================
# COMPLETE MAP: ALL STATIONS
# =============================================================================

# Create comprehensive map with both station types
complete_map <- ggplot() +
  # Swiss country boundary
  geom_sf(data = CH_sf, fill = "white", color = "black", size = 0.8) +
  # Swiss cantons for more detail
  geom_sf(data = CH_cantons, fill = NA, color = "gray60", size = 0.3) +
  # Study area boundaries
  geom_sf(data = convex_hull, fill = "lightblue", color = "blue", alpha = 0.3, size = 1) +
  geom_sf(data = buffered_area_wgs84, fill = NA, color = "darkblue", alpha = 0.8, size = 1.2, linetype = "dashed") +
  # MeteoSwiss stations
  geom_sf(data = meteoswiss_stations_sf, color = "darkgreen", size = 2.5, shape = 17) +  # triangles
  # Vitimeteo stations
  geom_sf(data = vitimeteo_sf, color = "red", size = 3, shape = 16) +  # circles
  # Add labels and styling
  labs(
    title = "Weather Stations",
    subtitle = paste0("Study region with ", BUFFER_DISTANCE_M/1000, "km buffer around Agrometeo stations\n with altitude < 900m"),
    caption = "Data sources: MeteoSwiss, Agrometeo/Vitimeteo (API)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 9, color = "gray60"),
    panel.grid = element_line(color = "gray90", size = 0.3),
    legend.position = "bottom"
  ) +
  # Set coordinate limits to focus on study area
  coord_sf(
    xlim = c(st_bbox(buffered_area_wgs84)[1] - 0.5, st_bbox(buffered_area_wgs84)[3] + 0.5),
    ylim = c(st_bbox(buffered_area_wgs84)[2] - 0.3, st_bbox(buffered_area_wgs84)[4] + 0.3)
  )

# Add manual legend
complete_map +
  # Legend items positioned in bottom right
  annotate("point", x = st_bbox(buffered_area_wgs84)[1], y = st_bbox(buffered_area_wgs84)[4] + 0.1, 
           color = "red", size = 3, shape = 16) +
  annotate("text", x = st_bbox(buffered_area_wgs84)[1]+ 0.05, y = st_bbox(buffered_area_wgs84)[4] + 0.1, 
           label = "Agrometeo", hjust = 0, size = 3, color = "red") +
  annotate("point", x = st_bbox(buffered_area_wgs84)[1], y = st_bbox(buffered_area_wgs84)[4] , 
           color = "darkgreen", size = 2.5, shape = 17) +
  annotate("text", x = st_bbox(buffered_area_wgs84)[1]+ 0.05, y = st_bbox(buffered_area_wgs84)[4], 
           label = "MeteoSwiss", hjust = 0, size = 3, color = "darkgreen")

# Optional: Save the map
ggsave("plots/weather_stations_map.png", width = 12, height = 8, dpi = 300)


# =============================================================================
# STEP 5: DOWNLOAD HISTORICAL DATA
# =============================================================================

cat("\nSTEP 5: Downloading historical data...\n")
cat("-----------------------------------------------------\n")

all_data_list <- list()
old_np <- fix_proxy()

for (i in 1:nrow(meteoswiss_stations)) {
  station <- meteoswiss_stations[i, ]
  station_abbr <- meteoswiss_stations$station_abbr[i]
  
  cat("Station", i, "/", nrow(meteoswiss_stations), "\n")
  all_data_list[[i]] <- get_meteoswiss(
    station     = station_abbr,
    variables   = target_params$parameter_shortname,
    start       = "1995",    # -> interpreted as 2010-01-01
    end         = "2024"     # -> interpreted as 2020-12-31
  )
  
  cat("\n")
}
restore_proxy(old_np)

for(i in 1:nrow(meteoswiss_stations)){
  all_data_list[[i]] <- cbind.data.frame(all_data_list[[i]], 
                                         stat_abbr = meteoswiss_stations$station_abbr[i])
}

# =============================================================================
# STEP 6: PROCESS AND EXPORT DATA
# =============================================================================

cat("STEP 6: Processing and exporting data...\n")
cat("-----------------------------------------------------\n")


# Combine all data
combined_data <- do.call(rbind, all_data_list)
name_var <- target_params$parameter_shortname
names(name_var) <- target_params$short_desc
combined_data <- combined_data |> rename(all_of(name_var))
combined_data <- combined_data |> 
  left_join( y = meteoswiss_stations |> 
               select(station_abbr, station_height_masl, station_coordinates_wgs84_lat, station_coordinates_wgs84_lon, station_name) |>
               rename(altitude = station_height_masl, latitude = station_coordinates_wgs84_lat, longitude = station_coordinates_wgs84_lon), 
             by = c("stat_abbr" = "station_abbr"))

## creating new variables

# =============================================================================
# EXPORT TO CSV
# =============================================================================

output_file <- paste0("data/meteoswiss_weather_data_", lubridate::year(Start_date),"_", lubridate::year(End_date), ".csv")
write_csv(combined_data, output_file)

# read_csv(file = "data/meteoswiss_weather_data_daily_2001_2022.csv")
