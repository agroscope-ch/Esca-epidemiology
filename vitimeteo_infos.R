# Load required libraries
library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(stringr)

# API Configuration
url <- "https://vitimeteo.ch/api/"
auth <- authenticate(Sys.getenv("VITIMETEO_USER"), Sys.getenv("VITIMETEO_PASSWORD"), type = 'basic')

stations_endpoint <- paste0(url, 'stations.json')
station_API <- content(GET(stations_endpoint, auth))

stat_list <- station_API$results |>
  unlist() |>
  matrix(byrow = TRUE, ncol = 2) |>
  data.frame()
names(stat_list) <- c("ID", "API_name")

vitimeteo_coordinates <- data.frame(
  station_id = numeric(),
  station_name = character(),
  latitude = numeric(),
  longitude = numeric(),
  altitude = numeric(),
  puttingIntoService = character(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(stat_list)) {
  station_id <- stat_list$ID[i]
  station_name <- stat_list$API_name[i]
  # Get station details
  station_url <- paste0(url, "stations/", station_id,".json", sep = "")
  # tryCatch({
  station_data <- content(GET(station_url, auth))
      station_info <- station_data$results[[1]]
      latitude <- station_info$lat_dec
      longitude <- station_info$long_dec
      altitude <- station_info$altitude
      puttingIntoService <- station_info$puttingIntoService
      vitimeteo_coordinates <- rbind(vitimeteo_coordinates, data.frame(
        station_id = station_id,
        station_name = station_name,
        latitude = latitude,
        longitude = longitude,
        altitude = ifelse(is.null(altitude), NA, altitude),
        puttingIntoService = puttingIntoService,
        stringsAsFactors = FALSE
      ))
  if(i %%10 == 0 ) print(i)
  # Small delay to be respectful to API
  Sys.sleep(0.1)
}
write_csv(vitimeteo_coordinates, "data/vitimeteo_stations_info.csv")
