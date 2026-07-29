# Load required libraries
library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(stringr)

# API Configuration
url <- "https://vitimeteo.ch/api/"
auth <- authenticate(Sys.getenv("VITIMETEO_USER"), Sys.getenv("VITIMETEO_PASSWORD"), type = 'basic')

# Function to extract individual station names from Excel entries
extract_station_names <- function(station_string) {
  if (is.na(station_string) || station_string == "") return(character(0))
  
  # Extract station names with years: "Station Name (Year)"
  stations_with_years <- str_extract_all(station_string, "[^()]+?\\s*\\([0-9]{4}\\)")[[1]]
  station_names <- str_replace_all(stations_with_years, "\\s*\\([0-9]{4}\\)", "")
  return(trimws(station_names))
}

# Function to normalize station names to API format
normalize_to_api <- function(name) {
  normalized <- toupper(name)
  normalized <- str_replace_all(normalized, "\\s+", "-")
  # Add any other accent removal if needed
  return(normalized)
}

# Read Excel file
station_data <- read_excel("ESCA brut 2001-2022.xlsx", sheet = "pas de changement", skip = 1)
excel_entries <- station_data$`Stations Agrometeo`[!is.na(station_data$`Stations Agrometeo`)]

# Extract all individual station names
all_stations <- data.frame(
  excel_name = character(0),
  normalized_name = character(0),
  stringsAsFactors = FALSE
)

for (entry in unique(excel_entries)) {
  individual_stations <- extract_station_names(entry)
  for (station in individual_stations) {
    normalized <- normalize_to_api(station)
    all_stations <- rbind(all_stations, data.frame(
      excel_name = station,
      normalized_name = normalized,
      stringsAsFactors = FALSE
    ))
  }
}

# Remove duplicates
all_stations <- all_stations[!duplicated(all_stations), ]

# Get API stations
stations_endpoint <- paste0(url, 'stations.json')
station_API <- content(GET(stations_endpoint, auth))

stat_list <- station_API$results |> 
  unlist() |> 
  matrix(byrow = TRUE, ncol = 2) |> 
  data.frame()
names(stat_list) <- c("ID", "API_name")
stat_list$ID <- as.numeric(stat_list$ID)

# Match and create final mapping
station_mapping <- all_stations |> 
  left_join(stat_list, by = c("normalized_name" = "API_name")) |>
  filter(!is.na(ID)) |>
  select(Excel_Station = excel_name, 
         API_Station = normalized_name, 
         API_ID = ID) |>
  arrange(API_ID)

print(station_mapping)
