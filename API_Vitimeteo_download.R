library(tidyverse)
library(readxl)
library(httr)
library(purrr)  # Added for keep() function in idDataType mapping

# source(file = "match_API_names.R")
month_flag = FALSE
url <- 'https://vitimeteo.ch/api/'
auth <- authenticate(Sys.getenv("VITIMETEO_USER"), Sys.getenv("VITIMETEO_PASSWORD"), type = 'basic')
# Sensor configuration - ROBUST MAPPING USING idDataType!
sens_indices <- c(1,4,6,10,11)
sens_names <- c("Temperature", "RelHum", "Rainfall", "EPT", "Radiation") 
id_datatype_mapping <- list(
  "1" = "Temperature",
  "4" = "RelHum", 
  "6" = "Rainfall",
  "10" = "EPT",
  "11" = "Radiation"
)

sens_call <- paste0("[", paste(sens_indices, collapse = ","), "]")


# list_of_stations <- station_mapping$API_ID
stations_associations <- read_csv("data/plot_station_associations.csv")
list_of_stations <- stations_associations$station_id |> unique() |> sort()
# VALIDATION: Test idDataType mapping with first station
cat("=== VALIDATING idDataType MAPPING ===\n")
test_station <- list_of_stations[1]
test_url <- paste0(url, 'stations/', test_station, '/weatherdata.json?startdate=2024-01-01&enddate=2024-01-01&sen_list=', sens_call, '&datastructure=chart&cflag=0')
test_response <- content(GET(test_url, auth))
if(length(test_response$chart) > 0) {
  for(i in 1:length(test_response$chart)) {
    id_dt <- test_response$chart[[i]][[1]]$idDataType
    name <- test_response$chart[[i]][[1]]$name
    mapped <- id_datatype_mapping[[as.character(id_dt)]]
    cat("Position", i, "- idDataType:", id_dt, "- API Name:", name, "- Mapped:", mapped, "\n")
  }
} else {
  cat("Warning: No test data available for validation\n")
}
cat("=====================================\n")

start_date = "2001-01-01"
end_date = "2022-12-31"
dates_call <- paste0("startdate=",start_date , "&enddate=", end_date, sep = "")

df_weather <- data.frame()
station_count <- 0
total_stations <- length(list_of_stations)

cat("Starting download for", total_stations, "stations...\n")

for(idStat in list_of_stations)
{
  station_count <- station_count + 1
  cat("Processing station", idStat, "(", station_count, "/", total_stations, ")...\n")
  
  singlestation <- paste(url, 'stations/', as.character(idStat), sep='')
  weather <- paste(singlestation, "/weatherdata.json?&", sep = '')
  
  if(month_flag){
    get_weather <- paste0(weather, 
                          dates_call, "&groupby=month&sen_list=", sens_call, "&datastructure=chart&cflag=0")
  } else
  {
    get_weather <- paste0(weather, 
                          dates_call, "&groupby=hour&sen_list=", sens_call, "&datastructure=chart&cflag=0")
  }
  
  # Add error handling
  tryCatch({
    weatherdata <- content(GET(get_weather, auth))
    if(length(weatherdata$chart) == 0 || length(weatherdata$chart[[1]][[1]]$point) == 0) {
      cat("  No data available for station", idStat, "\n")
      next
    }
    
    df_weather_stat <- lapply(1:length(sens_indices), 
                              function(z) {
                                if(z <= length(weatherdata$chart) && 
                                   length(weatherdata$chart[[z]][[1]]$point) > 0) {
                                  # Extract idDataType from API response
                                  id_datatype <- as.character(weatherdata$chart[[z]][[1]]$idDataType)
                                  # Map to English name using idDataType  
                                  english_name <- id_datatype_mapping[[id_datatype]]
                                  if (is.null(english_name)) {
                                    english_name <- paste0("Unknown_", id_datatype)
                                  }
                                  
                                  weatherdata$chart[[z]][[1]]$point |> 
                                    unlist() |> 
                                    matrix(nc = 2, byrow = T) |>
                                    cbind(english_name, idStat)
                                } else {
                                  NULL
                                }
                              }) |> 
      keep(~ !is.null(.)) |> 
      do.call(rbind, args = _)
    
    names(df_weather_stat) <- c("date", "value", "sensor", "Station_ID")
    df_weather <- rbind.data.frame(df_weather, df_weather_stat)
    cat("  Downloaded", nrow(df_weather_stat), "records\n")
    
  }, error = function(e) {
    cat("  Error downloading station", idStat, ":", e$message, "\n")
  })
}

names(df_weather) <- c("date", "value", "sensor", "Station_ID")
if(!month_flag){df_weather$date <- strptime(df_weather$date, format = "%Y-%m-%d %H:%M:%S")}


df_weather <- df_weather |>
  mutate(value = as.numeric(value), sensor = factor(sensor), Station_ID = factor(Station_ID)) |>
  left_join(y = stations_associations |> select(station_id, station_name) |> mutate(station_id = factor(station_id)),
            by = c("Station_ID" = "station_id"))
if(month_flag){
  write_csv2(df_weather, file = "data/weather_data_Agrometeo_Esca_plots_monthly.csv")
} else
{
  write_csv2(df_weather, file = "data/weather_data_Agrometeo_Esca_plots.csv")
}

df_weather |> 
  group_by(API_Station, sensor ) |> 
  summarise(MinDate = min(date), MaxDate = max(date)) |> View()


