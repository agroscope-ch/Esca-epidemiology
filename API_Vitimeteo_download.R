library(tidyverse)
library(readxl)
library(httr)

source(file = "match_API_names.R")

url <- 'https://vitimeteo.ch/api/'
auth <- authenticate("ProbaStat", "math234d", type = 'basic')
# used for weatherdata calls:
# 1: temperature
# 4: relative humidity
# 6: rainfall
sens_indices <- c(1,4,6, 10 )
sens_names <- c("Temperature", "RelHum", "Rainfall", "EPT")
grp_options =  c("\'avg\'", "\'avg\'", "\'sum\'", "\'sum\'")
# sen_list = paste0("[",paste(sens_indices, grp_options, sep ="") paste0(sens_indices, collapse = ","),"]")
sens_call <- paste0(paste("[", paste(sens_indices, grp_options, sep = ","), "]", sep = ""), collapse = ",")


list_of_stations <- station_mapping$API_ID

start_date = "2003-01-01"
end_date = "2022-12-31"
dates_call <- paste0("startdate=",start_date , "&enddate=", end_date, sep = "")

df_weather <- data.frame()
for(idStat in list_of_stations)
{
  singlestation <- paste(url, 'stations/', as.character(idStat), sep='')
  weather <- paste(singlestation, "/weatherdata.json?&", sep = '')
  get_weather <- paste0(weather, 
                        dates_call, "&groupby=hour&sen_list=", "[",sens_call,"]", "&datastructure=chart&cflag=0")
  weatherdata <- content(GET(get_weather, auth))
  if(weatherdata$chart[[1]][[1]]$point |> length() == 0) next
  df_weather_stat <- lapply(1:length(sens_indices), 
                       function(z)
                         weatherdata$chart[[z]][[1]]$point |> 
                         unlist() |> 
                         matrix(nc = 2, byrow = T) |>
                         cbind(sens_names[z], idStat)
                       ) |> do.call(rbind,args = _)
  names(df_weather_stat) <- c("date", "value", "sensor", "Station_ID")
  df_weather <- rbind.data.frame(df_weather, df_weather_stat)
  print(idStat)
}

names(df_weather) <- c("date", "value", "sensor", "Station_ID")
df_weather$date <- strptime(df_weather$date, format = "%Y-%m-%d %H:%M:%S")
df_weather <- df_weather |> 
  mutate(value = as.numeric(value), sensor = factor(sensor), Station_ID = factor(Station_ID)) |> 
  left_join(y = station_mapping |> select(-Excel_Station) |> mutate(API_ID = factor(API_ID)), 
            by = c("Station_ID" = "API_ID"))

write_csv2(df_weather, file = "data/weather_data_Agrometeo_Esca_plots.csv")

# df_weather |> group_by(API_Station, sensor ) |> summarise(MinDate = min(date), MaxDate = max(date)) |> View()
