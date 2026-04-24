## Conduct long-term analyses

esca_data <- read_csv("data/Esca_CH_2001-2022.csv", 
                      locale = locale(encoding = "latin1")) |> 
  mutate(SumSymptApo = percentage_apoplexy +percentage_symptoms)

MeteoSwiss_df <- read_csv("data/meteoswiss_weather_data_1995_2024.csv")|> 
  mutate(Year = lubridate::year(datetime),
         nbMonth = lubridate::month(datetime),
         Month = lubridate::month(datetime, label = T, abbr = F)) |> 
  filter(nbMonth %in% c(3:10)) |> 
  select(-datetime, -nbMonth) |> 
  pivot_wider(values_from = c(Temperature, Pressure, Precipitation, Radiation, Humidity, Evapotranspiration), names_from = Month)

MeteoSwiss_associations <- read_csv("data/plot_meteoswiss_associations.csv") |>
  select(plot, meteoswiss_station_abbr) |> 
  group_by(plot) |> 
  summarise(meteoswiss_station_abbr = meteoswiss_station_abbr[1]) # It avoids selecting several time the same MeteoCH station for a plot whose coordinates have slightly changed over time

library(readxl)
Esca_et_RU_des_sols_Vivian <- read_excel("data/Esca et RU des sols Vivian.xlsx")

Esca_et_RU_des_sols_Vivian$RU_cat <- Esca_et_RU_des_sols_Vivian$`RU (mm)` |> 
  as.numeric() |> 
  cut(breaks = c(0,90, 130, 160, 200)) 

levels_RU_cat <- levels(Esca_et_RU_des_sols_Vivian$RU_cat)

Esca_et_RU_des_sols_Vivian$RU_cat <- Esca_et_RU_des_sols_Vivian$RU_cat |> 
  as.character()
Esca_et_RU_des_sols_Vivian$RU_cat[is.na(Esca_et_RU_des_sols_Vivian$RU_cat)] <- 
  Esca_et_RU_des_sols_Vivian$`RU (mm)`[is.na(Esca_et_RU_des_sols_Vivian$RU_cat)]

## Arbitrary attribution
Esca_et_RU_des_sols_Vivian$RU_cat[Esca_et_RU_des_sols_Vivian$`RU (mm)` == "150-200"] <- "(90,130]"

Esca_et_RU_des_sols_Vivian$RU_cat[Esca_et_RU_des_sols_Vivian$`RU (mm)` == "100-150"] <- "(160,200]"

Esca_et_RU_des_sols_Vivian$RU_cat[Esca_et_RU_des_sols_Vivian$`RU (mm)` == "<100"] <- "(0,90]"
Esca_et_RU_des_sols_Vivian$RU_cat <- factor(Esca_et_RU_des_sols_Vivian$RU_cat, levels =levels_RU_cat, ordered = T )
# Esca_et_RU_des_sols_Vivian$RU_cat[Esca_et_RU_des_sols_Vivian$`RU (mm)` == "150-200"] <- "(160,200]"

# esca_data <- 
  
esca_stable <- esca_data |> 
  left_join(y = Esca_et_RU_des_sols_Vivian |> select(plot, `RU (mm)`, RU_cat), by = "plot") |> 
  left_join(y = MeteoSwiss_associations, by = "plot") |> 
    select(-c(contains("Apoplexy_"), contains("Eutypa"), contains("Symptoms_"), contains("oles"))) |> 
    filter(group_year == "2004-2022" & Year != 2001) |>
  filter(variety == "chasselas") |> 
  select(-age, training_system) |> 
    left_join(y = MeteoSwiss_df, by =c("meteoswiss_station_abbr" = "stat_abbr", "Year" )) 


library(FORD)

Y = esca_stable$SumSymptApo |> as.matrix()
X = esca_stable |> select(contains(c("Temper", "Precip", "Press", "Evapo", "Radia", "Humid"))) |> as.matrix()
ford(Y = Y,  X = X, , stop = T)

# makeRegrTask(data = esca_stable, target = "SumSymptApo")  # Requires mlr package



