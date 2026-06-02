
## ----setup--------------------------------------------------------------------------------------------------------
library(readxl) # to read .xlsx file
library(tidyverse) 
library(stringr) # to manipulate strings
library(lubridate) # to manipulate dates


## ----load data and rename, warning=FALSE--------------------------------------------------------------------------
Esca_df <- read_excel("~/mnt/Data-Work-CH/22_Plant_Production-CH/222.6_Mycologie_protected/Projets de recherche/2_Cultures spéciales/Esca/Papier Esca epidemiologie/ESCA brut 2001-2022.xlsx", skip  = 1, guess_max = 10000)


names(Esca_df)[str_detect(names(Esca_df),pattern = "conduite")] <- "conduite"
names(Esca_df)[11:14] <- sapply(1:4, function(z) paste0("Apoplexie_", z))
names(Esca_df)[15:18] <- sapply(1:4, function(z) paste0("Symptomes_", z))
names(Esca_df)[19:22] <- sapply(1:4, function(z) paste0("Trous_", z))
names(Esca_df)[23:26] <- sapply(1:4, function(z) paste0("Eutypiose_", z))


# We save the percentages which have been set for the years 2001 and 2004
Esca_percentage <- Esca_df |> 
  mutate(Year = lubridate::year(date_observation)) |> 
  filter(Year %in% c(2001, 2004)) |> 
    rename(percentage_apoplexy =`%_apoplexie`, 
         percentage_symptoms = `%_symptome`,
         percentage_eutypa = `%_eutypiose`) |> 
  select(parcelle, commune, Year, contains(match = "percentage_")) |> 
  mutate(parcelle = factor(parcelle), commune = factor(commune), across(contains("percentage"), .fns = as.numeric))

Esca_df <- Esca_df |>   
    mutate(Year = lubridate::year(date_observation)) |> 
    select(- contains(match = "%"))



## ----variable temoin----------------------------------------------------------------------------------------------
EscaTest <- Esca_df


## ----encodage conduite--------------------------------------------------------------------------------------------
Esca_df <- Esca_df |> mutate(
  `conduite` = case_when(
    str_detect(`conduite`, "ordon") ~ "Co",
    str_detect(`conduite`, "bas-cor") ~ "GB-C",
    str_detect(`conduite`, "basse") ~ "GB",
    str_detect(`conduite`, "G haute") ~ "GH",
    str_detect(`conduite`, "G mi") ~ "GM-H",
    str_detect(`conduite`, "Gobelet") ~ "Go",
    str_detect(`conduite`, "Guy") ~ "Gu",
    str_detect(`conduite`, "mi-haute") ~ "MH",
    str_detect(`conduite`, "haute") ~ "H",
    TRUE ~ `conduite` # by default, leaves the value unchanged
  ))
# check that it is working
unique(cbind(Temoin = EscaTest$`conduite`, Modif = Esca_df$`conduite`))


## ----encodage age-------------------------------------------------------------------------------------------------
Esca_df <- Esca_df |> mutate(
  `age` = case_when(
    str_detect(`age`, "moyen") ~ "moyenne",
    str_detect(`age`, "vie") ~ "vieille",
    str_detect(`age`, "19") ~ NA_character_,
    TRUE ~ `age`
  ))
# check that it is working
unique(cbind(Temoin = EscaTest$`age`, Modif = Esca_df$`age`))


## ----reencode cepage----------------------------------------------------------------------------------------------

Esca_df <- Esca_df |> mutate(
  `cepage` = case_when(
    str_detect(`cepage`, "Ch") ~ "CH",
    str_detect(`cepage`, "PN") ~ "PN",
    str_detect(`cepage`, "HR") ~ "G",
    str_detect(`cepage`, "J") ~ "JH",
    TRUE ~ `cepage`
  ))
#  We check in the original object the frequency of weird (uneven) values 
table(EscaTest$`cepage`)
# check
unique(cbind(Temoin = EscaTest$`cepage`, Modif = Esca_df$`cepage`))


## ----reecode regions and coordinates------------------------------------------------------------------------------
# dummy variable to indicate a potential problem;
# used to know the proportion of problematic parcelles
ind.pb <- rep(NA, length(unique(Esca_df $`parcelle`)))
for(i in unique(Esca_df $`parcelle`))
{
  # sometimes, Region is wrongly encoded
  k <- table(Esca_df[Esca_df $`parcelle` == i, c("region")])
  # For each parcelle, we determine if there are different values of Regions encoded
  # and if yes, the ind.pb flag is set to TRUE
  if(length(k) != 1) ind.pb[which(i == unique(Esca_df $`parcelle`))] <- T #
  # For each parcelle, all the Region variable are set to the most frequent value
  Esca_df [Esca_df $`parcelle` == i, "region"] <- names(k)[which.max(k)]
  # Sometimes, the X,Y coordinates are confused. Since we are using the Swiss federal coordinate system,
  # any point in Switzerland has X > Y, so we order it
  k <- Esca_df [Esca_df$`parcelle` == i, c("E","N")] 
  Esca_df [Esca_df $`parcelle` == i, c("E","N")] <- t(apply(k, 1, function(z)sort(z, na.last = T, decreasing = T)))
}
print(paste0("proportion de parcelles problématiques: ", round(length(which(ind.pb))/length(ind.pb), digits =2)) )


## ----encoding type------------------------------------------------------------------------------------------------
Esca_df <- Esca_df |> 
  mutate(across(c(`parcelle`,`region`,commune, `cepage`,`conduite`,  distance_de_plantation),factor)) |> 
  mutate(age = factor(`age`, order = TRUE, levels = c("jeune","moyenne", "vieille")))


## ----variables in percent-----------------------------------------------------------------------------------------
# since there are 400 vine stocks (aka ceps), the percentage is simply tot/400 *100 =tot/4
Esca_df <- Esca_df |> 
  mutate(`percentage_apoplexy` = apply(Esca_df[,str_which(names(Esca_df), "Apoplexie")], 1, function(z){sum(z, na.rm = T)/4})) |> 
  mutate(`percentage_symptoms` = apply(Esca_df[,str_which(names(Esca_df), "Symptome")], 1, function(z){sum(z, na.rm = T)/4})) |>  
  mutate(`percentage_holes` = apply(Esca_df[,str_which(names(Esca_df), "Trous")], 1, function(z){sum(z, na.rm = T)/4})) |> 
  mutate(`percentage_eutypa` = apply(Esca_df[,str_which(names(Esca_df), "Eutyp")], 1, function(z){sum(z, na.rm = T)/4})) 


## Long term observations
## We define two categories, 2004-2022 and 2012-2022 and we isolate plots (domains in this case) for which we have data for the whole period. Note that some plots have observations from 2001 to 2022 and are included in the group 2004-2022.
## WARNING: THOSE PLOTS ARE NOT NECESSARILY IDENTICAL OVER THE YEARS;
## THEY MAY HAVE CHANGED (E.G. PICKING A NEIGHBOURING ONE).
Year_range <-  Esca_df$Year |> unique() |> sort()
parcelle_range <- Esca_df$parcelle |> unique() |> sort() |>as.character() |>  as.numeric()
group_year_lev <- c("2004-2022", "2012-2022")
grp_years <- data.frame(plot =parcelle_range)
for(i in 1:length(parcelle_range))
{
  curr_range_years <- Esca_df |> filter(parcelle == parcelle_range[i]) |> 
    pull(Year) |> 
    unique() |> 
    sort()
  
  if(all(Year_range[Year_range >= 2012] %in% curr_range_years)){
    grp_years$group_year[grp_years$plot == parcelle_range[i]] <- "2012-2022"
  } else
  {
    grp_years$group_year[grp_years$plot == parcelle_range[i]] <- NA
  }
  if(all(Year_range[Year_range >= 2004] %in% curr_range_years)){
    grp_years$group_year[grp_years$plot == parcelle_range[i]] <- "2004-2022"
  }
}
grp_years <- grp_years |> mutate(across(everything(), factor))
group_domains <- left_join(x = Esca_df , y = grp_years, 
                                  by = c("parcelle"= "plot"))



### Among all the plots, some were perfectly identical. Not only they were 
### chosen in the same vineyard, but they were at the very same locations
### This information has been gathered by A.-L. Fabre
Stable_plots <- read_excel("~/mnt/Data-Work-CH/22_Plant_Production-CH/222.6_Mycologie_protected/Projets de recherche/2_Cultures spéciales/Esca/Papier Esca epidemiologie/ESCA brut 2001-2022.xlsx", sheet = 2, skip  = 1, guess_max = 10000)

Stable_plots <- Stable_plots |>
  select(-c(Village, Cépage, `Stations Agrometeo`)) |>
  pivot_longer(cols = -c(parcelle, region, )) |> drop_na()|>
  mutate(value = NULL, parcelle = factor(parcelle)) |>
  mutate(stable_group = factor(case_when(
    name == "2001-2022" ~ "2004-2022", # if we restict our attention to 2001-2022 only, we are left with a very few plots. We then include 2001 plots that are kept over the years in the group 2004-2022
    name == "2004-2022" ~ "2004-2022",
    name == "2001-2004     2012-2022" ~ "2012-2022",
    name == "2006-2022" ~ "2012-2022",
    name == "2012-2022" ~ "2012-2022",
    .default = NA)))

left_join(x = group_domains |> select(parcelle, group_year),
          y = Stable_plots, by = "parcelle") |> select(parcelle, group_year, stable_group) |>
  distinct() |>
  arrange(parcelle) #|>
# View()



write.table(x = Stable_plots, file = "data/Esca_CH_2001-2022_stable_plots.csv", sep = ",", fileEncoding = "latin1", dec = ".", row.names = F, na = "", quote = FALSE )

Esca_df <-
  Esca_df |>
  filter(Year %in% c(2001, 2004)) |>
  select(- contains("percentage")) |>
  left_join(Esca_percentage) |>  # taking care of the 2001-2004 part separately from the rest (only percentages where collected)
  bind_rows(Esca_df |> filter(! Year %in% c(2001, 2004))) |>
  left_join( y= Stable_plots |> select(parcelle, stable_group ) |> distinct(), by = "parcelle") |> 
  left_join( y= group_domains |> select(parcelle, group_year ) |> distinct(), by = "parcelle") |> 
  rename(plot = parcelle,  variety = cepage, training_system = conduite) |> 
  rename_with(~ gsub("^Apoplexie_", "Apoplexy_", .x)) |>
  rename_with(~ gsub("^Symptomes_", "Symptoms_", .x)) |>
  rename_with(~ gsub("^Trous_", "Holes_", .x)) |>
  rename_with(~ gsub("trous_", "holes_", .x)) |>
  rename_with(~ gsub("^Eutypiose_", "Eutypa_", .x)) |>
  mutate(distance_de_plantation = NULL) |> 
  relocate(Year, .after = training_system) |> 
  arrange(Year, plot) |> 
  relocate(E, .before = N)

Esca_df |> 
  write.table(file = "data/Esca_CH_2001-2022.csv", sep = ",", fileEncoding = "latin1", dec = ".", row.names = F, na = "", quote = FALSE )

