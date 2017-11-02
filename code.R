#### Pre-amble ####

#### Libraries ####
library(eurostat)
library(countrycode)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)
library(maptools)
library(rgeos)
library(stringr)
library(mapproj)


#### Functions ####

#### Reading data ####
clean_eurostat_cache() # Necessary (?) for using eurostat package

data_energy <- get_eurostat("nrg_ind_335a", time_format = "raw") # Reading data on renewable electricity prod.
data_energy <- filter(data_energy, indic_en == 119820)

data("eu_countries") # Data for filtering EU 28 countries
data_energy <- filter(data_energy, geo %in% eu_countries$code) # Filtering EU 28 countries

data_energy$cnt_name <- substr(data_energy$geo, 1, 2) # 2 Symbol country names for selecting country names
data_energy[data_energy$cnt_name == "EL", "cnt_name"] <- "GR" # Renaming Greece country code
data_energy[data_energy$cnt_name == "UK", "cnt_name"] <- "GB" # Renaming UK country code

data_energy$cnt_full_name <- countrycode(data_energy$cnt_name, "iso2c", "country.name") # Adding full country names
data_energy_map <- filter(data_energy, time == 2015)

data_map2 <- merge_eurostat_geodata(data_energy_map, all_regions = T, resolution = 20, output_class = "spdf") # Retrieving data for mapping

data_map <- merge_eurostat_geodata(data_energy, all_regions = T, resolution = 20) # Retrieving data for mapping
data_map <- filter(data_map, time == 2015) # Filtering to plot only one year

### First attempt at making the map
energy_map <- ggplot(data = data_map, aes(long, lat, group = group)) + 
  geom_polygon(data = data_map, aes(long, lat),fill = NA, colour= "white", size = 1) + 
  coord_map(project="orthographic", xlim=c(-22,34), ylim=c(35,70)) + 
  geom_polygon(aes(fill = values),colour="dim grey",size=.2) + theme_bw()

energy_map

test_map <-ggplot(data = data_map, aes(long, lat, group = group)) + 
  geom_polygon(data = data_map, aes(long, lat), colour= "white", size = 1) + 
  coord_map(project="orthographic", xlim=c(-22,34), ylim=c(35,70))

test_map
