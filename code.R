#### Pre-amble ####

#### Libraries ####
library(eurostat)
library(countrycode)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#### Functions ####

#### Reading data ####
clean_eurostat_cache() # Necessary (?) for using eurostat package

data_energy <- get_eurostat("nrg_ind_335a", time_format = "raw") # Reading data on renewable electricity prod.

data("eu_countries") # Data for filtering EU 28 countries
data_energy <- filter(data_energy, geo %in% eu_countries$code) # Filtering EU 28 countries

data_energy$cnt_name <- substr(data_energy$geo, 1, 2) # 2 Symbol country names for selecting country names
data_energy[data_energy$cnt_name == "EL", "cnt_name"] <- "GR" # Renaming Greece country code
data_energy[data_energy$cnt_name == "UK", "cnt_name"] <- "GB" # Renaming UK country code

data_energy$cnt_full_name <- countrycode(data_energy$cnt_name, "iso2c", "country.name") # Adding full country names