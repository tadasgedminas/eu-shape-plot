#### Pre-amble ####
rm(list = ls())
setwd("C:/Users/gedmi/Desktop/Personal/Projects/EU - Size Map/eu-shape-plot")

## Small test edit ##

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
library(ggmap)


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

### First attempt at making the map using eurostat package
# energy_map <- ggplot(data = data_map, aes(long, lat, group = group)) + 
#   geom_polygon(data = data_map, aes(long, lat),fill = NA, colour= "white", size = 1) + 
#   coord_map(project="orthographic", xlim=c(-22,34), ylim=c(35,70)) + 
#   geom_polygon(aes(fill = values),colour="dim grey",size=.2) + theme_bw()
# 
# energy_map

## Read data related to land mapping coast-lines and country-borders
wmap <- readOGR("Map data/ne_50m_land.shp", layer="ne_50m_land")
countries <- readOGR("Map data/ne_50m_admin_0_countries.shp", layer="ne_50m_admin_0_countries")

## Adjusting spatial data to more useful format + adjusting for projection
wmap_laea<- spTransform(wmap, CRS("+proj=laea"))
countries_laea<-spTransform(countries, CRS("+proj=laea"))

## Selecting data on country shape polygons
cnt_map_data <- countries@polygons

## Selecting additonal identification data on each country
side_data <- countries@data

## Selecting all countries that are in the sample
eu <- unique(data_energy$cnt_full_name)

## Additional ID for each country
side_data$ider <- 1:nrow(side_data)

## Selecting identifiers for relevant country polygons
cnt_ids <- side_data %>%
  filter(geounit %in% eu) %>%
  select(ider, geounit)

## Renaming columns for merging
colnames(data_energy_map)[ncol(data_energy_map)] <- "geounit"

## Adding to country identifiers actual data
cnt_ids_comb <- merge(cnt_ids, data_energy_map, by = "geounit")
cnt_ids_comb$id <- cnt_ids_comb$ider - 1
cnt_ids_comb <- select(cnt_ids_comb, -geounit, -ider)


## Loop for selecting relevant 
cnt_ids$lat <- NA
cnt_ids$long <- NA

for (i in 1:nrow(cnt_ids)){
  cnt_ids[i, "lat"] <- cnt_map_data[[cnt_ids[i, "ider"]]]@labpt[2]
  cnt_ids[i, "long"] <- cnt_map_data[[cnt_ids[i, "ider"]]]@labpt[1]
}

## Loop for celecting centroids of each country polygon
placeh <- list()
n <- 1

for (j in 1:nrow(cnt_ids)) {
  placeh2 <- cnt_map_data[[cnt_ids[j, "ider"]]]@Polygons
  place_order <- cnt_map_data[[cnt_ids[j, "ider"]]]@plotOrder
  for (z in 1:length(placeh2)){
    placeh[[n]] <- c(placeh2[[place_order[z]]]@labpt[1], placeh2[[place_order[z]]]@labpt[2],cnt_ids[j, "ider"] - 1, z)
    n <- n+1
  }
}

## Combining the selected data to a data.frame
poly_centers <- as.data.frame(do.call(rbind, placeh))
colnames(poly_centers) <- c("c.long", "c.lat", "id", "piece")

## Applying transformation for translating "lat" and "long" to points on a plot
loc <- poly_centers
coordinates(loc) <- c("c.long", "c.lat")
proj4string(loc) <- CRS("+proj=longlat")
loc_laea <-spTransform(loc, CRS("+proj=laea"))

## Tranforming spatial object of land and country data to a data frame
wmap_laea_df<-fortify(wmap_laea)
countries_laea_df<-fortify(countries_laea)
loc_laea_df <- data.frame(loc_laea)

## Combining data values and adjusting polygons points for re-drawing smaller country polygons
comb <- merge(countries_laea_df, loc_laea_df, by = c("piece", "id"))
comb <- arrange(comb, id, piece, order)
comb <- merge(comb, cnt_ids_comb, by = "id")

## New coordinates of polygons based on data values. Note the adjustment necessary with sqrt since we are shrinking a 2-dimensional object
comb <- mutate(comb, n.long = (long - c.long)*(sqrt(values/100)) + c.long,
               n.lat = (lat - c.lat)*(sqrt(values/100)) + c.lat)

## Theme of map
theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_rect(fill = 'white', colour = NA),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_text(size=22)))

## Defining the values for finding an appropriate POV of the map
xmin<- -730000
xmax<- 3105000
ymin<- 3940000
ymax<- 7000000
buff<-75000

## Plotting the map
map_eu <- ggplot() +
  geom_polygon(data=wmap_laea_df, aes(long,lat,group=group), fill="white")+
  geom_polygon(data=countries_laea_df, aes(long,lat, group=group), colour="white", fill = "grey") +
  geom_polygon(data=comb, aes(n.long,n.lat, group=group), fill="steelblue") +
  geom_path(data=countries_laea_df, aes(long,lat, group=group), colour="black") +
  scale_size(range=c(1,7), guide = "legend",labs(size="Count")) +
  coord_cartesian(xlim = c((xmin-buff),(xmax+buff)), ylim = c((ymin-buff),(ymax+buff))) +
  theme(aspect.ratio=1)+
  theme_opts

map_eu