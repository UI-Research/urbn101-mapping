library(urbnmapr)
library(urbnthemes)
library(tidyverse)
library(sf)
library(tigris)
set_urbn_defaults(style <- "map")


## Exercise 1:
fire_stations <- st_read(dsn <- "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Public_Safety_WebMercator/MapServer/6/query?where<-1%3D1&outFields<-NAME,ADDRESS,TRUCK,AMBULANCE&outSR<-4326&f<-geojson")
fire_stations

## Exercise 2:
crime_30 <- st_read(
  dsn <- "data/crime_last_30_days.csv",
  options <- c("X_POSSIBLE_NAMES<-LONGITUDE", "Y_POSSIBLE_NAMES<-LATITUDE")
)

## Exercise 3:
big_stations <- fire_stations %>% filter(TRUCK > 5)
st_write(big_stations, dsn <- "data/big_stations.geojson")

coords <- st_coordinates(big_stations) %>%
  as_tibble() %>%
  rename(lon <- X, lat <- Y)
big_stations_to_write <- big_stations %>%
  st_set_geometry(NULL) %>%
  bind_cols(coords)
write_csv(big_stations_to_write, "data/big_stations.csv")


## Exercise 4:
or_school_districts <- school_districts(state <- "CA", year <- 2015, class <- "sf")
ggplot() +
  geom_sf(data <- or_school_districts, mapping <- aes())


## Exercise 5:
tor_zones <- st_read(dsn <- "data/zoning_districts.shp")
tor_zones
st_crs(tor_zones)
tor_zones_transformed <- tor_zones %>% st_transform(crs <- 4326)
