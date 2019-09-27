library(tidyverse)
library(sf)
library(urbnmapr)
library(urbnthemes)
library(tigris)

set_urbn_defaults(style = "map")


# Exercise 1: Mapping Review ----------------------------------------------

# Read in data 
fire_stations <- st_read("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Public_Safety_WebMercator/MapServer/6/query?where=1%3D1&outFields=NAME,ADDRESS,TRUCK,AMBULANCE&outSR=4326&f=geojson")

# Use tigris to get census shapes
dc_tracts <- tracts(state = "DC",
                    year = 2017,
                    class = "sf",
                    cb = TRUE)

# Plot!
ggplot() +
  geom_sf(data = dc_tracts, mapping = aes(),
          fill = "grey", color = "black") +
  geom_sf(data = fire_stations, mapping = aes(),
          color = "red")


# Exercise 2: Spatial Joins -----------------------------------------------

# Check CRS
st_crs(fire_stations)
st_crs(dc_tracts)

# Change CRS
dc_tracts <- st_transform(dc_tracts, crs = 6487)
fire_stations <- st_transform(fire_stations, crs = 6487)

# Spatial join
dc_stations <- st_join(dc_tracts, fire_stations, join = st_intersects)
nrow(dc_stations) - nrow(dc_tracts)

# Spatial join the other way!
stations_dc <- st_join(fire_stations, dc_tracts, join = st_covered_by)

# map
dc_stations %>% 
  # create a dummy variable indicating the tract joined with a station
  mutate(station = ifelse(is.na(NAME.y),
                          0,
                          1)) %>% 
  group_by(GEOID) %>% 
  # get down to one observation per tract
  summarize(stations = sum(station)) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = factor(stations))) +
  scale_fill_discrete() +
  labs(fill = "Fire stations per census tract")
# Note: stations is converted to a factor so that a discrete color scheme can be used


# Exercise 3: Buffers -----------------------------------------------------

buffer_distance <- units::set_units(.5, "mile")

buffer_distance <- buffer_distance %>% 
  units::set_units("meters")

buffer_stations <- st_buffer(fire_stations, dist = buffer_distance)

ggplot() +
  geom_sf(dc_tracts, mapping = aes(),
          fill = "grey", color = "white") +
  geom_sf(buffer_stations, mapping = aes(),
          fill = "#1696d2", alpha = 0.5)


# Exercise 4: Aggregation -------------------------------------------------

states <- get_urbn_map(map = "states", sf = TRUE)

states <- states %>% 
  mutate(region = case_when(
    state_abbv %in% c("ME", "RI", "CT", "NY", "NJ", "PA", "VT", "NH", "MA") ~ "Northeast",
    state_abbv %in% c("TX", "LA", "OK", "AR", "TN", "AL", "MS", "KY", "WV",
                      "MD", "DC", "VA", "NC", "SC", "GA", "FL", "DE") ~ "South",
    state_abbv %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL",
                      "IN", "MI", "OH") ~ "Midwest",
    state_abbv %in% c("CA", "OR", "WA", "ID", "MT", "WY", "UT", "CO", "AZ", 
                      "NM", "AK", "HI", "NV") ~ "West"
  ))

regions <- states %>% 
  group_by(region) %>% 
  summarize()

ggplot() +
  geom_sf(data = regions, mapping = aes(fill = region))

