library(tidyverse)
library(urbnthemes)
library(urbnmapr)
library(tigris)
library(sf)

set_urbn_defaults(style = "map")

dc_tracts <- tigris::tracts(state = "DC",
                            cb = TRUE,
                            class = "sf")
va_tracts <- tigris::tracts(state = "VA",
                            cb = TRUE,
                            class = "sf")
md_tracts <- tigris::traCts(state = "MD",
                            cb = TRUE,
                            class = "sf")

dmv_tracts <- rbind(dc_tracts, va_tracts, md_tracts)


states <- get_urbn_map(map = "states", sf = TRUE) %>% 
  filter(state_abbv %in% c("DC", "MD", "VA"))

counties <-  get_urbn_map(map = "counties", sf = TRUE)



tract_data <- read_csv("https://r-training.s3.us-east-2.amazonaws.com/dmv_hmda_data.csv",
                       col_types = cols(census_tract = col_character(),
                                  county_fips = col_character(),
                                        loans = col_integer(),
                                        loan_amount = col_integer(),
                                        borrower_income = col_integer()))

geo_tracts <- inner_join(dmv_tracts, tract_data, by = c("GEOID" = "census_tract"))

stopifnot(
  nrow(geo_tracts) == nrow(tract_data)
)


ggplot() +
  geom_sf(counties, mapping = aes())

county_codes <- tract_data %>% 
  pull(county_fips) %>% 
  unique()

dmv_counties <- counties %>% 
  filter(county_fips %in% county_codes)


ggplot() +
  geom_sf(data = states, mapping = aes(),
          fill = "grey", color = "black") +
  geom_sf(data = dmv_counties, mapping = aes(),
          fill = "blue", color = "white")

states <- st_transform(states, crs = 6487)

dmv_counties <- st_transform(dmv_counties, crs = 6487)

geo_tracts <- st_transform(geo_tracts, crs = 6487)


dmv_states <- st_crop(states, dmv_counties)

ggplot() +
  geom_sf(data = dmv_states, mapping = aes(),
          fill = "grey", color = "black") +
  geom_sf(data = dmv_counties, mapping = aes(),
          fill = "blue", color = "white")


ggplot() +
  geom_sf(data = dmv_states, mapping = aes(),
          fill = "grey", color = "black") +
  geom_sf(data = geo_tracts, mapping = aes(),
          fill = "#1696d2", color = "white", size = 0.1) +
  geom_sf(data = dmv_counties, mapping = aes(),
          fill = NA, color = "white", size = 1)



ggplot() +
  geom_sf(data = dmv_states, mapping = aes(),
          fill = "#ececec", color = "#ec008b") +
  geom_sf(data = geo_tracts, mapping = aes(fill = loan_amount),
          color = "white", size = 0.1) +
  scale_fill_gradientn(na.value = "#9d9d9d",
                       labels = scales::dollar) +
  geom_sf(data = dmv_counties, mapping = aes(),
          fill = NA, color = "white", size = 1) +
  labs(fill = "Median loan amount")
