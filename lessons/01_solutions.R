
# Exercise 0 --------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("remotes")
# remotes::install_github("UrbanInstitute/urbnthemes")
# remotes::install_github("UrbanInstitute/urbnmapr")

library(tidyverse)
library(urbnthemes)
library(urbnmapr)

# Exercise 1 --------------------------------------------------------------

states <- get_urbn_map(map = "states", sf = TRUE)

ggplot() +
  geom_sf(data = states, mapping = aes())
    

ggplot() +
  geom_sf(
    data = states, mapping = aes(),
    fill = "#1696d2", color = "white"
  ) +
  theme_urbn_map()


# Exercise 2 --------------------------------------------------------------

set_urbn_defaults(style = "map")

data <- read_csv("https://raw.githubusercontent.com/UI-Research/urbn101-mapping/master/data/state_data.csv?token=AHJ7BDKDASBKNTRGKVZ756C5QTRPE")

geo_data <- left_join(states, data, by = "state_name")

ggplot() +
  geom_sf(
    data = geo_data, mapping = aes(fill = medhhincome),
    color = "white"
  ) +
  scale_fill_gradientn(labels = scales::dollar)



# Exercise 3 --------------------------------------------------------------

ggplot() +
  geom_sf(
    data = geo_data, mapping = aes(fill = cat_var),
    color = "#ffffff"
  ) +
  scale_fill_manual(values = c("#f5cbdf", "#e54096", "#af1f6b", "#351123")) +
  labs(fill = "My categorical variable")







