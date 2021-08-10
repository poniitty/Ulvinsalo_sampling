

library(sf)
library(tidyverse)

df <- data.frame(
  lon = c(662200, 669100, 669100, 662200, 662200),
  lat = c(7095600, 7095600, 7100600, 7100600, 7095600)
)

polygon <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 3067) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

plot(st_geometry(polygon))

st_write(polygon, "output/aoi.gpkg")
