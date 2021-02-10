
library(sf)
library(here)
library(tidyverse)

# read data
dt_hydro <- "analysis/data/raw_data/hydro/"

# hydrology
cz <- st_read(here(dt_hydro, "hydro_czechia"))

au <- st_read(here(dt_hydro, "hydro_austria"))

sk <- st_read(here(dt_hydro, "hydro_slovakia"))

# project mask
mask <- st_read(here(here("analysis/data/derived_data/maps/", "mask.geojson")))

# transform to S-JTSK and crop to mask
au_crop <- au %>%
  st_zm() %>%
  st_transform(st_crs(mask)) %>%
  st_intersection(mask)

sk_crop <- sk %>%
  st_transform(st_crs(mask)) %>%
  st_intersection(mask)

cz_crop <- cz %>%
  st_intersection(mask)

# control plot
ggplot() +
  geom_sf(data = au_crop, color = "green") +
  geom_sf(data = cz_crop, color = "blue") +
  geom_sf(data = sk_crop, color = "orange") +
  theme_void()

# join layers
temp <- st_join(st_as_sf(st_geometry(au_crop)),
                st_as_sf(st_geometry(sk_crop)))
x <- st_join(temp, st_as_sf(st_geometry(cz_crop)))


ggplot() +
  geom_sf(data = x) +
  theme_void()

st_union()
