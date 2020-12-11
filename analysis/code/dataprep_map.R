# Project "Settlements"
# Script nr. 0
# PREPARE BACKGROUND MAP DATA
# author: Petr Pajdla
# Prepare background data for maps (crop, transform etc.)
# Data from Natural Earth Project using rnaturalearth package

library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)

# paths -------------------------------------------------------------------
dt_raw <- "analysis/data/raw_data"
dt_der <- "analysis/data/derived_data"

dt_ne <- here(paste0(dt_raw, "/naturalearth"))
dt_area <- here(paste0(dt_raw, "/project_area"))

dt_map <- here(paste0(dt_der, "/maps"))

# bbox & mask -------------------------------------------------------------
bbox_large <- st_read(dt_area, "bbox_wgs84_large")
mask <- st_read(dt_area, "maska_poly_wgs84") %>%
  st_transform(5514)
bbox <- st_bbox(st_buffer(mask, dist = 20000)) %>% st_as_sfc()

# natural earth data ------------------------------------------------------
crop_and_transform <- function(x) {
  x %>%
    st_crop(bbox_large) %>%
    st_transform(5514) %>%
    st_crop(bbox) %>%
    st_geometry()
}

# rivers
rivers0 <- ne_load(destdir = dt_ne,
                   scale = 10,
                   category = "physical",
                   type = "rivers_lake_centerlines",
                   returnclass = "sf") %>%
  crop_and_transform()

rivers1 <- ne_load(destdir = dt_ne,
                   scale = 10,
                   category = "physical",
                   type = "rivers_europe",
                   returnclass = "sf") %>%
  crop_and_transform()

# state + admin boundaries
admin0 <- ne_load(scale = 10,
                  category = "cultural",
                  type = "boundary_lines_land",
                  destdir = dt_ne,
                  returnclass = "sf") %>%
  crop_and_transform()

admin1 <- ne_load(scale = 10,
                  category = "cultural",
                  type = "admin_1_states_provinces",
                  destdir = dt_ne,
                  returnclass = "sf") %>%
  crop_and_transform()

# plots -------------------------------------------------------------------

ggplot() +
  geom_sf(data = mask, fill = "wheat", color = NA, alpha = 0.4) +
  geom_sf(data = admin1, linetype = 3, fill = NA, color = "gray60", size = 0.2, alpha = 0.4) +
  geom_sf(data = admin0, linetype = 1, fill = NA, color = "gray60", alpha = 0.4) +
  geom_sf(data = rivers1, color = "mediumblue", size = 0.4, alpha = 0.6) +
  geom_sf(data = rivers0, color = "mediumblue", size = 0.6, alpha = 0.6) +
  # geom_rect(aes(xmin = bbox[1], xmax = bbox[3],
  #               ymin = bbox[2], ymax = bbox[4]), fill = NA, color = "black") +
  geom_sf(data = bbox, fill = NA) +
  theme_void()

# write derived datasets --------------------------------------------------

geojson <- function(x) {
  st_write(x,
           dsn = paste0(dt_map, "/", deparse(substitute(x)), ".geojson"),
           delete_dsn = TRUE)
}

geojson(rivers0)
geojson(rivers1)
geojson(admin0)
geojson(admin1)
geojson(mask)
geojson(bbox_large)
geojson(bbox)
