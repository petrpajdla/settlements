# Project "Settlements"
# Script nr. 3
# DENSITY OF WATERCOURSE
# author: Petr Pajdla
#

library(dplyr)
library(sf)
library(terra)

# size of a buffer zone around the settlement (meters)
buffer_dist <- units::set_units(0.3, "km")

# data --------------------------------------------------------------------

source(here::here("analysis/code/data_load.R"))

# watercourse raster
hydro_dens <- terra::rast(
  here::here(derived_data, "hydro", "density_raster.tif")
)

# vicinity of settlemetns
set_buffer <- set_spat %>%
  st_transform(terra::crs(hydro_dens)) %>%
  st_buffer(dist = buffer_dist)


# extract vals ------------------------------------

hydro_dens_at_points <- terra::extract(
  hydro_dens,
  terra::vect(set_buffer)
) %>%
  group_by(ID) %>%
  summarise(hydro_kde = mean(density_raster)) %>%
  bind_cols(id = set_spat$id) %>%
  select(-ID)

hydro_dens_at_points %>%
  readr::write_csv(here::here(derived_data, "hydro_at_points.csv"))

# explore -----------------------------------------

hydro_dens_at_points %>%
  pull(hydro_kde) %>%
  hist()
