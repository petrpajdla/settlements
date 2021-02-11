# Project "Settlements"
# Script nr.
# WATERCOURSE DENSITY
# author: Petr Pajdla
# Joins neccessary layers and calculates watercourse intensity (KDE)

library(sf)
library(here)
library(tidyverse)
library(spatstat)
library(raster)

# read data
dt_hydro <- "analysis/data/raw_data/hydro/"

# hydrology
cz <- st_read(here(dt_hydro, "hydro_czechia"))

au <- st_read(here(dt_hydro, "hydro_austria"))

sk <- st_read(here(dt_hydro, "hydro_slovakia"))

# project mask
mask <- st_read(here("analysis/data/derived_data/maps/", "mask.geojson"))

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

rm(list = c("cz", "au", "sk"))

# control plot
ggplot() +
  geom_sf(data = au_crop, color = "green") +
  geom_sf(data = cz_crop, color = "blue") +
  geom_sf(data = sk_crop, color = "orange") +
  theme_void()

# join layers?
sk_geom <- st_combine(st_geometry(sk_crop))
au_geom <- st_combine(st_geometry(au_crop))
cz_geom <- st_combine(st_geometry(cz_crop))

rm(list = c("cz_crop", "au_crop", "sk_crop"))

# save temp files...
# dt_hydro_derived <- "analysis/data/derived_data/hydro/"
# dir.create(here(dt_hydro_derived))
# st_write(sk_geom, here(dt_hydro_derived, "sk_prep.geojson"))
# st_write(au_geom, here(dt_hydro_derived, "au_prep.geojson"))
# st_write(cz_geom, here(dt_hydro_derived, "cz_prep.geojson"))
#
# rstudioapi::restartSession()
#
# sk_geom <- st_read(here(dt_hydro_derived, "sk_prep.geojson"))
# au_geom <- st_read(here(dt_hydro_derived, "au_prep.geojson"))
# cz_geom <- st_read(here(dt_hydro_derived, "cz_prep.geojson"))

# join layers
temp <- st_union(sk_geom, au_geom)

full <- st_union(temp, cz_geom)

# save full layer
# st_write(full, here(dt_hydro_derived, "hydro_mask.geojson"))
# full <- st_read(here(dt_hydro_derived, "hydro_mask.geojson"))

rm(list = c("cz_geom", "au_geom", "sk_geom", "temp"))
rstudioapi::restartSession()

ggplot() +
  geom_sf(data = full) +
  theme_void()


# calculate density -------------------------------------------------------

full_psp <- as.psp(full)
full_dens <- density.psp(full_psp, edge = TRUE, sigma = 2000)

# plot(full_dens)

dens_raster <- full_dens %>% raster()
crs(dens_raster) <- st_crs(mask)$proj4string

dens_raster <- dens_raster %>%
  mask(mask)

# save resulting raster
dens_raster %>% writeRaster(here(dt_hydro_derived, "density_raster.tif"))
