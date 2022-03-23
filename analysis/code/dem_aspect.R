# Project "Settlements"
# Script nr. -
# DEM: ASPECT
# author: Petr Pajdla
# Extract values from DEM: aspect, not included further

library(dplyr)
library(sf)
devtools::load_all()

# size of a buffer zone around the settlement (meters)
buffer_dist <- units::set_units(0.2, "km")

# data --------------------------------------------------------------------

source(here::here("analysis/code/data_load.R"))

# dem
r <- terra::rast(here::here("analysis/data/raw_data/dem/DEM_digital_elevation.tif"))

# vicinity of settlemetns
set_buffer <- set_spat %>%
  st_transform(terra::crs(r)) %>%
  st_buffer(dist = buffer_dist)

# slope -------------------------------------------

median_asp <- extract_from_dem(set_buffer, r, model = "aspect", fun = "median")

# median_asp %>%
#   select(id, value) %>%
#   readr::write_csv(here::here(derived_data, "from_dem/tpi.csv"))

# explore -----------------------------------------

shapiro.test(filter(median_asp, reg == "B")$value)
shapiro.test(filter(median_asp, reg == "M")$med_asp)

library(ggplot2)

median_asp %>%
  mutate(reg = stringr::str_extract(id, ".")) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "white") +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(0, 90, 180, 270)) +
  coord_polar(start = 0) +
  facet_wrap(vars(reg)) +
  theme_bw() +
  labs(x = "median aspect (°)")

median_asp %>%
  mutate(reg = stringr::str_extract(id, ".")) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "white") +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(0, 90, 180, 270)) +
  facet_wrap(vars(reg)) +
  theme_bw() +
  labs(x = "median aspect (°)")

median_asp %>%
  mutate(reg = stringr::str_extract(id, ".")) %>%
  inner_join(settlements2, by = "id") %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "white") +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(0, 90, 180, 270, 360)) +
  facet_grid(vars(period_label), vars(reg), scales = "free_y") +
  theme_bw() +
  labs(x = "median aspect (°)")
