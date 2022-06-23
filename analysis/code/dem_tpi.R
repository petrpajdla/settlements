# Project "Settlements"
# Script nr. 2
# DEM: TPI
# author: Petr Pajdla
# Extract values from DEM: Terrain/Topography position index

library(dplyr)
library(sf)
devtools::load_all()

# size of a buffer zone around the settlement (meters)
buffer_dist <- units::set_units(0.3, "km")

# data --------------------------------------------------------------------

source(here::here("analysis/code/data_load.R"))

# dem
r <- terra::rast(here::here("analysis/data/raw_data/dem/DEM_digital_elevation.tif"))

# vicinity of settlemetns
set_buffer <- set_spat %>%
  st_transform(terra::crs(r)) %>%
  st_buffer(dist = buffer_dist)


# tpi ---------------------------------------------

mean_tpi <- extract_from_dem(set_buffer, r, model = "TPI", fun = "mean")

mean_tpi %>%
  select(id, value) %>%
  readr::write_csv(here::here(derived_data, "tpi_at_points.csv"))

# explore -----------------------------------------

library(ggplot2)

mean_tpi %>%
  mutate(reg = stringr::str_extract(id, ".")) %>%
  ggplot(aes(value, reg)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  geom_jitter(alpha = 0.1, height = 0.2) +
  theme_bw() +
  coord_flip() +
  labs(x = "TPI (terrain position index)")

mean_tpi %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(color = value), size = 1.8)

mean_tpi %>%
  ggplot(aes(value)) +
  geom_histogram()

# mean_slope %>%
#   mutate(reg = stringr::str_extract(id, ".")) %>%
#   inner_join(settlements2, by = "id") %>%
#   ggplot(aes(slope)) +
#   geom_histogram(color = "black", fill = "white") +
#   facet_grid(vars(period_label), vars(reg), scales = "free_y") +
#   theme_bw() +
#   labs(x = "slope (°)")
#
# mean_slope %>%
#   mutate(reg = stringr::str_extract(id, ".")) %>%
#   inner_join(settlements2, by = "id") %>%
#   ggplot(aes(slope, period_label)) +
#   geom_boxplot(color = "black", fill = "white") +
#   facet_wrap(vars(reg), scales = "free_y") +
#   theme_bw() +
#   labs(x = "slope (°)")
#
# t.test(mean_tpi[stringr::str_detect(mean_tpi$id, "M"), ]$value,
#        mean_tpi[stringr::str_detect(mean_tpi$id, "B"), ]$value)
