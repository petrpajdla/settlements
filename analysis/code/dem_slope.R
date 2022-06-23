# Project "Settlements"
# Script nr. 1
# DEM: SLOPE
# author: Petr Pajdla
# Extract values from DEM: slope

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


# # altitude from dem -------------------------------
#
# nmv <- extract(r, vect(set_buffer))
#
# nmv %>% tibble::tibble() %>%
#   group_by(ID) %>%
#   summarise(mean = mean(DEM_digital_elevation)) %>%
#   bind_cols(id = set_spat$id, alt = set_spat$altitude) %>%
#   summarise(cor = cor(mean, alt))


# slope -------------------------------------------

mean_slope <- extract_from_dem(set_buffer, r, model = "slope", fun = "mean")

mean_slope %>%
  select(id, value) %>%
  readr::write_csv(here::here(derived_data, "slope_at_points.csv"))

# explore -----------------------------------------

library(ggplot2)

mean_slope %>%
  mutate(reg = stringr::str_extract(id, ".")) %>%
  ggplot(aes(value, reg)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  geom_jitter(alpha = 0.1, height = 0.2) +
  theme_bw() +
  coord_flip() +
  labs(x = "slope (°)")

ggplot() +
  geom_sf(data = st_as_sf(mean_slope), aes(color = value),
          size = 2, alpha = 0.8) +
  theme_void()

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
