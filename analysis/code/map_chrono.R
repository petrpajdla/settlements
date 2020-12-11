# Project "Settlements"
# Script nr.
# PLOT SETTLEMENTS
# author: Petr Pajdla
# Plot settlement distribution in different periods

library(tidyverse)
library(sf)
library(here)

# dt_raw <- "analysis/data/raw_data"
dt_der <- "analysis/data/derived_data"
dt_maps <- here(dt_der, "maps/")

pth_plots <- "analysis/figures/temp"

# read data ---------------------------------------------------------------
# mask
mask <- st_read(paste0(dt_maps, "mask.geojson"))
bbox <- st_read(paste0(dt_maps, "bbox.geojson"))

# rivers + admin
admin0 <- st_read(paste0(dt_maps, "admin0.geojson"))
admin1 <- st_read(paste0(dt_maps, "admin1.geojson"))
rivers0 <- st_read(paste0(dt_maps, "rivers0.geojson"))
rivers1 <- st_read(paste0(dt_maps, "rivers1.geojson"))

# chrono labels
labs_chrono <- read_rds(here(dt_der, "chrono_labels.RDS"))

set_base <- read_rds(here(dt_der, "settlements.RDS"))

settlements1 <- st_read(here(dt_der, "settlements_sf.geojson")) %>%
  right_join(set_base$chrono1, by = "rowid")

settlements2 <- st_read(here(dt_der, "settlements_sf.geojson")) %>%
  right_join(set_base$chrono2, by = "rowid")

# maps - point distribution -----------------------------------------------

# 1
ch1_n <- set_base$chrono1 %>%
  group_by(label) %>%
  summarize(n = n()) %>%
  mutate(n = str_c("n = ", n))

settlements1 %>%
  ggplot() +
  geom_sf(data = mask, fill = "gray80", color = NA, alpha = 0.4) +
  # geom_sf(data = admin1, linetype = 3, fill = NA, color = "gray60", size = 0.2, alpha = 0.4) +
  geom_sf(data = admin0, linetype = 1, fill = NA, color = "gray60", alpha = 0.4) +
  geom_sf(data = rivers1, color = "mediumblue", size = 0.4, alpha = 0.4) +
  geom_sf(data = rivers0, color = "mediumblue", size = 0.6, alpha = 0.4) +
  geom_sf(data = bbox, fill = NA) +
  geom_sf(alpha = 0.4) +
  geom_label(data = ch1_n, aes(y = -980000, x = -500000, label = n)) +
  theme_void() +
  facet_wrap(vars(label))

ggsave(here(pth_plots, "chrono1.png"), scale = 3)

# 2
ch2_n <- set_base$chrono2 %>%
  group_by(label) %>%
  summarize(n = n()) %>%
  mutate(n = str_c("n = ", n))

settlements2 %>%
  ggplot() +
  geom_sf(data = mask, fill = "gray80", color = NA, alpha = 0.4) +
  # geom_sf(data = admin1, linetype = 3, fill = NA, color = "gray60", size = 0.2, alpha = 0.4) +
  geom_sf(data = admin0, linetype = 1, fill = NA, color = "gray60", alpha = 0.4) +
  geom_sf(data = rivers1, color = "mediumblue", size = 0.4, alpha = 0.4) +
  geom_sf(data = rivers0, color = "mediumblue", size = 0.6, alpha = 0.4) +
  geom_sf(data = bbox, fill = NA) +
  geom_sf(alpha = 0.4) +
  geom_label(data = ch2_n, aes(y = -980000, x = -500000, label = n)) +
  theme_void() +
  facet_wrap(vars(label), nrow = 2)

ggsave(here(pth_plots, "chrono2.png"), scale = 3)

# # 3
# ch3_n <- settlements %>%
#   st_drop_geometry() %>%
#   group_by(chrono3_label) %>%
#   summarize(n = n()) %>%
#   mutate(chrono3_label = labs_chrono$chrono3[chrono3_label],
#          chrono3_label = factor(chrono3_label, levels = labs_chrono$chrono3),
#          n = str_c("n = ", n))
#
# settlements %>%
#   # filter(!is.na(chrono3)) %>%
#   ggplot() +
#   geom_sf(data = kraje, fill = NA, color = "gray80") +
#   geom_sf(shape = 4) +
#   geom_text(data = ch3_n, aes(y = 51, x = 18, label = n)) +
#   theme_void() +
#   facet_wrap(~chrono3_label)
#
# ggsave(here(path_plots, "chrono3.png"), scale = 3)
