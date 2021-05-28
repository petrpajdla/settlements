# Project "Settlements"
# Script nr.
# DISTANCE TO RAW MATERIAL
# author: Petr Pajdla
# Calculated separately for the speed

devtools::load_all(".")
library(tidyverse)
library(sf)


derived_data <- "analysis/data/derived_data"
temp_data <- here::here(derived_data, "temp")

# database
set_base <- read_rds(here::here(derived_data, "settlements.RDS"))

set_spat <- st_read(here::here(derived_data, "settlements_sf.geojson"),
                    quiet = TRUE)

# raw materials
rm_pts <- st_read(here::here(derived_data, "rm_points.geojson"), quiet = TRUE) %>%
  mutate(label = if_else(orig == "l", "Chipped", "Polished"))

rm_lns <- st_read(here::here(derived_data, "rm_lines.geojson"), quiet = TRUE) %>%
  mutate(label = if_else(orig == "l", "Chipped", "Polished"))

# distance
dist_pts <- dist_to_rm(set_spat, rm_pts, "rm")

dist_lns <- dist_to_rm(set_spat, rm_lns, "rm")

# polished stone tools raw materials
pstrm <- c("AD", "AMFIBOLIT", "KULM", "MTB_JH", "PMD", "ZELESICE")

dist <- bind_rows(p = dist_pts, l = dist_lns, .id = "shp") %>%
  select(id, everything()) %>%
  mutate(across(where(is.numeric), unclass),
         type = if_else(rm %in% pstrm, "Polished", "Chipped"),
         region = if_else(str_detect(id, "^B"), "Boh.", "Mor."))


# save result

if (!dir.exists(temp_data)) {
  dir.create(temp_data)
}

write_csv(dist, paste0(temp_data, "/rm_dist.csv"))

