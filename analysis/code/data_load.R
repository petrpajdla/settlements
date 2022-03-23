# Project "Settlements"
# LOADS DATA
# author: Petr Pajdla
# This script loads basic data when sourced

# input data path
input_data <- "analysis/data/input_data"

# derived data paths
derived_data <- "analysis/data/derived_data"
path_data <- here::here(derived_data)
path_maps <- here::here(derived_data, "maps/")
path_dem <- here::here(derived_data, "dem/")
path_temp <- here::here(derived_data, "temp/")

# mask
mask <- sf::st_read(paste0(path_maps, "mask.geojson"), quiet = TRUE)
bbox <- sf::st_read(paste0(path_maps, "bbox.geojson"), quiet = TRUE)

# rivers + admin for plots
admin0 <- sf::st_read(paste0(path_maps, "admin0.geojson"), quiet = TRUE)
admin1 <- sf::st_read(paste0(path_maps, "admin1.geojson"), quiet = TRUE)
rivers0 <- sf::st_read(paste0(path_maps, "rivers0.geojson"), quiet = TRUE)
rivers1 <- sf::st_read(paste0(path_maps, "rivers1.geojson"), quiet = TRUE)

# labels
labs_chrono <- readr::read_rds(here::here(input_data, "chrono_labels.RDS"))

# database
set_base <- readr::read_rds(here::here(input_data, "settlements.RDS"))

# spatial data
set_spat <- sf::st_read(here::here(input_data, "settlements_sf.geojson"),
                    quiet = TRUE)

# neolithic B/C (period0)

# pottery traditions (period1)
settlements1 <- dplyr::right_join(set_spat, set_base$period1, by = "id")
settlements1 <- dplyr::mutate(settlements1, label = labs_chrono$chrono1[chrono])

# pottery groups (period2)
settlements2 <- dplyr::right_join(set_spat, set_base$period2, by = "id")
settlements2 <- dplyr::mutate(settlements2, label = labs_chrono$chrono2[chrono])

# colors
mycol <- c("B" = "#7FC97F", "M" = "#BEAED4", "b" = "#7FC97F", "m" = "#BEAED4")

rm(list = ls(pattern = "path"))
