# Project "Settlements"
# Script nr. 0
# DATA PREPARATION II
# author: Petr Pajdla
# Downloads updated data from GD

library(here)
library(tidyverse)
library(sf)
library(googledrive)

# paths
dt_der <- "analysis/data/derived_data"
gd_path <- "~/settlements/data/database/"

# dir.create(here(dt_der, "input_gd"))

# get updated data from drive
files_gd <- drive_ls(gd_path)

for (i in seq_along(files_gd$id)) {
  drive_download(as_id(files_gd[i, ]),
                 here(dt_der, "input_gd", files_gd$name)[i],
                 type = "csv", overwrite = TRUE)
}


# prepare derived data ----------------------------------------------------


# list downloaded files
files_disk <- list.files(here(dt_der, "input_gd"), full.names = TRUE)

set_base <- vector("list")

set_base$base <- read_csv(str_subset(files_disk, "base")) %>%
  filter(in_analysis)

set_base$chrono1 <- read_csv(str_subset(files_disk, "chrono1")) %>%
  filter(id %in% set_base$base$id)

set_base$chrono2 <- read_csv(str_subset(files_disk, "chrono2")) %>%
  filter(id %in% set_base$base$id)

set_base$period1 <- read_csv(str_subset(files_disk, "period1")) %>%
  filter(id %in% set_base$base$id)

set_base$period2 <- read_csv(str_subset(files_disk, "period2")) %>%
  filter(id %in% set_base$base$id)

read_csv(str_subset(files_disk, "spatial")) %>%
  st_as_sf(coords = c("X", "Y")) %>%
  st_set_crs(5514) %>%
  st_write(here(dt_der, "settlements_sf.geojson"), delete_dsn = TRUE)

write_rds(set_base, here(dt_der, "settlements.RDS"))
