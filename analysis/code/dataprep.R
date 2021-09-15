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
labs_chrono <- read_rds(here::here(dt_der, "chrono_labels.RDS"))

# labs_chrono$periods["p5.0"] <- "4.9 – 4.8 k"
# labs_chrono$periods["p3.4"] <- "3.4 – 3.3 k"
#
# write_rds(labs_chrono, here::here(dt_der, "chrono_labels.RDS"))

# list downloaded files
files_disk <- list.files(here(dt_der, "input_gd"), full.names = TRUE)

# prepare input files for analysis
set_base <- vector("list")

set_base$base <- read_csv(str_subset(files_disk, "base")) %>%
  filter(in_analysis)

# set_base$chrono1 <- read_csv(str_subset(files_disk, "chrono1")) %>%
#   filter(id %in% set_base$base$id)
#
# set_base$chrono2 <- read_csv(str_subset(files_disk, "chrono2")) %>%
#   filter(id %in% set_base$base$id)

set_base$period1 <- read_csv(str_subset(files_disk, "period1")) %>%
  filter(id %in% set_base$base$id) %>%
  mutate(
    period_label = labs_chrono$periods[period],
    period_label = fct_relevel(period_label, unname(labs_chrono$periods)),
    period = fct_relevel(period, names(labs_chrono$periods)),
    keep = if_else(str_detect(id, "^B") &
                     period == "p4.8" &
                     chrono == "LgK", FALSE, TRUE)
  ) %>%
  filter(keep) %>%
  select(-keep)

set_base$period2 <- read_csv(str_subset(files_disk, "period2")) %>%
  filter(id %in% set_base$base$id) %>%
  mutate(
    period_label = labs_chrono$periods[period],
    period_label = fct_relevel(period_label, unname(labs_chrono$periods)),
    period = fct_relevel(period, names(labs_chrono$periods)),
    keep = if_else(str_detect(id, "^B") &
                     period == "p4.8" &
                     chrono == "LgK2", FALSE, TRUE),
    keep = if_else(period == "p5.0" &
                     chrono == "SBK2", FALSE, keep),
    keep = if_else(period == "p3.6" &
                     chrono == "TRB1", FALSE, keep)
  ) %>%
  filter(keep) %>%
  select(-keep)

set_base$references <- read_csv(str_subset(files_disk, "references")) %>%
  filter(id %in% set_base$base$id)

read_csv(str_subset(files_disk, "spatial")) %>%
  st_as_sf(coords = c("X", "Y")) %>%
  st_set_crs(5514) %>%
  st_write(here(dt_der, "settlements_sf.geojson"), delete_dsn = TRUE)

write_rds(set_base, here(dt_der, "settlements.RDS"))


# # upload corrected csv to GD ----------------------------------------------
#
# dt_manual <- paste0(dt_der, "/output_manual_edit")
#
# set_base$period1 %>%
#   select(-period_label) %>%
#   arrange(id, period) %>%
#   write_csv(here(dt_manual, "period1.csv"))
#
# set_base$period2 %>%
#   select(-period_label) %>%
#   arrange(id, period) %>%
#   write_csv(here(dt_manual, "period2.csv"))
#
# cor_per_up <- c(here(dt_manual, "period1.csv"),
#                 here(dt_manual, "period2.csv"))
#
# map(cor_per_up, ~ drive_upload(.x, gd_path,
#                                type = "spreadsheet"))
