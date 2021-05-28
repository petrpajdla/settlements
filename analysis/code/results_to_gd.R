# Project "Settlements"
# Script nr. +Inf
# PUBLISH RESULTS DATA TO GD
# author: Petr Pajdla
# Pushes results csvs to GD

library(here)
library(tidyverse)
library(googledrive)


# upload results to GD -----------------------------------------------------

# paths
dt_result <- "analysis/data/derived_data/results"
gd_path <- "~/settlements/data/results/"

now <- paste("Updated", Sys.time())
write_lines(now, here(dt_result, "README"), append = TRUE)

# delete previous files
drive_trash(filter(drive_ls(gd_path), str_detect(name, "^results.$")))

# list files to upload
files_local <- list.files(dt_result, pattern = "csv$", full.names = TRUE)
readme <- list.files(dt_result, pattern = "^README$", full.names = TRUE)

drive_results <- map(files_local, ~ drive_upload(.x, gd_path,
                                           type = "spreadsheet",
                                           overwrite = TRUE))
drive_readme <- drive_upload(readme, gd_path, overwrite = TRUE)


# upload SI to GD ---------------------------------------------------------

si <- here("analysis/paper/paper.html")

drive_si <- drive_upload(si, "~/settlements/SI/", overwrite = TRUE)


# Figs to GD --------------------------------------------------------------

figs <- here("analysis/figures")
figs_up <- str_c(figs, "/", list.files(figs, pattern = "^res"))

map(figs_up, ~ drive_upload(.x, "~/settlements/figures/",
                                overwrite = TRUE))
