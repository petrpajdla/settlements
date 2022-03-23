# prepare data for JOAD submission / Zotero

library(sf)
library(tidyverse)


# data --------------------------------------------------------------------

derived_data <- "analysis/data/derived_data"
path_maps <- here::here(derived_data, "maps/")
path_data <- here::here(derived_data)
path_out <- here::here("analysis/data/joad_data/")

mask <- st_read(paste0(path_maps, "mask.geojson"), quiet = TRUE)

labs_chrono <- read_rds(here::here(derived_data, "chrono_labels.RDS"))

set_base <- read_rds(here::here(derived_data, "settlements.RDS"))

set_spat <- st_read(here::here(derived_data, "settlements_sf.geojson"),
                    quiet = TRUE)

source("./R/rm_distance.R")
rm_pts <- st_read(here::here(derived_data, "rm_points.geojson"), quiet = TRUE) %>%
  add_rm_type()

rm_lns <- st_read(here::here(derived_data, "rm_lines.geojson"), quiet = TRUE) %>%
  add_rm_type()


# filtering ---------------------------------------------------------------

in_analysis <- set_base$base %>%
  filter(in_analysis) %>%
  pull(id)

# maska -------------------------------------------------------------------
# mask %>%
#   st_geometry() %>%
#   st_write(paste0(path_out, "geodata/", "study_area.gml"), append = FALSE)

# plot(st_read(paste0(path_out, "study_area.gml"), options="FORCE_SRS_DETECTION=YES"))


# site locations ----------------------------------------------------------

# set_spat %>%
#   select(-site) %>%
#   filter(id %in% in_analysis) %>%
#   mutate(accuracy = as.integer(accuracy)) %>%
#   arrange(id) %>%
#   st_write(paste0(path_out, "geodata/", "site_locations.gml"), append = FALSE)

# fuu <- st_read(paste0(path_out, "site_locations.gml"), options="FORCE_SRS_DETECTION=YES")


# regions ----------------------------------------

buffer <- set_spat %>%
  filter(id %in% in_analysis) %>%
  mutate(region = str_extract(id, "^.")) %>%
  group_by(region) %>%
  st_buffer(10e3)

region_m <- buffer %>%
  filter(region == "M") %>%
  st_union() %>%
  nngeo::st_remove_holes() %>%
  rmapshaper::ms_simplify(keep = 0.02, weighting = 1) %>%
  st_intersection(mask) %>%
  st_sf() %>%
  mutate(region = "Morava river catchment",
         reg = "M")

region_b <- buffer %>%
  filter(region == "B") %>%
  st_union() %>%
  nngeo::st_remove_holes() %>%
  rmapshaper::ms_simplify(keep = 0.02, weighting = 1) %>%
  st_intersection(mask) %>%
  st_sf() %>%
  mutate(region = "Eastern part of Bohemia",
         reg = "B")

area <- bind_rows(region_m, region_b) %>%
  st_area() %>%
  units::set_units("km^2")

area[1] / 1556 # M - 12.42359 lokalit/km2
area[2] / 597 # B - 15.25837 lokalit/km2

# bind_rows(region_m, region_b) %>%
#   st_write(paste0(path_out, "geodata/", "regions.gml"), append = FALSE)


# raw mat -----------------------------------------------------------------

# bind_rows(rm_pts, rm_lns) %>%
#   mutate(id = row_number(),
#          id = str_pad(id, width = 2, side = "left", pad = "0")) %>%
#   select(-label) %>%
#   rename(type = orig) %>%
#   st_write(paste0(path_out, "geodata/", "raw_material_sources.gml"), append = FALSE)

# fuu <- st_read(paste0(path_out, "raw_material_sources.gml"), options="FORCE_SRS_DETECTION=YES")
# st_geometry(fuu) %>% plot()

# rm labels

# readODS::read_ods(here::here("analysis/data/raw_data/suroviny/suroviny_eng.ods"),
#                   col_names = FALSE) %>%
#   as_tibble() %>%
#   rename(rm = A, label = B) %>%
#   arrange(rm) %>%
#   write_csv(paste0(path_out, "vocabularies/", "voc_raw_materials.csv"),
#             quote = "all")


# base --------------------------------------------------------------------

# set_base$base %>%
#   select(-in_analysis, -note, -site_note, -region) %>%
#   filter(id %in% in_analysis) %>%
#   arrange(id) %>%
#   write_csv(paste0(path_out, "sites.csv"), quote = "all")

# read_csv(paste0(path_out, "sites.csv"))

# set_base$period1 %>%
#   select(-period_label) %>%
#   filter(id %in% in_analysis) %>%
#   arrange(id) %>%
#   write_csv(paste0(path_out, "pot_traditions.csv"), quote = "all")

# read_csv(paste0(path_out, "pot_traditions.csv"))

# set_base$period2 %>%
#   select(-period_label, -facet) %>%
#   filter(id %in% in_analysis) %>%
#   arrange(id) %>%
#   write_csv(paste0(path_out, "pot_groups.csv"), quote = "all")

# read_csv(paste0(path_out, "pot_groups.csv"))


# references --------------------------------------------------------------

ref <- set_base$references %>%
  filter(id %in% in_analysis) %>%
  group_by(reference) %>%
  nest() %>%
  ungroup(reference) %>%
  arrange(reference) %>%
  mutate(ref_id = row_number(),
         ref_id = str_pad(ref_id, width = 4, pad = "0", side = "left"),
         ref_id = paste0("Ref", ref_id))

# tail(ref, 200) %>% View()

# ref %>%
#   select(ref_id, reference) %>%
#   arrange(ref_id) %>%
#   write_csv(paste0(path_out, "references.csv"), quote = "all")

# ref %>%
#   select(ref_id, data) %>%
#   unnest(data) %>%
#   select(id, ref_id) %>%
#   arrange(id) %>%
#   write_csv(paste0(path_out, "references_sites.csv"), quote = "all")


# labels ------------------------------------------------------------------

# labs_chrono$periods %>%
#   as_tibble(rownames = "period") %>%
#   rename(label = value) %>%
#   write_csv(paste0(path_out, "vocabularies/", "voc_periods.csv"),
#             quote = "all")

# labs_chrono$chrono1 %>%
#   as_tibble(rownames = "chrono") %>%
#   rename(label = value) %>%
#   mutate(periodo_link = c(
#     "http://n2t.net/ark:/99152/p0wctqtnkjq",
#     "http://n2t.net/ark:/99152/p0wctqt4rnk",
#     NA,
#     "http://n2t.net/ark:/99152/p0wctqtm5d7"
#   )) %>%
#   write_csv(paste0(path_out, "vocabularies/", "voc_pot_traditions.csv"),
#             quote = "all")

# labs_chrono$chrono2 %>%
#   as_tibble(rownames = "chrono") %>%
#   rename(label = value) %>%
#   mutate(periodo_link = c(
#     NA,
#     NA,
#     NA,
#     NA,
#     NA,
#     "http://n2t.net/ark:/99152/p0wctqtc2b9",
#     NA,
#     NA,
#     NA
#   )) %>%
#   write_csv(paste0(path_out, "vocabularies/", "voc_pot_groups.csv"),
#             quote = "all")

# tribble(~chrono, ~label,
#         "SBK1", "SBK Early",
#         "SBK2", "SBK Late/LgK I",
#         "LgK1", "SBK Late/LgK I",
#         "LgK2", "LgK II",
#         "ENE0", "Proto Ene.",
#         "ENE1", "Proto Ene.",
#         "ENE2", "Proto Ene.",
#         "TRB1", "TRB Baalberge",
#         "TRB1", "TRB Boleraz/Saalzmuende") %>%
#   write_csv(paste0(path_out, "vocabularies/", "voc_pot_groups_facets.csv"),
#             quote = "all")

# labs_chrono_facets <- chrono_facets$facet %>%
#   setNames(chrono_facets$chrono)
# unname(labs_chrono_facets) %>%
#   factor(levels = unique(labs_chrono_facets), ordered = TRUE)


