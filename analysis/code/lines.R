# Project "Settlements"
# Script nr.
# ARRANGEMENT ON LINES
# author: Petr Pajdla
#

library(tidyverse)
library(sf)

derived_data <- "analysis/data/derived_data"
path_maps <- here::here(derived_data, "maps/")

mask <- st_read(paste0(path_maps, "mask.geojson"), quiet = TRUE)

# database
set_base <- read_rds(here::here(derived_data, "settlements.RDS"))

set_spat <- st_read(here::here(derived_data, "settlements_sf.geojson"),
                    quiet = TRUE)

chrono_labels <- read_rds(here::here(derived_data, "chrono_labels.RDS"))

# # settlements spatial by periods
# set_spat_1 <- set_spat %>%
#   right_join(set_base$period1) %>%
#   mutate(reg = if_else(str_detect(id, "^B"), "b", "m"))
#
# set_spat_2 <- set_spat %>%
#   right_join(set_base$period2) %>%
#   mutate(reg = if_else(str_detect(id, "^B"), "b", "m"))

# arrangement on lines
ids <- set_base$base %>% pull(id)
names(ids) <- set_base$base %>% pull(site)

add_id <- function(x) {
  x %>%
    filter(site %in% set_base$base$site) %>%
    mutate(id = unname(ids[site]))
}

pivot_terrain_and_water <- function(x) {
  x %>%
    pivot_longer(c(starts_with("w"), starts_with("t")), values_to = "line_len") %>%
    filter(!is.na(line_len)) %>%
    separate(name, into = c("type", "chrono"), sep = "_") %>%
    group_by(type)
}

pivot_period <- function(x) {
  x %>%
    pivot_longer(cols = starts_with("p"), names_to = "period") %>%
    filter(value) %>%
    select(-value) %>%
    mutate(period_label = chrono_labels$periods[period],
           period_label = factor(period_label, levels = chrono_labels$periods))
}

# east art of bohemia
# traditions
b1 <- readxl::read_excel(
  here::here("analysis/data/raw_data/lines.xlsx"),
  sheet = "b") %>%
  select(site = NAZLOK, site2 = ALTERNAZ, starts_with("w"), starts_with("t")) %>%
  add_id() %>%
  pivot_terrain_and_water() %>%
  mutate("p5.0" = if_else(chrono  %in% c("SBK1", "SBK2"), TRUE, FALSE),
         "p4.8" = if_else(chrono  %in% c("SBK1", "SBK2", "LgK2"), TRUE, FALSE),
         "p4.6" = if_else(chrono  %in% c("LgK2"), TRUE, FALSE),
         "p4.4" = if_else(chrono  %in% c("LgK2", "ENE0"), TRUE, FALSE),
         "p4.2" = if_else(chrono  == "ENE0", TRUE, FALSE),
         "p4.0" = if_else(chrono  == "ENE0", TRUE, FALSE),
         "p3.8" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE),
         "p3.6" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE),
         "p3.4" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE)) %>%
  pivot_period()

# grousp
b2 <- readxl::read_excel(
  here::here("analysis/data/raw_data/lines.xlsx"),
  sheet = "b") %>%
  select(site = NAZLOK, site2 = ALTERNAZ, starts_with("w"), starts_with("t")) %>%
  add_id() %>%
  pivot_terrain_and_water() %>%
  mutate("p5.0" = if_else(chrono  %in% c("SBK1", "SBK2"), TRUE, FALSE),
         "p4.8" = if_else(chrono  %in% c("SBK1", "SBK2", "LgK2"), TRUE, FALSE),
         "p4.6" = if_else(chrono  %in% c("LgK2"), TRUE, FALSE),
         "p4.4" = if_else(chrono  %in% c("LgK2", "ENE0"), TRUE, FALSE),
         "p4.2" = if_else(chrono  == "ENE0", TRUE, FALSE),
         "p4.0" = if_else(chrono  == "ENE0", TRUE, FALSE),
         "p3.8" = if_else(chrono  == "TRB1", TRUE, FALSE),
         "p3.6" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE),
         "p3.4" = if_else(chrono  == "TRB2", TRUE, FALSE)) %>%
  pivot_period()

# morava river catchment
# traditions
m1 <- readxl::read_excel(
  here::here("analysis/data/raw_data/lines.xlsx"),
  sheet = "m") %>%
  select(site = NAZLOK, site2 = ALTERNAZ, starts_with("w"), starts_with("t")) %>%
  add_id() %>%
  pivot_terrain_and_water() %>%
  mutate("p5.0" = if_else(chrono  %in% c("SBK1", "SBK2"), TRUE, FALSE),
         "p4.8" = if_else(chrono  %in% c("SBK1", "SBK2", "LgK1", "LgK2"), TRUE, FALSE),
         "p4.6" = if_else(chrono  %in% c("LgK1", "LgK2"), TRUE, FALSE),
         "p4.4" = if_else(chrono  %in% c("LgK1", "LgK2", "ENE0"), TRUE, FALSE),
         "p4.2" = if_else(chrono  == "ENE0", TRUE, FALSE),
         "p4.0" = FALSE,
         "p3.8" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE),
         "p3.6" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE),
         "p3.4" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE)) %>%
  pivot_period()

# groups
m2 <- readxl::read_excel(
  here::here("analysis/data/raw_data/lines.xlsx"),
  sheet = "m") %>%
  select(site = NAZLOK, site2 = ALTERNAZ, starts_with("w"), starts_with("t")) %>%
  add_id() %>%
  pivot_terrain_and_water() %>%
  mutate("p5.0" = if_else(chrono  %in% c("SBK1", "SBK2"), TRUE, FALSE),
         "p4.8" = if_else(chrono  %in% c("SBK1", "SBK2", "LgK1"), TRUE, FALSE),
         "p4.6" = if_else(chrono  %in% c("LgK1", "LgK2"), TRUE, FALSE),
         "p4.4" = if_else(chrono  %in% c("LgK2", "ENE0"), TRUE, FALSE),
         "p4.2" = if_else(chrono  == "ENE0", TRUE, FALSE),
         "p4.0" = FALSE,
         "p3.8" = if_else(chrono  == "TRB1", TRUE, FALSE),
         "p3.6" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE),
         "p3.4" = if_else(chrono  == "TRB2", TRUE, FALSE)) %>%
  pivot_period()


# result ------------------------------------------------------------------

lines1 <- bind_rows(m1, b1) %>%
  mutate(reg = if_else(str_detect(id, "^M"), "m", "b"),
         line_len_scaled = line_len / max(line_len)) %>%
  mutate(variable = if_else(type == "t", "line_terrain", "line_water")) %>%
  ungroup() %>%
  select(-starts_with("site"), -type, -line_len, -period_label, -reg,
         value = line_len_scaled)

lines2 <- bind_rows(m2, b2) %>%
  mutate(reg = if_else(str_detect(id, "^M"), "m", "b"),
         line_len_scaled = line_len / max(line_len)) %>%
  mutate(variable = if_else(type == "t", "line_terrain", "line_water")) %>%
  ungroup() %>%
  select(-starts_with("site"), -type, -line_len, -period_label, -reg,
         value = line_len_scaled)

linear_arrangement <- list(traditions = lines1,
                           groups = lines2)

out_dir <- here::here(derived_data, "lines")
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

write_rds(linear_arrangement, paste0(out_dir, "/linear_arrangement.RDS"))
