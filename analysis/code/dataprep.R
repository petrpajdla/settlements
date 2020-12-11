# Project "Settlements"
# Script nr. 0
# DATA PREPARATION
# author: Petr Pajdla
# Prepares data for the analysis from raw spreadsheet formats

library(here)
library(tidyverse)
library(readxl)
library(sf)

dt_raw <- "analysis/data/raw_data"
dt_der <- "analysis/data/derived_data"

# funs --------------------------------------------------------------------

indicator_to_logical <- function(x) {
  x <- if_else(str_detect(x, "1"), "TRUE", x)
  x <- if_else(str_detect(x, "0"), "FALSE", x)
  x <- if_else(str_detect(x, "\\?"), "TRUE", x)
  return(as.logical(x))
}

# data input --------------------------------------------------------------

in_morava <- read_excel(here(dt_raw, "morava_pp.xlsx"))

in_cechy <- read_excel(here(dt_raw, "cechy_pp.xlsx"))

# morava ------------------------------------------------------------------
m <- in_morava %>%
  rowid_to_column() %>%
  # correct phase labels
  rename(SBK_Late = SBK_IV,
         'TRB_Boleraz/Saalzm' = TRB_Boleraz) %>%
  unite(col = "ENE_Proto", LgK_EPI, Ludanice, na.rm = TRUE) %>%
  unite(col = "SBK_Early", `SBK_II-III`, SBK_III, na.rm = TRUE) %>%
  unite(col = "LgK_II_b", LgK_II_b, Brodzany, na.rm = TRUE) %>%
  unite(col = "TRB_Baalberg", TRB_Baalberg, TRB_I, na.rm = TRUE) %>%
  mutate(across(.cols = c(SBK:LgK_II_a), as.character),
         across(.cols = c(SBK_Early, LgK_II_b, ENE_Proto, TRB_Baalberg), na_if, ""))

# level 1
m1 <- m %>% select(rowid, SBK, LgK, TRB) %>%
  pivot_longer(-rowid,
               names_to = "chrono1",
               values_to = "chrono1_dubious", values_drop_na = TRUE) %>%
  # filter only certain sites
  filter(chrono1_dubious == 1) %>%
  select(-chrono1_dubious)

# level 2
m2 <- m %>% select(rowid, SBK_Early, SBK_Late, LgK_I, LgK_II, ENE_Proto,
                   TRB_Baalberg, `TRB_Boleraz/Saalzm`) %>%
  pivot_longer(-rowid,
               names_to = "chrono2",
               values_to = "chrono2_dubious", values_drop_na = TRUE) %>%
  separate(chrono2, into = c("chrono1", "chrono2"), sep = "_") %>%
  # filter only certain sites
  filter(chrono2_dubious != "1?") %>%
  select(-chrono2_dubious)

# # level 3
# m3 <- m %>% select(rowid, LgK_I_a, LgK_I_b, LgK_I_c, LgK_II_a, LgK_II_b) %>%
#   pivot_longer(-rowid,
#                names_to = "chrono3",
#                values_to = "chrono3_dubious", values_drop_na = TRUE) %>%
#   separate(chrono3, into = c("chrono1", "chrono2", "chrono3"), sep = "_")

# full chronology
m_chrono <- full_join(m1, m2, by = c("rowid", "chrono1")) %>%
  # full_join(m3, by = c("rowid", "chrono1", "chrono2")) %>%
  # mutate(
  #   across(ends_with("_dubious"),
  #          function(x) if_else(str_detect(x, "\\?"), "TRUE", x)),
  #   across(ends_with("_dubious"),
  #          function(x) if_else(str_detect(x, "1"), "FALSE", x)),
  #   across(ends_with("_dubious"), as.logical)
  #   ) %>%
  mutate(chrono2 = str_c(chrono1, chrono2, sep = " ") #,
         # chrono3_label = str_c(chrono1, chrono2, chrono3, sep = " ")
  )

# binding chronology to the rest
clean_m <- m %>% select(-c(SBK:LgK_II_a)) %>%
  janitor::clean_names() %>%
  select(rowid,
         orig_id = uan_porcsa,
         point_x,
         point_y,
         region = okr,
         site = nazlok,
         site_note = alternaz,
         accuracy = presnostlo,
         surface = sber,
         altitude = nmv,
         cave = jesk,
         source = literatura,
         note = poznamka) %>%
  mutate(across(.cols = c(surface, cave), indicator_to_logical)) %>%
  right_join(m_chrono, by = "rowid") %>%
  # create unique ids
  mutate(rowid = str_pad(rowid, width = 4, side = "left", pad = "0"),
         rowid = str_c("M", rowid))

# cechy -------------------------------------------------------------------

c <- in_cechy %>%
  rowid_to_column() %>%
  # correct names to correspond to morava
  unite(col = "TRB_Boleraz/Saalzm", TRB_Saalzm, TRB_Boleraz, na.rm = TRUE) %>%
  unite(col = "TRB_Baalberg", TRB_Baalbg, TRB_Sirem, TRB_Early, na.rm = TRUE) %>%
  unite(col = "ENE_Proto", ENE_Michels, ENE_Schussen, ENE_JoK, na.rm = TRUE) %>%
  mutate(across(.cols = c(SBK:`TRB_Boleraz/Saalzm`), as.character),
         across(.cols = c('TRB_Boleraz/Saalzm',
                          TRB_Baalberg,
                          ENE_Proto), na_if, ""))

# level 1
c1 <- c %>% select(rowid, SBK, LgK, TRB, ENE) %>%
  pivot_longer(-rowid,
               names_to = "chrono1",
               values_to = "chrono1_dubious", values_drop_na = TRUE) %>%
  # keep certain sites only
  filter(chrono1_dubious == 1) %>%
  select(-chrono1_dubious)

# level 2
c2 <- c %>% select(rowid, SBK_Early, SBK_Late, LgK_I, LgK_II,
                   ENE_Proto,
                   TRB_Baalberg, `TRB_Boleraz/Saalzm`) %>%
  pivot_longer(-rowid,
               names_to = "chrono2",
               values_to = "chrono2_dubious", values_drop_na = TRUE) %>%
  separate(chrono2, into = c("chrono1", "chrono2"), sep = "_") %>%
  # keep certain sites only
  filter(!str_detect(chrono2_dubious, "\\?")) %>%
  select(-chrono2_dubious)

# # level 3
# c3 <- c %>% select(rowid, SBK_Early_II, SBK_Late_III, SBK_Late_IV, SBK_Late_V,
#                    LgK_II_a, LgK_II_b,
#                    ENE_JoK_I, ENE_JoK_II, ENE_JoK_III) %>%
#   pivot_longer(-rowid,
#                names_to = "chrono3",
#                values_to = "chrono3_dubious", values_drop_na = TRUE) %>%
#   separate(chrono3, into = c("chrono1", "chrono2", "chrono3"), sep = "_")

# full chronology
c_chrono <- full_join(c1, c2, by = c("rowid", "chrono1")) %>%
  # full_join(c3, by = c("rowid", "chrono1", "chrono2")) %>%
  # mutate(across(ends_with("_dubious"),
  #               function(x) if_else(str_detect(x, "\\?"), "TRUE", x)),
  #        across(ends_with("_dubious"),
  #               function(x) if_else(str_detect(x, "1"), "FALSE", x)),
  #        across(ends_with("_dubious"), as.logical)) %>%
  mutate(chrono2 = str_c(chrono1, chrono2, sep = " ") #,
         # chrono3_label = str_c(chrono1, chrono2, chrono3, sep = " ")
  )

# binding chronology to the rest
clean_c <- c %>% select(-c(SBK:`TRB_Boleraz/Saalzm`)) %>%
  janitor::clean_names() %>%
  select(rowid,
         orig_id = nejake_id,
         point_x,
         point_y,
         region = okr,
         site = nazlok,
         site_note = detail,
         accuracy = presnost,
         surface = sber,
         altitude = nmv,
         cave = jeskyn,
         source = zdroj,
         note = poznamka) %>%
  mutate(across(starts_with("point_"), as.numeric),
         altitude = as.numeric(altitude),
         across(.cols = c(surface, cave), as.character),
         across(.cols = c(surface, cave), indicator_to_logical)) %>%
  right_join(c_chrono, by = "rowid") %>%
  # unique ids
  mutate(rowid = str_pad(rowid, width = 4, side = "left", pad = "0"),
         rowid = str_c("C", rowid))

# bind data ---------------------------------------------------------------

# proper labels for chronology
labs_chrono1 <- c(
  "SBK" = "SBK",
  "LgK" = "LgK",
  "ENE" = "Early Ene.",
  "TRB" = "TRB"
  # "Retz" = "Retz"
)

labs_chrono2 <- c(
  "SBK Early" = "SBK Early",
  "SBK Late" = "SBK Late",
  "LgK I" = "LgK I",
  "LgK II" = "LgK II",
  # "LgK EPI" = "Epi LgK",
  "ENE Proto" = "Proto Ene.",
  # "ENE JoK" = "Jordanow",
  # "ENE Schussen" = "Schussenried",
  # "ENE Michels" = "Michelsberg",
  # "TRB Early" = "TRB Early",
  "TRB Baalberg" = "TRB Baalberg",
  # "TRB Sirem" = "TRB Sirem",
  "TRB Boleraz/Saalzm" = "TRB Boleraz/Saalzmund"
)

# labs_chrono3 <- c(
#   "SBK Early II" = "SBK II",
#   "SBK Late III" = "SBK III",
#   "SBK Late IV" = "SBK IV",
#   "SBK Late V" = "SBK V",
#   "LgK I a" = "LgK Ia",
#   "LgK I b" = "LgK Ib",
#   "LgK I c" = "LgK Ic",
#   "LgK II a" = "LgK IIa",
#   "LgK II b" = "LgK IIb",
#   "ENE JoK I" = "Jordanow I",
#   "ENE JoK II" = "Jordanow II",
#   "ENE JoK III" = "Jordanow III"
# )

# output levels list
list(chrono1 = labs_chrono1,
     chrono2 = labs_chrono2 # ,
     # chrono3 = labs_chrono3
) %>%
  write_rds(here("analysis/data/derived_data", "chrono_labels.RDS"))

# dataset -----------------------------------------------------------------

dataset_full <- bind_rows(b = clean_c, m = clean_m, .id = "orig") %>%
  mutate(chrono1_label = labs_chrono1[chrono1],
         chrono2_label = labs_chrono2[chrono2],
         # chrono3_label = labs_chrono3[chrono3_label],
         chrono1_label = factor(chrono1_label, levels = labs_chrono1),
         chrono2_label = factor(chrono2_label, levels = labs_chrono2),
         # chrono3_label = factor(chrono3_label, levels = labs_chrono3)
  ) %>%
  # remove cave sites
  filter(!cave) %>%
  select(-cave) %>%
  # remove spatial outlier (south bohemia)
  filter(site != "Vlkov nad Lužnicí-pískovna")

# transform dataset into a relational database
settlement_database <- list(base = NA,
                            chrono1 = NA,
                            chrono2 = NA)

settlement_database$chrono1 <- dataset_full %>%
  select(rowid, chrono1, chrono1_label) %>%
  filter(!is.na(chrono1_label), !is.na(chrono1)) %>%
  rename(chrono = chrono1, label = chrono1_label)

settlement_database$chrono2 <- dataset_full %>%
  select(rowid, chrono2, chrono2_label) %>%
  filter(!is.na(chrono2), !is.na(chrono2_label)) %>%
  rename(chrono = chrono2, label = chrono2_label)

settlement_database$base <- dataset_full %>%
  select(-starts_with("chrono")) %>%
  filter(!duplicated(rowid)) %>%
  select(rowid, everything())

# write data --------------------------------------------------------------

write_rds(settlement_database, here(dt_der, "settlements.RDS"))

write_excel_csv(dataset_full, here(dt_der, "settlements_spreadsheet.csv"))

# write geodatabase
settlement_database$base %>%
  st_as_sf(coords = c("point_x", "point_y"), crs = 5514) %>%
  # st_transform(crs = 4326) %>%
  st_write(here(dt_der, "settlements_sf.geojson"),
           delete_dsn = TRUE)

# plotting ------------------------------------------------------------------

# kraje <- RCzechia::kraje(resolution = "low")
# maska <- st_read(here(dt_der, "maps", "mask.geojson"))
#
# dataset_full %>%
#   st_as_sf(coords = c("point_x", "point_y"), crs = 5514) %>%
#   st_transform(crs = 4326) %>%
#   ggplot() +
#   geom_sf(data = kraje) +
#   geom_sf(shape = 4) +
#   theme_void()
#
# dataset_full %>%
#   st_as_sf(coords = c("point_x", "point_y"), crs = 5514) %>%
#   # st_difference(maska) %>%
#   ggplot() +
#   geom_sf(data = maska) +
#   geom_sf(shape = 4) +
#   # geom_sf_label(aes(label = site)) +
#   theme_void()
#
# dataset_full %>%
#   st_as_sf(coords = c("point_x", "point_y"), crs = 5514) %>%
#   st_intersection(st_transform(filter(kraje, NAZ_CZNUTS3 == "Jihočeský kraj"), 5514)) %>%
#   ggplot() +
#   geom_sf(data = kraje) +
#   geom_sf_label(aes(label = site)) +
#   geom_sf(shape = 4) +
#   theme_void()
#
# dataset_full %>% pull(chrono1) %>% factor() %>% levels()
# dataset_full %>% pull(chrono1_label) %>% factor() %>% levels()
#
# dataset_full %>% pull(chrono2) %>% factor() %>% levels()
# dataset_full %>% pull(chrono2_label) %>% factor() %>% levels()
