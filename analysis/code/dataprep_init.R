# # Project "Settlements"
# # Script nr. 0
# # DATA PREPARATION
# # author: Petr Pajdla
# # Prepares data for the analysis from raw spreadsheet formats
#
# library(here)
# library(tidyverse)
# library(sf)
# library(googledrive)
#
# dt_raw <- "analysis/data/raw_data"
# dt_der <- "analysis/data/derived_data"
#
# # funs --------------------------------------------------------------------
#
# # 1 / 0 indicators to logical TRUE / FALSE
# indicator_to_logical <- function(x) {
#   x <- as.character(x)
#   x <- if_else(str_detect(x, "1"), "TRUE", x)
#   x <- if_else(str_detect(x, "0"), "FALSE", x)
#   x <- if_else(str_detect(x, "\\?"), "TRUE", x)
#   return(as.logical(x))
# }
#
# # creates chronologies from indicators
# create_chrono <- function(x, chrono) {
#   stopifnot(chrono %in% c(1, 2, 3))
#
#   if (chrono == 1) {
#     chrono_cols <- c("SBK", "LgK", "ENE", "TRB")
#   } else if (chrono == 2) {
#     chrono_cols <- c("SBK1", "SBK2", "LgK1", "LgK2", "ENE0",
#                      "TRB1", "TRB2")
#   } else if (chrono == 3) {
#     chrono_cols = c("ENE0_Epilengyel", "ENE0_Ludanice",
#                     "ENE0_Schussenried", "ENE0_Michelsberg", "ENE0_Jordanow")
#   }
#
#   x %>%
#     select(id, any_of(chrono_cols)) %>%
#     pivot_longer(-id,
#                  names_to = "chrono",
#                  values_to = "chrono_dubious", values_drop_na = TRUE) %>%
#     # filter only certain sites
#     filter(!str_detect(chrono_dubious, "\\?")) %>%
#     select(-chrono_dubious)
# }
#
# # create dataset
# create_data_set <- function(x, pad) {
#   stopifnot(pad %in% c("M", "B"))
#
#   res <- list(
#     data = NA,
#     chrono1 = NA,
#     chrono2 = NA
#   )
#
#   # unique ids
#   dt <- x %>% mutate(id = str_pad(rowid, width = 4, side = "left", pad = "0"),
#                      id = str_c(pad, id),
#                      cave = as.numeric(cave)) %>%
#     filter(cave == 0)
#
#   # create chronologies
#   res$chrono1 <- dt %>% create_chrono(1) %>%
#     mutate(label = chrono_labels$chrono1[chrono],
#            label = factor(label, levels = chrono_labels$chrono1),
#            chrono = factor(chrono, levels = names(chrono_labels$chrono1)))
#
#   res$chrono2 <- dt %>% create_chrono(2) %>%
#     mutate(label = chrono_labels$chrono2[chrono],
#            label = factor(label, levels = chrono_labels$chrono2),
#            chrono = factor(chrono, levels = names(chrono_labels$chrono2)))
#
#   # clean data set
#   res$data <- dt %>% janitor::clean_names() %>%
#     select(id,
#            orig_id,
#            point_x,
#            point_y,
#            region = okr,
#            site = nazlok,
#            site_note,
#            accuracy,
#            surface = sber,
#            altitude = nmv,
#            source,
#            note = poznamka) %>%
#     mutate(across(.cols = c(surface), indicator_to_logical),
#            across(starts_with("point_"), as.numeric),
#            altitude = as.numeric(altitude))
#
#   return(res)
# }
#
# # data input --------------------------------------------------------------
#
# in_morava <- readxl::read_excel(here(dt_raw, "morava_pp.xlsx"))
#
# in_cechy <- readxl::read_excel(here(dt_raw, "cechy_pp.xlsx"))
#
#
# # data preparation --------------------------------------------------------
#
# # morava
# m <- in_morava %>%
#   rowid_to_column() %>%
#   # correct chrono phase labels
#   mutate(ENE0_Epilengyel = LgK_EPI,
#          ENE0_Ludanice = Ludanice) %>%
#   unite(col = "SBK1", `SBK_II-III`, SBK_III, na.rm = TRUE) %>%
#   unite(col = "ENE0", LgK_EPI, Ludanice, na.rm = TRUE) %>%
#   unite(col = "TRB1", TRB_Baalberg, TRB_I, na.rm = TRUE) %>%
#   unite(col = "LgK_II", LgK_II, Brodzany, na.rm = TRUE) %>%
#   rename(SBK2 = SBK_IV,
#          LgK1 = LgK_I,
#          LgK2 = LgK_II,
#          TRB2 = TRB_Boleraz,
#          # rename cols to o with cechy
#          orig_id = uan_PORCSA,
#          site_note = ALTERNAZ,
#          accuracy = PRESNOSTLO,
#          cave = jesk,
#          source = Literatura) %>%
#   mutate(across(.cols = c(SBK:LgK_II_a), as.character),
#          across(.cols = c(SBK1,
#                           ENE0,
#                           TRB1,
#                           LgK2), na_if, ""),
#          # artificially create Early Ene. group for Morava?
#          ENE = ENE0)
#
# # cechy
# c <- in_cechy %>%
#   rowid_to_column() %>%
#   mutate(ENE0_Jordanow = ENE_JoK,
#          ENE0_Schussenried = ENE_Schussen,
#          ENE0_Michelsberg = ENE_Michels) %>%
#   # correct chrono phase labels
#   unite(col = "TRB2", TRB_Saalzm, TRB_Boleraz, na.rm = TRUE) %>%
#   unite(col = "TRB1", TRB_Baalbg, TRB_Sirem, TRB_Early, na.rm = TRUE) %>%
#   unite(col = "ENE0", ENE_Michels, ENE_Schussen, ENE_JoK, na.rm = TRUE) %>%
#   rename(SBK1 = SBK_Early,
#          SBK2 = SBK_Late,
#          LgK1 = LgK_I,
#          LgK2 = LgK_II,
#          # rename cols to go with morava
#          orig_id = `nějaké ID`,
#          site_note = detail,
#          accuracy = Přesnost,
#          cave = jeskyň,
#          source = ZDROJ) %>%
#   mutate(across(.cols = c(SBK:TRB2), as.character),
#          across(.cols = c(TRB2,
#                           TRB1,
#                           ENE0), na_if, ""))
#
#
# # proper labels for chronology ------------------------------------------
#
# chrono_labels <- list(
#   chrono1 = c(
#     "SBK" = "SBK",
#     "LgK" = "LgK",
#     "ENE" = "Proto Ene.",
#     "TRB" = "TRB"),
#   chrono2 = c(
#     "SBK1" = "SBK Early",
#     "SBK2" = "SBK Late",
#     "LgK1" = "LgK I",
#     "LgK2" = "LgK II",
#     "ENE0" = "Proto Ene.",
#     "ENE1" = "Jordanow",
#     "ENE2" = "Epi LgK",
#     "TRB1" = "TRB Baalberge",
#     "TRB2" = "TRB Boleraz/Saalzmuende"),
#   facets = c(
#     "SBK Early",
#     "SBK Late/LgK I",
#     "LgK II",
#     "Proto Ene.",
#     "TRB Baalberge",
#     "TRB Boleraz/Saalzmuende"),
#   # labels for time slices
#   periods = c(
#     "p5.0" = "5.0 – 4.8 k",
#     "p4.8" = "4.8 – 4.6 k",
#     "p4.6" = "4.6 – 4.4 k",
#     "p4.4" = "4.4 – 4.2 k",
#     "p4.2" = "4.2 – 4.0 k",
#     "p4.0" = "4.0 – 3.8 k",
#     "p3.8" = "3.8 – 3.6 k",
#     "p3.6" = "3.6 – 3.4 k",
#     "p3.4" = "3.4 – 3.2 k"))
#
# write_rds(chrono_labels, here(dt_der, "chrono_labels.RDS"))
#
# # dataset -----------------------------------------------------------------
#
# clean_m <- create_data_set(m, "M")
# clean_c <- create_data_set(c, "B")
#
# set_base <- list(base = NA,
#                  chrono1 = NA,
#                  chrono2 = NA,
#                  period1 = NA,
#                  period2 = NA)
#
# # base metadata
# set_base$base <- bind_rows(clean_c$data,
#                            clean_m$data) %>%
#   # remove spatial outlier (south bohemia)
#   filter(site != "Vlkov nad Lužnicí-pískovna")
#
# # first facet of chronology
# set_base$chrono1 <- bind_rows(clean_c$chrono1,
#                               clean_m$chrono1) %>%
#   filter(id %in% set_base$base$id)
#
# set_base$period1 <- set_base$chrono1 %>%
#   mutate("p5.0" = if_else(chrono  == "SBK", TRUE, FALSE),
#          "p4.8" = if_else(chrono  %in% c("SBK", "LgK"), TRUE, FALSE),
#          # "p4.8" = if_else(chrono  == "LgK" & str_detect(id, "M"), TRUE, p4.8),
#          "p4.6" = if_else(chrono  == "LgK", TRUE, FALSE),
#          "p4.4" = if_else(chrono  %in% c("LgK", "ENE"), TRUE, FALSE),
#          "p4.2" = if_else(chrono  == "ENE", TRUE, FALSE),
#          "p4.0" = if_else(chrono  == "ENE", TRUE, FALSE),
#          "p3.8" = if_else(chrono  == "TRB", TRUE, FALSE),
#          "p3.6" = if_else(chrono  == "TRB", TRUE, FALSE),
#          "p3.4" = if_else(chrono  == "TRB", TRUE, FALSE)) %>%
#   pivot_longer(cols = starts_with("p"), names_to = "period") %>%
#   filter(value) %>%
#   select(-value) %>%
#   mutate(period_label = chrono_labels$periods[period],
#          period_label = factor(period_label, levels = chrono_labels$periods))
#
# # second facet of chronology
# set_base$chrono2 <- bind_rows(clean_c$chrono2,
#                               clean_m$chrono2) %>%
#   filter(id %in% set_base$base$id) %>%
#   mutate(facet = if_else(chrono %in% c("SBK2", "LgK1"),
#                          "SBK Late/LgK I",
#                          as.character(label)),
#          label = if_else(str_detect(id, "^B") & chrono == "ENE0",
#                          "Jordanow", as.character(label)),
#          label = if_else(str_detect(id, "^M") & chrono == "ENE0",
#                          "Epi LgK", label),
#          facet = factor(facet, levels = chrono_labels$facets),
#          label = factor(label, levels = chrono_labels$chrono2))
#
# set_base$period2 <- set_base$chrono2 %>%
#   mutate("p5.0" = if_else(chrono  %in% c("SBK1", "SBK2"), TRUE, FALSE),
#          "p4.8" = if_else(chrono  %in% c("SBK1", "SBK2"), TRUE, FALSE),
#          "p4.8" = if_else(chrono  == "LgK2" & str_detect(id, "B"), TRUE, p4.8),
#          "p4.8" = if_else(chrono  == "LgK1" & str_detect(id, "M"), TRUE, p4.8),
#          "p4.6" = if_else(chrono  == "LgK2", TRUE, FALSE),
#          "p4.6" = if_else(chrono  == "LgK1" & str_detect(id, "M"), TRUE, p4.6),
#          "p4.4" = if_else(chrono  %in% c("LgK2", "ENE0"), TRUE, FALSE),
#          "p4.2" = if_else(chrono  == "ENE0", TRUE, FALSE),
#          "p4.0" = if_else(chrono  == "ENE0" & str_detect(id, "B"), TRUE, FALSE),
#          "p3.8" = if_else(chrono  == "TRB1", TRUE, FALSE),
#          "p3.6" = if_else(chrono  %in% c("TRB1", "TRB2"), TRUE, FALSE),
#          "p3.4" = if_else(chrono  == "TRB2", TRUE, FALSE)) %>%
#   pivot_longer(cols = starts_with("p"), names_to = "period") %>%
#   filter(value) %>%
#   select(-value) %>%
#   mutate(period_label = chrono_labels$periods[period],
#          period_label = factor(period_label, levels = chrono_labels$periods))
#
# # geodatabase
# set_spat <- set_base$base %>%
#   select(id, site, accuracy, surface, altitude, starts_with("point")) %>%
#   st_as_sf(coords = c("point_x", "point_y"), crs = 5514)
#
# # removing columns now in geodatabase from orig base
#
# set_base$base <- set_base$base %>%
#   select(-starts_with("point"),
#          -accuracy,
#          -surface,
#          -altitude)
#
# # write data --------------------------------------------------------------
#
# # write_rds(set_base, here(dt_der, "settlements.RDS"))
#
# # write geodatabase
# # set_spat %>%
# #   st_write(here(dt_der, "settlements_sf.geojson"),
# #            delete_dsn = TRUE)
#
# # write separate files for manual editing etc.
# dt_manual <- paste0(dt_der, "/output_manual_edit")
# dir.create(dt_manual)
#
# set_base$base %>%
#   arrange(id) %>%
#   write_csv(here(dt_manual, "base.csv"))
#
# set_base$chrono1 %>%
#   arrange(id) %>%
#   write_csv(here(dt_manual, "chrono1.csv"))
#
# set_base$chrono2 %>%
#   arrange(id) %>%
#   write_csv(here(dt_manual, "chrono2.csv"))
#
# set_base$period1 %>%
#   arrange(id) %>%
#   write_csv(here(dt_manual, "period1.csv"))
#
# set_base$period2 %>%
#   arrange(id) %>%
#   write_csv(here(dt_manual, "period2.csv"))
#
# # set_spat %>%
# #   st_write(here(dt_manual, "settlements_sf.geojson"),
# #            delete_dsn = TRUE)
#
# set_spat_coords <- set_spat %>%
#   st_coordinates() %>%
#   as_tibble()
#
# bind_cols(st_drop_geometry(set_spat), set_spat_coords) %>%
#   arrange(id) %>%
#   write_csv(here(dt_manual, "spatial.csv"))
#
# # upload files to GD ------------------------------------------------------
#
# # list files to upload
# files_local <- list.files(dt_manual, full.names = TRUE)
#
# files_csv <- str_subset(files_local, "csv$")
# # files_other <- str_subset(files_local, "[^csv]$")
#
# # path to correct drive directory
# drive_path <- "~/settlements/data/database/"
#
# drive_csv <- map(files_csv, ~ drive_upload(.x, drive_path,
#                                            type = "spreadsheet"))
# # drive_other <- map(files_other, ~ drive_upload(.x, drive_path))
#
# # check drive if files exist
# drive_ls(drive_path)
#
#
#
