# Project "Settlements"
# Script nr.
# DISTANCE TO FENCED SETTLEMENT
# author: Petr Pajdla
# Calculated separately for the speed

# devtools::load_all(".")
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

# settlements spatial by periods
set_spat_1 <- set_spat %>%
  right_join(set_base$period1) %>%
  mutate(reg = if_else(str_detect(id, "^B"), "b", "m"))

set_spat_2 <- set_spat %>%
  right_join(set_base$period2) %>%
  mutate(reg = if_else(str_detect(id, "^B"), "b", "m"))

# fenced settlements
b <- readxl::read_excel(
  here::here("analysis/data/raw_data/ohrazena_sidliste.xlsx"),
  sheet = "b")

m <- readxl::read_excel(
  here::here("analysis/data/raw_data/ohrazena_sidliste.xlsx"),
  sheet = "m")

# point layer
# period I - pottery tradtions
fortif_p1 <- bind_rows(b = b, m = m, .id = "reg") %>%
  separate(abbrv_p2, into = c("p2_1", "p2_2"), sep = ";") %>%
  pivot_longer(c(p2_1, p2_2), values_to = "abbrv_p2") %>%
  select(-name) %>%
  filter(!is.na(abbrv_p2)) %>%
  mutate(abbrv_p2 = str_trim(abbrv_p2, "both"),
         "p5.0" = if_else(abbrv_p1  == "SBK", TRUE, FALSE),
         "p4.8" = if_else(abbrv_p1  %in% c("SBK", "LgK"), TRUE, FALSE),
         "p4.6" = if_else(abbrv_p1  == "LgK", TRUE, FALSE),
         "p4.4" = if_else(abbrv_p1  %in% c("LgK", "ENE"), TRUE, FALSE),
         "p4.2" = if_else(abbrv_p1  == "ENE", TRUE, FALSE),
         "p4.0" = if_else(abbrv_p1  == "ENE", TRUE, FALSE),
         "p3.8" = if_else(abbrv_p1  == "TRB", TRUE, FALSE),
         "p3.6" = if_else(abbrv_p1  == "TRB", TRUE, FALSE),
         "p3.4" = if_else(abbrv_p1  == "TRB", TRUE, FALSE)) %>%
  pivot_longer(cols = starts_with("p"), names_to = "period") %>%
  filter(value) %>%
  select(-value) %>%
  mutate(period_label = chrono_labels$periods[period],
         period_label = factor(period_label, levels = chrono_labels$periods)) %>%
  distinct(reg, lokalita, period, .keep_all = TRUE) %>%
  st_as_sf(coords = c("X", "Y")) %>%
  st_set_crs(5514)

# period II - pottery groups
fortif_p2 <- bind_rows(b = b, m = m, .id = "reg") %>%
  separate(abbrv_p2, into = c("p2_1", "p2_2"), sep = ";") %>%
  pivot_longer(c(p2_1, p2_2), values_to = "abbrv_p2") %>%
  select(-name) %>%
  filter(!is.na(abbrv_p2)) %>%
  mutate(abbrv_p2 = str_trim(abbrv_p2, "both"),
         "p5.0" = if_else(abbrv_p2  %in% c("SBK1", "SBK2"), TRUE, FALSE),
         "p4.8" = if_else(abbrv_p2  %in% c("SBK1", "SBK2", "LgK1"), TRUE, FALSE),
         "p4.8" = if_else(abbrv_p2 == "LgK1" & reg == "b", TRUE, p4.8),
         "p4.6" = if_else(abbrv_p2  %in% c("LgK1", "LgK2"), TRUE, FALSE),
         "p4.4" = if_else(abbrv_p2  %in% c("LgK2", "ENE0"), TRUE, FALSE),
         "p4.2" = if_else(abbrv_p2  == "ENE0", TRUE, FALSE),
         "p4.0" = if_else(abbrv_p2  == "ENE0" & reg != "m", TRUE, FALSE),
         "p3.8" = if_else(abbrv_p2  == "TRB1", TRUE, FALSE),
         "p3.6" = if_else(abbrv_p2  %in% c("TRB1", "TRB2"), TRUE, FALSE),
         "p3.4" = if_else(abbrv_p2  == "TRB2", TRUE, FALSE)) %>%
  pivot_longer(cols = starts_with("p"), names_to = "period") %>%
  filter(value) %>%
  select(-value) %>%
  mutate(period_label = chrono_labels$periods[period],
         period_label = factor(period_label, levels = chrono_labels$periods)) %>%
  distinct(reg, lokalita, period, .keep_all = TRUE) %>%
  st_as_sf(coords = c("X", "Y")) %>%
  st_set_crs(5514)


# distance function for fortified settlements -----------------------------
spatial_distance <- function(x, y) {
  st_distance(x, y) %>%
    as_tibble() %>%
    bind_cols(id = x$id) %>%
    pivot_longer(cols = starts_with("V")) %>%
    mutate(dist = as.double(value),
           dist = dist * 1e-3) %>%
    select(-name, -value) %>%
    group_by(id) %>%
    summarize(min_dist = min(dist))
}

fortif_dist <- function(x, y) {

  len <- length(names(chrono_labels$periods))

  res <- vector("list", len)
  names(res) <- names(chrono_labels$periods)

  regs <- c("b", "m")

  for (i in seq_along(res)) {
    per <- names(res)[i]

    x1 <- x %>% filter(period == per)
    y1 <- y %>% filter(period == per)

    reg_res <- vector("list", 2)
    names(reg_res) <- regs

    for (j in seq_along(regs)) {

      x2 <- x1 %>% filter(reg == regs[j])
      y2 <- y1 %>% filter(reg == regs[j])

      if (nrow(x2) == 0 | nrow(y2) == 0) {
        reg_res[[j]] <- tribble(~"id", ~"min_dist")
      } else {
        reg_res[[j]] <- spatial_distance(x2, y2)
      }
    }
    res[[i]] <- bind_rows(reg_res)
  }

  return(res)

}

# results -----------------------------------------------------------------

fd1 <- fortif_dist(set_spat_1, fortif_p1) %>%
  bind_rows(.id = "period") %>%
  mutate(reg = if_else(str_detect(id, "^B"), "b", "m"))

fd2 <- fortif_dist(set_spat_2, fortif_p2) %>%
  bind_rows(.id = "period") %>%
  mutate(reg = if_else(str_detect(id, "^B"), "b", "m"))

fdistances <- list(traditions = fd1,
     groups = fd2)

out_dir <- here::here(derived_data, "fortif")
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

write_rds(fdistances, paste0(out_dir, "/distance_fortif.RDS"))

# fd1 %>%
#   ggplot(aes(min_dist, period)) +
#   geom_boxplot() +
#   facet_wrap(~reg)
#
# fd2 %>%
#   ggplot(aes(min_dist, period)) +
#   geom_boxplot() +
#   facet_wrap(~reg)

# maps
# set_spat_1 %>%
#   ggplot() +
#   geom_sf(data = mask, fill = "gray80", color = NA, alpha = 0.4) +
#   # geom_sf(data = admin0, linetype = 1, fill = NA, color = "gray60", alpha = 0.4) +
#   geom_sf(alpha = 0.1, color = "gray") +
#   geom_sf(data = fortif_p1, shape = 4) +
#   # ggsflabel::geom_sf_text_repel(data = fortif_p1, aes(label = str_extract(lokalita, "^.{2}"))) +
#   facet_wrap(vars(period_label)) +
#   theme_void()
#
# set_spat_2 %>%
#   ggplot() +
#   geom_sf(data = mask, fill = "gray80", color = NA, alpha = 0.4) +
#   # geom_sf(data = admin0, linetype = 1, fill = NA, color = "gray60", alpha = 0.4) +
#   geom_sf(alpha = 0.1, color = "gray") +
#   geom_sf(data = fortif_p2, shape = 4) +
#   # ggsflabel::geom_sf_text_repel(data = fortif_p2, aes(label = str_extract(lokalita, "^.{2}"))) +
#   facet_wrap(vars(period_label)) +
#   theme_void()
