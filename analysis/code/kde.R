# Project "Settlements"
# Script nr.
# GET AND PLOT KDE VALUES FOR DIFFERENT SETTLEMENTS
# author: Petr Pajdla
#

library(tidyverse)
library(sf)
library(here)
library(spatstat)

dt_der <- "analysis/data/derived_data"
dt_maps <- here(dt_der, "/maps/")

pth_plots <- here("analysis/figures/temp/")

# read data ---------------------------------------------------------------
# mask
mask <- st_read(paste0(dt_maps, "mask.geojson"))

# chrono labels
labs_chrono <- read_rds(here(dt_der, "chrono_labels.RDS"))

# settlements
set_base <- read_rds(here(dt_der, "settlements.RDS"))

set1<- st_read(here(dt_der, "settlements_sf.geojson")) %>%
  right_join(set_base$chrono1, by = "rowid")

set2<- st_read(here(dt_der, "settlements_sf.geojson")) %>%
  right_join(set_base$chrono2, by = "rowid")

# kde ---------------------------------------------------------------------

kdestimate <- function(x, mask, markscol = "label", plot = FALSE) {
  coords <- st_coordinates(x)
  mask <- as.owin(mask)
  marks <- select(st_drop_geometry(x), all_of(markscol))

  pppset <- ppp(x = coords[, 1], y = coords[, 2],
                window = mask,
                marks = marks)

  splitpppset <- split.ppp(pppset)

  if (plot) {
    plot(density.splitppp(splitpppset), main = "")
  } else {
    dens <- density.splitppp(splitpppset, at = "points") %>%
      map(as_tibble) %>%
      bind_rows(.id = "label") %>%
      group_by(label) %>%
      nest()

    set <- x %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      select(rowid, label) %>%
      group_by(label) %>%
      nest()

    left_join(set, dens, by = "label") %>%
      mutate(data = pmap(list(data.x, data.y), bind_cols)) %>%
      select(label, data) %>%
      unnest(data) %>%
      mutate(label = factor(label, levels = levels(marks[, 1]))) %>%
      rename(kde = value)
  }
}

# get kde, level 1
kde1 <- kdestimate(set1, mask)
kdestimate(set1, mask, plot = TRUE)

# get kde, level 2
kde2 <- kdestimate(set2, mask)
kdestimate(set2, mask, plot = TRUE)


# output ------------------------------------------------------------------

write_rds(list(chrono1 = kde1,
               chrono2 = kde2),
          here(dt_der, "settlements_results.RDS"))

# plots -------------------------------------------------------------------

# kde1
full_join(set1, kde1, by = c("rowid", "label")) %>%
  ggplot() +
  geom_sf(data = mask) +
  geom_sf(aes(color = kde)) +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(vars(label)) +
  theme_void()
ggsave(paste0(pth_plots, "kde1.png"), scale = 3)

# hist
full_join(set1, kde1, by = c("rowid", "label")) %>%
  ggplot(aes(kde)) +
  geom_histogram(fill = NA, color = "black") +
  facet_grid(rows = vars(label), cols = vars(orig)) +
  theme_bw()
ggsave(paste0(pth_plots, "kde1_hist.png"), scale = 2)

# kde 2
full_join(set2, kde2, by = c("rowid", "label")) %>%
  ggplot() +
  geom_sf(data = mask) +
  geom_sf(aes(color = kde)) +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(vars(label), nrow = 2) +
  theme_void()
ggsave(paste0(pth_plots, "kde2.png"), scale = 3)

# hist
full_join(set2, kde2, by = c("rowid", "label")) %>%
  ggplot(aes(kde)) +
  geom_histogram(col = "black", fill = NA) +
  facet_grid(rows = vars(label), cols = vars(orig)) +
  theme_bw()
ggsave(paste0(pth_plots, "kde2_hist.png"), scale = 2)
