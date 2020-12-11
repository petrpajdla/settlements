# Project "Settlements"
# Script nr. 0
# PLOT SETTLEMENTS
# author: Petr Pajdla
# Plot settlement distribution in different periods

library(tidyverse)
library(here)

dt_raw <- "analysis/data/raw_data"
dt_der <- "analysis/data/derived_data"

chrono <- read_csv(here(dt_raw, "chronotable.csv"))
set <- read_rds(here(dt_der, "settlements.RDS"))
chrono_lvls <- read_rds(here(dt_der, "chrono_labels.RDS"))

chrono %>%
  filter(chrono_lvl == 2) %>%
  mutate(chrono = factor(chrono, levels = chrono_lvls$chrono2)) %>%
  ggplot(aes(y = chrono)) +
  geom_point(aes(x = from, color = chrono)) +
  geom_point(aes(x = to, color = chrono)) +
  geom_linerange(aes(xmin = from, xmax = to, color = chrono)) +
  facet_wrap(~region, nrow = 2, scales = "free_y") +
  theme_minimal()

chrono_yvals <- tribble(
  ~region, ~chrono, ~ymin, ~ymax,
  "nw", "SBK", "SBK Early", "SBK Late",
  "nw", "LgK", "LgK I", "Epi LgK",
  "nw", "Early Ene.", "Jordanow", "Michelsberg",
  "nw", "TRB", "TRB Early", "TRB Boleraz/Saalzmund",
)

chrono_rects <- chrono %>% filter(chrono_lvl == 1) %>%
  left_join(chrono_yvals) %>%
  mutate(
    # chrono = factor(chrono, levels = chrono_lvls$chrono1),
    ymin = factor(ymin, levels = chrono_lvls$chrono2),
    ymax = factor(ymax, levels = chrono_lvls$chrono2))



geom_point(data = filter(chrono, chrono_lvl == 2),
           aes(x = from, y = chrono, color = chrono)) +
  geom_point(data = filter(chrono, chrono_lvl == 2),
             aes(x = to, y = chrono, color = chrono)) +
  geom_linerange(data = filter(chrono, chrono_lvl == 2),
                 aes(xmin = from, xmax = to, y = chrono, color = chrono))
