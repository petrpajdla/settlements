cont <- set_base$period2 %>%
  select(-period_label, -period, -chrono) %>%
  distinct(id, facet) %>%
  mutate(pres = 1L) %>%
  pivot_wider(names_from = facet, values_from = pres, values_fill = 0L) %>%
  pivot_longer(c(starts_with("TRB"), starts_with("SBK"), starts_with("LgK"), "Proto Ene."),
               names_to = "facet") %>%
  mutate(facet = factor(facet, levels = labs_chrono$facets)) %>%
  arrange(id, facet) %>%
  group_by(id) %>%
  mutate(prev = if_else(value == 1, lag(value), 0L),
         prev = if_else(is.na(prev), 0L, prev)) %>%
  ungroup(id) %>%
  select(-value) %>%
  mutate(variable = "cont") %>%
  rename(value = prev)

n_set <- set_base$period2 %>%
  select(-starts_with("period"), -chrono) %>%
  add_region() %>%
  group_by(reg, facet) %>%
  summarise(sum = n())

cont %>%
  add_region() %>%
  select(-variable) %>%
  group_by(reg, facet) %>%
  summarise(n = sum(value, na.rm = TRUE)) %>%
  full_join(n_set) %>%
  mutate(
    n = if_else(is.na(n), 0L, n),
    sum = if_else(is.na(sum), 0L, sum),
    perc = (n / sum) * 100,
    perc = if_else(perc == 0 | is.nan(perc), NA_real_, perc),
    reg = if_else(reg == "B", "East Bohemia", "Morava river catchment"),
    # chrono_label = labs_chrono$chrono2[chrono],
    # chrono_label = factor(chrono_label, levels = labs_chrono$chrono2)
    facet = factor(facet, levels = labs_chrono$facets)
  ) %>%
  ggplot(aes(facet, perc)) +
  geom_col(color = "black", fill = "white",
           position = "dodge", show.legend = FALSE) +
  facet_wrap(vars(reg), ncol = 1) +
  labs(x = "Pottery group", y = "Continuity of settlements (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))

ggsave(here::here("groups_chrono_continuity2.svg"), width = 7, height = 5)
