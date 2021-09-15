#' Distance to a source of a raw material
#'
#' For simple feature geometries in projected coordinate reference system,
#' euclidean distance (in km) is returned between features of \code{from}
#' and \code{to} categorized by levels specified in \code{rm}.
#'
#' @param from \code{sf} object of geometry type \code{POINTS} in projected
#'     CRS (settlements).
#' @param to \code{sf} object of geometry type \code{POINT} or \code{LINESTRING}
#'     in projected CRS (raw material locations).
#'     Each raw material can have multiple locations, same raw materials are
#'     identified by identical label in column specified by \code{rm} argument.
#' @param rm Column name of \code{to} to use as a grouping variable
#'     (identical raw material).
#' @param id Column name of \code{from} containing unique identifiers.
#'
#' @return A \code{tibble} containing min, max, mean and median distance
#'     for each raw material (level of \code{rm} column) to an observation
#'     specified in \code{from} (settlement) is returned.
#'
dist_to_rm <- function(from, to, rm, id = "id") {

  # set units to km
  to_km <- function(x) {
    units(x) <- "km"
    return(x)
  }

  lvls <- levels(factor(pull(to, {{ rm }} )))

  res <- vector("list", length(lvls))
  names(res) <- lvls

  for (i in seq_along(lvls)) {

    rm_flt <- filter(to, .data[[rm]] == lvls[i])

    res[[lvls[i]]] <- st_distance(from, rm_flt) %>%
      as_tibble() %>%
      # first col must contain ids
      bind_cols(st_drop_geometry(select(from, all_of(id)))) %>%
      pivot_longer(-id) %>%
      # to kilometers
      mutate(value = to_km(value)) %>%
      group_by(id) %>%
      summarize(min = min(value),
                mean = mean(value),
                median = median(value),
                max = max(value), .groups = "drop")

    # print(paste(lvls[i], "done,", i, "of", length(lvls)))

  }

  res %>%
    bind_rows(.id = "rm")

}

#' Plot linear model for distance to raw material vs settlements kde
#'
#' @param x Long results object.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_dist_lm <- function(x) {
  x %>% filter(variable == "settlements_kde") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    add_period_label() %>%
    select(id, period_label, settlements_kde) %>%
    left_join(rm_dist) %>%
    mutate(region = str_extract(id, "^.")) %>%
    filter(rm %in% c("ad", "zelesice", "pmd", "kl"),
           region == "M") %>%
    ggplot(aes(mean, settlements_kde)) +
    geom_point(alpha = 0.1) +
    stat_smooth(method = "lm", se = FALSE) +
    facet_grid(vars(period_label), vars(rm)) +
    labs(x = "Mean distance to raw material source (km)",
         y = "KDE") +
    theme_minimal() +
    theme(strip.text.y = element_text(angle = 0),
          panel.border = element_rect(color = "black", fill = NA))
}

#' Calculate correlation between set. KDE and dist. to raw materials
#'
#' @param x Long results object.
#'
#' @return A \code{kable} object.
#' @export
#'
#' @examples
tab_dist_lm <- function(x) {
  x %>% filter(variable == "settlements_kde") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    add_period_label() %>%
    left_join(rm_dist) %>%
    mutate(region = str_extract(id, "^.")) %>%
    filter(rm %in% c("ad", "zelesice", "pmd", "kl"),
           region == "M") %>%
    group_by(period_label, rm) %>%
    summarise(cor = cor(min, settlements_kde)) %>%
    pivot_wider(id_cols = period_label, names_from = rm, values_from = cor) %>%
    rename(period = period_label) %>%
    knitr::kable(caption = "Table of correlations between minimal distance to
                 raw material and settlement density, pottery traditions.") %>%
    kableExtra::kable_styling(full_width = FALSE)
}

#' Filter period-relevant distances to raw material sources
#'
#' @param x Input period data \code{tibble}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
filter_rel_dist_rm <- function(x) {
  rm_dist %>% group_by(id, rm) %>%
    summarise(mean = mean(mean)) %>%
    mutate(reg = if_else(str_detect(id, "^B"), "b", "m")) %>%
    left_join(select(x, period, id)) %>%
    left_join(rm_relevance) %>%
    filter(relevance > 0) %>%
    mutate(relevance = factor(relevance, levels = c("0", "0.5", "1")),
           label = labs_chrono$periods[period],
           label = factor(label, levels = labs_chrono$periods))
}

#' Plot period-relevant distance to raw material sources in selected region
#'
#' @param x A \code{tibble}.
#' @param region Region abbreviation.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_rm_reg_dist <- function(x, region = c("m", "b")) {
  region <- match.arg(region)

  x %>%
    filter(reg == region) %>%
    ggplot(aes(label, mean, color = relevance)) +
    geom_violin(show.legend = FALSE) +
    geom_boxplot(width = 0.2, show.legend = FALSE) +
    scale_color_manual(values = c("gray", "black")) +
    facet_wrap(vars(rm), scales = "free_y") +
    labs(x = element_blank(), y = "Mean distance to raw material source (km)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 80, hjust = 1))
}

#' Count distance to period-relevant raw material sources
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
count_dist_rel_rm <- function(x) {
  x %>%
    mutate(type = unname(rm_types_tab[rm])) %>%
    filter(relevance == 1) %>%
    group_by(period, reg, id, type) %>%
    summarise(mean = mean(mean))
}

#' Prepare period-relevant distance to the generic type of raw material source
#'
#' Functions with \code{prep_} prefix prepare the data into the long format for
#' binding with results \code{tibble}.
#'
#' @param x A \code{tibble}.
#'
#' @return A long results \code{tibble}. Raw materials generalized into
#'     \code{Chipped} and \code{Polished} categories.
#' @export
#'
#' @examples
prep_dist_rel_rm <- function(x) {
  x %>% count_dist_rel_rm() %>%
    ungroup(c(period, reg, id)) %>%
    mutate(type = case_when(
      type == "Chipped" ~ "rm_dist_chipped",
      type == "Polished" ~ "rm_dist_polished"
    )) %>%
    select(id, period, variable = type, value = mean)
}

#' Plot distance to generalized raw material sources.
#'
#' @param x A \code{tibble}.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_dist_rel_rm <- function(x) {
  x %>%
    mutate(label = unname(labs_chrono$periods[period]),
           label = factor(label, levels = unname(labs_chrono$periods))) %>%
    ggplot(aes(label, mean, fill = reg)) +
    geom_violin() +
    scale_fill_manual(values = mycol) +
    facet_wrap(~type) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 80, hjust = 1)) +
    labs(x = element_blank(), y = "Mean dist. to RM source (km)")
}

#' Relabel type of raw material
#'
#' @param x \code{simple features} object.
#'
#' @return \code{simple features} object.
#' @export
#'
#' @examples
add_rm_type <- function(x) {
  x %>% mutate(label = if_else(orig == "l", "Chipped", "Polished"),
               rm = str_to_lower(rm))
}
