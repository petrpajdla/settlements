#' NA to zero
#'
#' Changes NA value to a zero (0).
#'
#' @param x An atomic vector.
#'
#' @return An atomic numeric vector.
#' @export
#'
#' @examples
na2null <- function(x) {
  x[is.na(x)] <- 0
  x
}

# not_na <- function(x) {
#   !is.na(x)
# }


# Labelling functions ---------------------------------------------------

#' Add period labels
#'
#' @param x A \code{tibble}.
#'
#' @return A \code{tibble}.
#' @export
add_period_label <- function(x) {
  x %>% mutate(period_label = unname(labs_chrono$periods[period]),
               period_label = factor(period_label, levels = unname(labs_chrono$periods)))
}

#' Add region labels
#'
#' @param x A \code{tibble}.
#'
#' @return A \code{tibble}.
#' @export
add_region <- function(x) {
  x %>% mutate(reg = str_extract(id, "^."),
               reg = factor(reg, levels = c("B", "M")))
}

#' Add labels Neolithic B and C
#'
#' @param x A \code{tibble}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
add_neo <- function(x) {
  x %>% mutate(neo = case_when(
    period %in% c("p5.0", "p4.8", "p4.6", "p4.4", "p4.2", "p4.0") ~ "NeoB",
    period %in% c("p3.8", "p3.6", "p3.4") ~ "NeoC"
  ),
  neo = factor(neo, levels = c("NeoB", "NeoC")))
}

#' Abbreviate variable names
#'
#' @param x A \code{tibble}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
relabel_variable <- function(x) {
  x %>% mutate(var = case_when(
    column == "altitude" ~ "alt.",
    column == "cont" ~ "cont.",
    column == "dist_fenced" ~ "d. fenced",
    column == "hydro_kde" ~ "hydro.",
    column == "line_terrain" ~ " t. line",
    column == "settlements_kde" ~ "dens.",
    column == "slope" ~ "slope",
    column == "rm_dist_chipped" ~ "d. ch.",
    column == "line_water" ~ "w. line",
    column == "rm_dist_polished" ~ "d. pol.",
  ))
}


# Functions to create general overview of input data ----------------------

#' Tabulate input data
#'
#' @param x Input data \code{tibble}.
#'
#' @return A \code{kable} object.
#' @export
#'
#' @examples
table_nr_set <- function(x) {
  x %>% group_by(period_label) %>%
    summarize(n = n()) %>%
    rename('Period' = period_label) %>%
    knitr::kable(caption = "Number of settlements across time periods.") %>%
    kableExtra::kable_styling(full_width = FALSE)
}

#' Map input data
#'
#' @param x Input data \code{tibble}.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_nr_set <- function(x) {
  x %>% ggplot() +
    geom_sf(data = mask, fill = "gray80", color = NA, alpha = 0.4) +
    geom_sf(data = admin0, linetype = 1, fill = NA, color = "gray60", alpha = 0.4) +
    geom_sf(data = rivers1, color = "mediumblue", size = 0.4, alpha = 0.4) +
    geom_sf(data = rivers0, color = "mediumblue", size = 0.6, alpha = 0.4) +
    geom_sf(alpha = 0.2, aes(color = label)) +
    scale_color_brewer(palette = "Set1", name = "Tradition") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    facet_wrap(vars(period_label), nrow = 3) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill = NA))
}

