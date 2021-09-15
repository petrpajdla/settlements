# Settlement continuity

#' Derive settlement continuity across periods
#'
#' @param x Input period \code{tibble}.
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
settlement_continuity <- function(x) {
  x %>%
    select(-period_label) %>%
    distinct(id, period, chrono) %>%
    mutate(pres = 1L) %>%
    pivot_wider(names_from = period, values_from = pres, values_fill = 0L) %>%
    pivot_longer(starts_with("p"), names_to = "period") %>%
    mutate(period = factor(period, levels = levels(set_base$period1$period))) %>%
    arrange(id, period, chrono) %>%
    group_by(id, chrono) %>%
    mutate(prev = if_else(value == 1, lag(value), 0L),
           prev = if_else(is.na(prev), 0L, prev)) %>%
    ungroup(id, chrono) %>%
    select(-value, -chrono) %>%
    mutate(variable = "cont") %>%
    rename(value = prev) %>%
    filter(value != 0)
}

#' Plot settlement continuity
#'
#' @param x Long results \code{tibble}.
#' @param y A \code{tibble} with number of settlements.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_continuity <- function(x, y) {
  x %>%
    filter(variable == "cont") %>%
    add_period_label() %>%
    add_region() %>%
    group_by(period_label, reg) %>%
    summarise(n = sum(value)) %>%
    full_join(y) %>%
    mutate(n = if_else(is.na(n), 0, n),
           perc = (n / sum) * 100) %>%
    ggplot(aes(period_label, perc, fill = reg)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = mycol) +
    facet_wrap(vars(reg), ncol = 1) +
    labs(x = "Period", y = "Continuity of settlement (%)") +
    theme_minimal()
}
