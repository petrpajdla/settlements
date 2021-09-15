
# Synthesis plot functions ------------------------------------------------

#' Summary violin/box plots
#'
#' @param x Long result \code{tibble}.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_violin <- function(x) {
  x %>%
    filter(variable != "cont",
           variable != "dem") %>%
    mutate(reg = str_extract(id, "^."),
           period = unname(labs_chrono$periods[period]),
           period = factor(period, levels = unname(labs_chrono$periods)),
           variable = labs_variables[variable],
           variable = fct_relevel(variable, labs_variables)) %>%
    ggplot(aes(reg, value)) +
    # geom_violin(fill = "white") +
    geom_jitter(alpha = 0.04, width = 0.4) +
    geom_boxplot(aes(fill = reg), width = 0.2) +
    scale_fill_manual(values = mycol, name = "Region") +
    facet_grid(rows = vars(variable), cols = vars(period), scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.border = element_rect(color = "black", fill = NA),
          plot.background = element_rect(fill = "white", color = "white"),
          strip.text.y = element_text(angle = 0))
}

#' Summary line plots
#'
#' @param x Long result \code{tibble}.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_lines <- function(x) {
  x %>%
    filter(variable != "cont",
           variable != "dem") %>%
    mutate(reg = str_extract(id, "^."),
           period = unname(labs_chrono$periods[period]),
           period = factor(period, levels = unname(labs_chrono$periods)),
           variable = labs_variables[variable],
           variable = fct_relevel(variable, labs_variables)) %>%
    group_by(period, variable, reg) %>%
    summarise(mean = mean(value),
              median = median(value)) %>%
    ggplot(aes(x = period, color = reg, group = reg)) +
    geom_line(aes(y = mean)) +
    geom_line(aes(y = median), alpha = 0.4, size = 2) +
    scale_color_manual(values = mycol, name = "Region") +
    facet_wrap(vars(variable), scales = "free_y", ncol = 2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 80, hjust = 1),
          axis.title = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"))
}
