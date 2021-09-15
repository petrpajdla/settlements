#' Nested PCA
#'
#' @param x A \code{tibble} grouped according to chosen variables.
#'
#' @return A nested \code{tibble} with results of \code{prcomp} function.
#' @export
#'
#' @examples
nested_pca <- function(x) {
  x %>%
    mutate(
      id = map(data, \(x) select(x, id)),
      data = map(data, \(x) select(x, -id)),
      mx = map(data, \(x) complete(x)),
      mx = map(mx, \(x) select(x, where(~ sum(.x) != 0))),
      mx = map(mx, \(x) select(x, where(~ sum(.x)/length(.x) != 1))),
      mx = map(mx, \(x) mutate(x, across(where(is.numeric), scale))),
      mx = map(mx, as.matrix),
      pca = map(mx, prcomp),
      sdev = map(pca, \(x) tibble(sdev = x$sdev)),
      sdev = map(sdev, \(x) mutate(x,
                                   var_prop = sdev^2 / sum(sdev^2),
                                   cum_prop = cumsum(var_prop),
                                   pc = row_number())),
      x = map(pca, \(x) as_tibble(x$x)),
      rotation = map(pca, broom::tidy, matrix = "rotation"),
      rotation = map(rotation, pivot_wider,
                     names_from = "PC", names_prefix = "PC",
                     values_from = "value")
    ) %>%
    select(-mx, -pca)
}

#' Plot nested PCA results
#'
#' @param x Nested PCA object, output of \code{nested_pca} function.
#' @param var Grouping variable visualize on.
#' @param pc Principal components to show on axes x and y.
#'     Defaults to PC1 and PC2.
#'
#' @return Returns a \code{patchwork} of data in selected PC space and
#'     associated plots with rotation visualization.
#' @export
#'
#' @examples
plot_nested_pca <- function(x, var, pc = c("PC1", "PC2")) {
  pca <- x %>%
    select(reg, !!sym(var), x) %>%
    unnest(x) %>%
    ungroup(!!sym(var), reg) %>%
    ggplot(aes(!!sym(pc[1]), !!sym(pc[2]))) +
    geom_point(alpha = 0.2) +
    facet_grid(vars(!!sym(var)), vars(reg), scales = "free") +
    theme_minimal()

  rot <- x %>%
    select(reg, !!sym(var), rotation) %>%
    unnest(rotation) %>%
    ungroup(!!sym(var), reg) %>%
    relabel_variable() %>%
    ggplot(aes(!!sym(pc[1]), !!sym(pc[2]))) +
    geom_segment(xend = 0, yend = 0, alpha = 0.4) +
    ggrepel::geom_text_repel(aes(label = var),
                             color = "#904C2F",
                             size = 2.6) +
    facet_grid(vars(!!sym(var)), vars(reg)) +
    theme_minimal()

  pca | rot
}


#' Hierarchical clustering on the variables
#'
#' @param x Nested PCA object.
#'
#' @return Nested object with hierarchcal clustering results and dendrogram.
#' @export
#'
#' @examples
hclust_variables <- function(x) {
  x %>% mutate(
    dist = map(rotation, \(x) column_to_rownames(x, "column")),
    dist = map(dist, \(x) as.matrix(x)),
    dist = map(dist, \(x) dist(x, method = "maximum")),
    hclust = map(dist, \(x) hclust(x, method = "ward.D2")),
    lab = map2(reg, neo, \(x, y) str_c("Reg. ", x, ", ", y)),
    dendro = map2(hclust, lab, \(x, y) ggdendro::ggdendrogram(x, rotate = TRUE) +
                    labs(title = y))
  )
}

hclust_plot_dendro <- function(x, vars) {
  plots <- x %>%
    unnest(c({ vars[1] }, { vars[2] })) %>%
    ungroup() %>%
    pull(dendro)

  (plots[[3]] / plots[4]) | (plots[[2]] / plots[[1]])
}
