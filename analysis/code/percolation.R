# Project "Settlements"
# Script nr.
# PERCOLATION ANALYSIS
# author: Petr Pajdla
# Find percolation distances

library(tidyverse)
library(igraph)
library(sf)
library(here)


# functions ---------------------------------------------------------------

# percolation function
percolation <- function(x, lower, upper, step) {
  stopifnot(any(class(x) == "sf"))
  m <- st_coordinates(x)
  rownames(m) <- x$id

  dist <- as.matrix(stats::dist(m, method = "euclidean"))

  # radius
  # triangle <- dist[lower.tri(dist)]
  # if (is.null(lower)) {
  #   lower <- min(triangle)
  # }
  # if (is.null(upper)) {
  #   upper <- max(triangle) / 4
  # }
  # if (is.null(step)) {
  #   step <- (upper - lower) / 10
  # }
  radii <- seq(lower, upper, by = step)

  res_lst <- vector("list", length(radii))
  names(res_lst) <- radii

  # distances
  dist_df <- reshape2::melt(dist)
  colnames(dist_df) <- c("from", "to", "value")
  dist_df <- dist_df[!(dist_df$from == dist_df$to), ]

  for (i in seq_along(radii)) {
    index <- dist_df$value <= radii[i]
    if (!any(index)) {
      res_lst[[as.character(radii[i])]] <- matrix(
        ncol = 2,
        dimnames = list(NULL, c("id", "clust")))
    } else {
      nodes <- dist_df[index, c("from", "to")]
      nodes <- sapply(nodes, as.character)
      g <- graph_from_edgelist(nodes)

      # get clusters
      clust_memb <- clusters(g)$membership
      # clust_memb <- clusters(g, mode = "weak")$membership
      memb <- matrix(ncol = 2, nrow = length(clust_memb))
      colnames(memb) <- c("id", "clust")
      memb[, "clust"]  <- unname(clust_memb)
      memb[, "id"] <- names(clust_memb)

      # result - list
      res_lst[[as.character(radii[i])]] <- memb
    }
  }

  res_tbl <- lapply(res_lst, as_tibble) %>%
    bind_rows(.id = "radius") %>%
    filter(!is.na(id)) %>%
    mutate(radius = as.numeric(radius))

  # membership for different radii
  res <- vector("list", 3)
  names(res) <- c("stats",
                  "membership_wide",
                  "membership_long")

  res[["membership_long"]] <- res_tbl %>%
    mutate(id = as.character(id)) %>%
    arrange(id)

  # res[["membership_wide"]] <- res_tbl %>%
  #   pivot_wider(values_from = clust,
  #               names_from = radius,
  #               names_prefix = "radius") %>%
  #   mutate(id = as.numeric(id)) %>%
  #   arrange(id)

  # number of nodes
  nodes_num <- res_tbl %>%
    group_by(radius) %>%
    summarise(clust = unique(as.numeric(clust)), .groups = "keep") %>%
    count() %>%
    arrange(radius)

  # maximum cluster size and normalized cluster size etc.
  clust_stats <- res_tbl %>%
    group_by(radius, clust) %>%
    count() %>%
    ungroup(clust) %>%
    summarise(max_nodes = max(n),
              mean_nodes = mean(n),
              median_nodes = median(n), .groups = "drop") %>%
    arrange(radius) %>%
    mutate(max_nodes_norm = max_nodes / nrow(x))

  res$stats <- left_join(nodes_num, clust_stats, by = "radius")

  return(res)
}

# percolation for different factors (aka chronological periods)
# period must be in column labeled 'chrono'
percolate_periods <- function(x, ...) {

  lvls <- x %>%
    pull(chrono) %>%
    factor() %>%
    levels()

  res <- vector("list", length(lvls))
  names(res) <- lvls

  for (i in seq_along(lvls)) {
    flt <- x %>%
      filter(chrono == lvls[i])

    res[[i]] <- percolation(x = flt, ...)
  }

  return(res)

}

# plot lines
plot_percolation_periods <- function(x, distances = NULL) {
  stopifnot(is.list(x))
  lvls <- names(x)

  if (is_empty(distances)) {
    for (i in seq_along(lvls)) {
      input <- x[[i]]

      input$stats %>%
        mutate(radius_km = radius / 1e3) %>%
        ggplot(aes(radius_km, max_nodes_norm)) +
        geom_path() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
        labs(y = "max. nr of nodes (normalized)", x = "distance (km)",
             title = paste(lvls[i])) +
        theme_bw()

      ggsave(here(pth_plots, paste0("percolation_",
                                    str_replace(lvls[i], "\\/", "\\."),
                                    ".pdf")), scale = 2)
    }
  } else {
    for (i in seq_along(lvls)) {
      input <- x[[i]]

      input$stats %>%
        mutate(radius_km = radius / 1e3,
               label_radius = if_else(radius_km %in% distances$breaks[[lvls[i]]],
                                      radius_km, NA_real_)) %>%
        ggplot(aes(radius_km, max_nodes_norm)) +
        geom_path() +
        geom_point(aes(x = label_radius),
                   shape = 21, size = 2.8, fill = "white") +
        geom_text(aes(x = radius_km - 0.025, y = max_nodes_norm + 0.025,
                      label = label_radius)) +
        annotate('text', +Inf, -Inf,
                 hjust = 1.2, vjust = -2.5,
                 label = paste("Single cluster at",
                               distances$max[[lvls[i]]],
                               "km")) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
        labs(y = "max. nr of nodes (normalized)", x = "distance (km)",
             title = paste(lvls[i])) +
        theme_bw()

      ggsave(here(pth_plots, paste0("percolation_",
                                    str_replace(lvls[i], "\\/", "\\."),
                                    ".pdf")), scale = 2)
    }
  }
}

# plot clusters in space
plot_clusters <- function(x, sf, distances, mask) {
  lvls <- names(x)

  for (i in seq_along(lvls)) {
    spat <- sf %>% filter(chrono == lvls[i])

    memb <- x[[lvls[i]]]$membership_long %>%
      filter(radius %in% (distances$breaks[[lvls[i]]] * 1e3)) %>%
      mutate(radius_label = paste(radius * 0.001, "km"),
             radius_label = fct_reorder(radius_label, radius))

    input <- spat %>%
      right_join(memb, by = "id")

    # save geodata for gis inspection
    st_write(obj = input,
             dsn = here(pth_dt_out, paste0("percolation_",
                                     str_replace(lvls[i], "\\/", "\\."),
                                     ".geojson")),
             delete_dsn = TRUE)

    input %>%
      ggplot() +
      geom_sf(data = mask, color = "gray90", fill = "gray90") +
      geom_sf(data = spat, shape = 4, color = "white") +
      geom_sf(aes(color = factor(clust)), shape = 4, show.legend = FALSE) +
      theme_void() +
      facet_wrap(vars(radius_label)) +
      labs(title = lvls[i])

    ggsave(here(pth_plots, paste0("clusters_",
                                  str_replace(lvls[i], "\\/", "\\."),
                                  ".pdf")), scale = 2)
  }
}

# data --------------------------------------------------------------------

# paths
dt_der <- "analysis/data/derived_data"
dt_maps <- here(dt_der, "maps/")

pth_plots <- "analysis/figures/percolation"
pth_dt_out <- "analysis/data/derived_data/outputs"

# dir.create(here(pth_plots))
# dir.create(here(pth_dt_out))

# database
set_base <- read_rds(here(dt_der, "settlements.RDS"))
set_spat <- st_read(here(dt_der, "settlements_sf.geojson"))

settlements1 <- set_spat %>%
  right_join(set_base$chrono1, by = "id")

settlements2 <- set_spat %>%
  right_join(set_base$chrono2, by = "id")

# geodata
mask <- st_read(paste0(dt_maps, "mask.geojson"))


# all sites ---------------------------------------------------------------

# # lower radius 100 m, upper radius 10 km, step 100 m
# # 0, 50 km by 500 m
# perc <- percolation(settlements1, lower = 0, upper = 50000, step = 500)
#
# # identifying breaks (km)
# p_breaks <- c(2.5, 4, 5.5, 7.5, 9, 37.5)
#
# perc$stats %>%
#   mutate(radius_km = radius / 1e3,
#          label_radius = if_else(radius_km %in% p_breaks,
#                                 radius_km, NA_real_)) %>%
#   ggplot(aes(radius_km, max_nodes_norm)) +
#   geom_path() +
#   geom_point(aes(x = label_radius), shape = 21, size = 2.8, fill = "white") +
#   geom_text(aes(x = radius_km - 0.05, y = max_nodes_norm + 0.05, label = label_radius)) +
#   # annotate('text', +Inf, -Inf, hjust = 1, vjust = -2, label = "Single cluster at x") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
#   labs(y = "max. nr of nodes (normalized)", x = "distance (km)",
#        title = "All settlements") +
#   theme_bw()
#
# ggsave(here(pth_plots, "all.pdf"), scale = 2)

# different periods -------------------------------------------------------

# percolation for different periods
perc_periods1 <- percolate_periods(settlements1,
                                   lower = 0, upper = 50000, step = 500)

perc_periods2 <- percolate_periods(settlements2,
                                   lower = 0, upper = 50000, step = 500)

# manually identified point breaks - implement something in the future!
# for no, using plotly to get correct points
# pl_perc <- perc_periods1$ENE$stats %>%
#   mutate(radius_km = radius / 1e3) %>%
#   ggplot(aes(radius_km, max_nodes_norm)) +
#   geom_path()
#
# plotly::ggplotly(pl)

percolation_distances <- list(
  breaks = list(
    ENE = c(6, 9.5, 11, 21.5), # max 28.5
    'ENE Proto' = c(6, 10.5, 12.5, 16, 22, 26), # max 51.5
    LgK = c(5.5, 7.5, 9), # max 37.5
    'LgK I' = c(5, 6.5, 11, 16.5, 22.5), # max 107.5
    'LgK II' = c(6.5, 8.5, 10.5), # max 61.5
    SBK = c(3, 6, 7, 9), # max 48.5
    'SBK Early' = c(8.5, 12, 16, 21), # max 70
    'SBK Late' = c(7.5, 10.5, 21), # max 88.5
    TRB = c(5.5, 7, 10.5, 19.5), # max 43.5
    'TRB Baalberg' = c(6.5, 12, 17.5, 28.5), # max 72.5
    'TRB Boleraz/Saalzm' = c(7, 12, 20, 35.5) # max 97
  ),
  max = list(
    ENE = 28.5,
    'ENE Proto' = 51.5,
    LgK = 37.5,
    'LgK I' = 107.5,
    'LgK II' = 61.5,
    SBK = 48.5,
    'SBK Early' = 70,
    'SBK Late' = 88.5,
    TRB = 43.5,
    'TRB Baalberg' = 72.5,
    'TRB Boleraz/Saalzm' = 97
  )
)


# diagnostic lines --------------------------------------------------------

plot_percolation_periods(perc_periods1, distances = percolation_distances)
plot_percolation_periods(perc_periods2, distances = percolation_distances)


# maps --------------------------------------------------------------------

plot_clusters(perc_periods1, settlements1, percolation_distances, mask)
plot_clusters(perc_periods2, settlements2, percolation_distances, mask)







