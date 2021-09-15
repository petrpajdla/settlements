#' Kernel density estimation for spatial point pattern
#'
#' Wrapper around \code{spatstat} functions. Kernel density is estimated for
#' a point pattern in a given window for different levels of a categorical
#' variable.
#'
#' @param x \code{sf} object of geometry type \code{POINTS} in projected CRS.
#' @param mask Single \code{sf} \code{polygon}, the study window.
#' @param markscol Column name of \code{x} to use as grouping variable.
#' @param lvls Vector of levels of \code{markscol}.
#' @param sigma Standard deviation of isotropic smoothing kernel.
#'     Numeric value, passed to \code{spatstat::density.splitppp} function.
#'     Scale depends on coordinate reference system, for S-JTSK (EPSG 5514)
#'     it is in meters and defaults to 4 km.
#' @param points Logical, return value for points or whole surface as raster image.
#'     Defaults to \code{TRUE}.
#'
#' @return With \code{points} set to \code{TRUE} (default) returns a tibble with
#'     KDE estimated at given points. If set to \code{FALSE}, a tibble with KDE
#'     values in a grid is returned to be plotted using \code{ggplot2}.
#'
kdestimate <- function(x, mask, markscol, lvls, sigma = 4e3, points = TRUE) {
  stopifnot(any(class(x) == "sf"))
  stopifnot(all(sf::st_geometry_type(x) == "POINT"))
  stopifnot(all_of(markscol) %in% colnames(x))

  coords <- x %>% dplyr::filter(!is.na(markscol)) %>%
    sf::st_coordinates(x)
  mask <- spatstat.geom::as.owin(mask)
  marks <- sf::st_drop_geometry(x) %>%
    dplyr::filter(!is.na(markscol)) %>%
    dplyr::pull(markscol) %>%
    factor(levels = lvls)

  pppset <- spatstat.geom::ppp(x = coords[, 1], y = coords[, 2],
                               window = mask,
                               marks = marks)
  splitpppset <- spatstat.geom::split.ppp(pppset)

  if (points) {
    dens <- spatstat.core::density.splitppp(splitpppset,
                                            at = "points",
                                            sigma = sigma) %>%
      purrr::map(as_tibble) %>%
      dplyr::bind_rows(.id = markscol) %>%
      dplyr::group_by(!!as.symbol(markscol)) %>%
      tidyr::nest()

    set <- x %>%
      sf::st_drop_geometry() %>%
      dplyr::as_tibble() %>%
      dplyr::select(id, chrono, markscol) %>%
      dplyr::group_by(!!as.symbol(markscol)) %>%
      tidyr::nest()

    dplyr::left_join(set, dens, by = markscol) %>%
      dplyr::mutate(data = pmap(list(data.x, data.y), bind_cols)) %>%
      dplyr::select(markscol, data) %>%
      tidyr::unnest(data) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(name = factor(!!as.symbol(markscol),
                                  levels = levels(marks))) %>%
      dplyr::mutate(kde = value * 1e6) %>%
      dplyr::select(id, name, chrono, kde)
  } else {
    img <- spatstat.core::density.splitppp(splitpppset,
                                           sigma = sigma)
    rstr <- vector("list", length(img))
    names(rstr) <- lvls

    for (i in seq_along(img)) {
      rstr[[i]] <- raster::raster(img[[i]]) %>%
        raster::rasterToPoints() %>%
        as_tibble()
    }

    dplyr::bind_rows(rstr, .id = markscol) %>%
      dplyr::mutate(kde = layer * 1e6) %>%
      dplyr::select(markscol, x, y, kde, -layer)
  }
}


#' Helper function to estimate KDE
#'
#' @param x Simple features object. Input data.
#'
#' @return A long \code{tibble} with kernel density estimate for each id and period pair.
#' @export
#'
#' @examples
est_kde <- function(x) {
  x %>% group_by(id, period) %>%
    mutate(n = row_number()) %>%
    filter(n == 1L) %>%
    select(-label) %>%
    kdestimate(mask,
               markscol = "period",
               lvls = names(labs_chrono$period)) %>%
    rename(period = name) %>%
    mutate(period_label = labs_chrono$periods[period],
           period_label = factor(period_label, levels = labs_chrono$periods)) %>%
    select(id, starts_with("period"), kde) %>%
    rename(settlements_kde = kde) %>%
    select(-period_label) %>%
    pivot_longer(col = settlements_kde, names_to = "variable") %>%
    distinct()
}

#' Helper function to plot KDE
#'
#' @param x Simple features object. Input data.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
plot_kde <- function(x) {
  x %>% group_by(id, period) %>%
    mutate(n = row_number()) %>%
    filter(n == 1L) %>%
    select(-label) %>%
    kdestimate(mask,
               markscol = "period",
               lvls = names(labs_chrono$period),
               points = FALSE) %>%
    mutate(period_label = labs_chrono$periods[period],
           period_label = factor(period_label, levels = labs_chrono$periods)) %>%
    distinct() %>%
    ggplot(aes(x, y, fill = kde)) +
    geom_raster() +
    scale_fill_viridis_c(name = "KDE") +
    facet_wrap(vars(period_label)) +
    coord_fixed() +
    theme_void()
}
