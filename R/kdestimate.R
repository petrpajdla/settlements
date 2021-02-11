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
  mask <- spatstat::as.owin(mask)
  marks <- sf::st_drop_geometry(x) %>%
    dplyr::filter(!is.na(markscol)) %>%
    dplyr::pull(markscol) %>%
    factor(levels = lvls)

  pppset <- spatstat::ppp(x = coords[, 1], y = coords[, 2],
                          window = mask,
                          marks = marks)
  splitpppset <- spatstat::split.ppp(pppset)

  if (points) {
    dens <- spatstat::density.splitppp(splitpppset,
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
    img <- spatstat::density.splitppp(splitpppset,
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
