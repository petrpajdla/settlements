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
