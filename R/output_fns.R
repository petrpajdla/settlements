# Output preparation functions --------------------------------------------

# Functions with \code{prep_} prefix prepare the data into the long format for
# binding with results \code{tibble}.


#' Prepare density of watercourse result data
#'
#' Functions with \code{prep_} prefix prepare the data into the long format for
#' binding with results \code{tibble}.
#'
#' @param x A \code{tibble}.
#'
#' @return A long results \code{tibble}.
#' @export
#'
#' @examples
prep_hydro_dens_res <- function(x) {
  x %>% select(-ends_with("label"), -chrono) %>%
    left_join(hydro_dens_at_points, by = "id") %>%
    pivot_longer(cols = hydro_kde) %>%
    rename(variable = name)
}

#' Prepare DEM result
#'
#' Functions with \code{prep_} prefix prepare the data into the long format for
#' binding with results \code{tibble}.
#'
#' @param x A \code{tibble}.
#'
#' @return A long results \code{tibble}.
#' @export
#'
#' @examples
prep_dem_res <- function(x) {
  x %>% select(-ends_with("label"), -chrono) %>%
    left_join(dem_data, by = "id") %>%
    pivot_longer(cols = c(dem, slope)) %>%
    rename(variable = name)
}

#' Prepare altitude result
#'
#' Functions with \code{prep_} prefix prepare the data into the long format for
#' binding with results \code{tibble}.
#'
#' @param x A \code{tibble}.
#'
#' @return A long results \code{tibble}.
#' @export
#'
#' @examples
prep_alt_res <- function(x) {
  x %>% select(-ends_with("label"), -chrono) %>%
    left_join(altitude, by = "id") %>%
    pivot_longer(cols = altitude) %>%
    rename(variable = name)
}

#' Prepare distance to fenced settlements result
#'
#' Functions with \code{prep_} prefix prepare the data into the long format for
#' binding with results \code{tibble}.
#'
#' @param x A \code{tibble}.
#'
#' @return A long results \code{tibble}.
#' @export
#'
#' @examples
prep_dist_fenced <- function(x) {
  x %>% mutate(variable = "dist_fenced",
               value = min_dist) %>%
    select(-min_dist, -reg)
}

#' Prepare wide results \code{tibble}
#'
#' Transforms long results \code{tibble} into a wide \code{tibble} with
#' variables in columns for different settlements adn periods.
#'
#' @param x A long results \code{tibble}.
#'
#' @return A wide results \code{tibble}.
#' @export
#'
#' @examples
prep_wide_res <- function(x) {
  x %>% group_by(id, period, variable) %>%
    filter(variable != "dem") %>%
    summarise(value = mean(value), .groups = "drop") %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    mutate(across(everything(), na2null))
}
