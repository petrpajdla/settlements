#' Extract values from terrain model derived from DEM.
#'
#' @param x Object of class \code{sf} or similar.
#' @param dem Digital elevation model.
#' @param model What model to use: slope, aspect, TPI, TRI...
#' @param fun Summary function.
#'
#' @return Object x with result in column \code{value}.
#' @export
#'
#' @examples
extract_from_dem <- function(x, dem, model = "slope", fun = "mean") {

  # create model from dem
  message("Creating terrain model.")
  terrain_model <- terra::terrain(x = dem, v = model)

  # check type of geom
  geom_type <- terra::geomtype(terra::vect(x))
  if (!(geom_type %in% c("points", "polygons"))) {
    stop("x must contain points or polygons.")
  }

  # transform x
  if (terra::crs(terra::vect(x)) != terra::crs(dem)) {
    x <- sf::st_transform(x, terra::crs(dem))
    message("Transforming x to CRS of dem.")
  }

  # slope in the vicinity of settlements
  message("Extracting values.")
  model2values <- terra::extract(terrain_model,
                                 terra::vect(x))
  cols <- colnames(model2values)

  # return values
  if (geom_type == "points") {
    res <- model2values |>
      dplyr::bind_cols(x) |>
      dplyr::select(-!!dplyr::sym(cols[1]))
  } else if (geom_type == "polygons") {
    res <- model2values |>
      dplyr::group_by(!!dplyr::sym(cols[1])) |>
      dplyr::summarise(value = match.fun(fun)(!!dplyr::sym(cols[2]))) |>
      dplyr::bind_cols(x) |>
      dplyr::select(-!!dplyr::sym(cols[1]))
  }
  return(res)
}
