# Project "Settlements"
# Script nr.
# ANALYSIS DEM
# author: Petr Pajdla
# Getting values from DEM

library(tidyverse)
library(sf)
library(stars)
library(starsExtra)


# data --------------------------------------------------------------------

derived_data <- "analysis/data/derived_data"

path_der <- here::here(derived_data, "dem/")
path_maps <- here::here(derived_data, "maps/")

# mask
mask <- st_read(paste0(path_maps, "mask.geojson"), quiet = TRUE)
bbox <- st_read(paste0(path_maps, "bbox.geojson"), quiet = TRUE)
# bboxes_m <- st_read(paste0(path_der, "bboxes_morava.geojson"))

# dem
# dem <- read_stars(paste0(path_der, "dem_aster_sjtsk.tif")) %>%
#   st_set_crs(st_crs(bbox))
# write_stars(dem, paste0(path_der, "dem_aster_sjtsk_stars.tif"))
dem <- read_stars(paste0(path_der, "dem_aster_sjtsk_stars.tif"), proxy = FALSE)

# settlements
set_spat <- st_read(here::here(derived_data, "settlements_sf.geojson"),
                    quiet = TRUE)

# reduce dem ---------------------------------------------------------------

# bbox_grid <- st_make_grid(bbox, cellsize = 5000)
#
# plot(bbox_grid)
# plot(st_geometry(mask), add = TRUE)
#
# dem_grid <- aggregate(dem, bbox_grid, mean, na.rm = TRUE)
#
# dem_grid_sf <- st_as_sf(dem_grid)

# st_write(dem_grid_sf, paste0(path_der, "dem_aster_sjtsk_sf.geojson"))
dem_sf <- st_read(paste0(path_der, "dem_aster_sjtsk_sf.geojson")) %>%
  rename(value = dem_aster_sjtsk.tif)


# B -----------------------------------------------------------------------

# bbox
b_bbox <- set_spat %>%
  filter(str_detect(id, "^B")) %>%
  st_buffer(dist = 5e3) %>%
  st_union() %>%
  st_bbox()

# crop
b_dem <- dem[b_bbox]

# slope
b_slope <- slope(b_dem)

# plot(b_dem, breaks = "equal", col = terrain.colors(11))
# plot(b_slope, breaks = "equal", col = hcl.colors(11, "Spectral"))

# b points
b_points <- set_spat %>% filter(str_detect(id, "^B"))

# extract values
b_slope_vals <- st_extract(b_slope, b_points) %>%
  st_drop_geometry() %>%
  as_tibble()

b_dem_vals <- st_extract(b_dem, b_points) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  rename(dem = "dem_aster_sjtsk_stars.tif")

# result
b_res <- b_points %>%
  st_drop_geometry() %>%
  select(id) %>%
  as_tibble() %>%
  bind_cols(b_dem_vals, b_slope_vals)


# M -----------------------------------------------------------------------

# bbox
m_bbox <- set_spat %>%
  filter(str_detect(id, "^M")) %>%
  st_buffer(dist = 5e3) %>%
  st_union() %>%
  st_bbox()

# grid
m_grid <- m_bbox %>%
  st_make_grid(cellsize = c(4.2e4, 4.2e4)) %>%
  st_as_sf() %>%
  mutate(id = 1:25) %>%
  filter(id %in% c(2:4, 6:9, 11:15, 17:20, 23:24))

# # crop
# m_dem8 <- dem[st_as_sfc(st_bbox(filter(m_grid_buff, id == 8)))]
# # slope
# m_slope8 <- slope(m_dem8)
# # plot
# plot(m_dem8, breaks = "equal", col = terrain.colors(11))
# plot(m_slope8, breaks = "equal", col = hcl.colors(11, "Spectral"))


# functions ---------------------------------------------------------------

#' @title Extract values from DEM
#' @param x dem
#' @param pts points of interest
#' @param plg layer of polygons /grid
#' @param nr numeric id of a polygon in plg layer
extract_from_dem <- function(x, pts, plg, nr) {

  # filter polygon
  plg_flt <- filter(plg, id == nr)
  # polygon buffer
  plg_buff <- plg_flt %>%
    st_buffer(5e3) %>%
    st_bbox() %>%
    st_as_sfc()
  # filter sites in polygon
  pts_flt <- pts %>% st_filter(plg_flt)

  # crop dem
  crp <- x %>% st_crop(plg_buff)
  # slope
  slp <- slope(crp)

  # extract values
  slp_vals <- st_extract(slp, pts_flt) %>%
    st_drop_geometry() %>%
    as_tibble()

  crp_vals <- st_extract(crp, pts_flt) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    rename(dem = "dem_aster_sjtsk_stars.tif")

  # result
  res <- pts_flt %>%
    st_drop_geometry() %>%
    select(id) %>%
    as_tibble() %>%
    bind_cols(crp_vals, slp_vals)

  return(res)
}

#' @title Extract vaues from DEM for different polygons
#' @param x dem
#' @param pts points
#' @param plg layer of polygons
loop_through_grid <- function(x, pts, plg) {

  index <- plg %>% st_drop_geometry() %>% pull(id)

  res <- vector("list", length(index))

  for (i in seq_along(index)) {
    res[[i]] <- extract_from_dem(x, pts, plg, nr = index[i])
  }

  return(res)
}


# M - get values
m_vals <- loop_through_grid(x = dem, pts = set_spat, plg = m_grid)


# export resulting values -------------------------------------------------

m_vals %>%
  bind_rows() %>%
  bind_rows(b_res) %>%
  write_csv(here::here(derived_data, "dem", "vals_dem.csv"))

# plots -------------------------------------------------------------------

# ggplot() +
#   geom_sf(data = dem_sf, aes(fill = value), color = NA) +
#   scale_fill_viridis_c() +
#   # geom_sf(data = st_as_sfc(b_bbox),
#   #         fill = NA, color = "white") +
#   geom_sf(data = st_as_sfc(m_bbox),
#           fill = NA, color = "white") +
#   geom_sf(data = set_spat,
#           color = "white", alpha = 0.4, shape = 4) +
#   geom_sf(data = m_grid, color = "red", fill = NA) +
#   # geom_sf(data = m_grid_filtered, color = "white", fill = "gray", alpha = 0.4) +
#   theme_void()

# # plot B
# ggplot() +
#   geom_stars(data = b_dem) +
#   scale_fill_viridis_c() +
#   geom_sf(data = filter(set_spat, str_detect(id, "^B")),
#           color = "white", alpha = 0.4, shape = 4) +
#   theme_void()

# NOPE!
# # plot M
# ggplot() +
#   geom_stars(data = m_dem) +
#   scale_fill_viridis_c() +
#   geom_sf(data = filter(set_spat, str_detect(id, "^M")),
#           color = "white", alpha = 0.4, shape = 4) +
#   theme_void()
