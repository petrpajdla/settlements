# fancy map for doaj article

library(tidyverse)
library(sf)
library(stars)


# data --------------------------------------------------------------------

set_spat <- st_read(here::here("analysis/data/derived_data/settlements_sf.geojson"))

mask <- st_read(here::here("analysis/data/raw_data/project_area/maska_poly_wgs84.dbf")) %>%
  st_transform(32633)

reg <- st_read(here::here("analysis/data/joad_data/geodata/regions.gml"),
               options="FORCE_SRS_DETECTION=YES") %>%
  st_transform(32633)

# ne data
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/NE2_LR_LC_SR_W_DR.zip
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/GRAY_HR_SR_W.zip
# ne_raster <- rnaturalearth::ne_download(scale = 10, category = "raster",
#                                         type = "GRAY_HR_SR_W",
#                                         destdir = here::here("analysis/data/temp/"),
#                                         returnclass = "sf")

# ne_raster <- read_stars(here::here("analysis/data/temp/GRAY_HR_SR_W.tif"))

raster <- read_stars(here::here("analysis/data/raw_data/dem/DEM_digital_elevation.tif"))

riv <- rnaturalearth::ne_download(scale = 10, category = "physical",
                                  type = "rivers_lake_centerlines_scale_rank",
                                  returnclass = "sf")

# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip
bord <- rnaturalearth::ne_download(scale = 10, category = "cultural",
                                   type = "admin_0_countries",
                                   returnclass = "sf")

# prep --------------------------------------------------------------------

bbox <- set_spat %>%
  st_transform(32633) %>%
  st_buffer(1.5e4) %>%
  st_bbox() %>%
  st_as_sfc()

# crop to ce
ce <- st_crop(raster, bbox)
# plot(ce)

# rivers
riv2 <- riv %>%
  filter(st_is_valid(riv)) %>%
  st_crop(st_transform(bbox, 4326)) %>%
  st_transform(32633)

bord2 <- bord %>%
  filter(st_is_valid(bord)) %>%
  st_crop(st_transform(bbox, 4326)) %>%
  st_transform(32633)

# transparent mask outside research area
mask2 <- st_difference(bbox, mask)
reg2 <- st_difference(bbox, st_union(reg))

plot(reg2, col = "red")

# plot --------------------------------------------------------------------

# version 02
set_spat %>%
  st_transform(32633) %>%
  ggplot() +
  geom_stars(data = ce, downsample = 16) +
  scale_fill_gradient2(low = "#006837",
                       mid = "#ffffbf",
                       high = "#a50026",
                       midpoint = 500, "Elevation (m)") +
  geom_sf(data = riv2, color = "darkblue") +
  geom_sf(data = bord2, fill = NA, linetype = 3) +
  geom_sf(data = reg2, fill = "white",
          color = NA, alpha = 0.6) +
  geom_sf(alpha = 0.2) +
  theme(legend.position = c(0.88, 0.9),
        legend.justification = c(1, 1),
        legend.direction = "horizontal",
        legend.background =
          # element_rect(fill = "#FFFFFF90", color = "#FFFFFF90")
          element_blank()
  ) +
  ggspatial::annotation_scale(pad_x = unit(11, "cm"),
                              pad_y = unit(1.9, "cm")) +
  ggspatial::annotation_north_arrow(pad_x = unit(19, "cm"),
                                    pad_y = unit(1.9, "cm"),
                                    width = unit(1.4, "cm"))

# version 01
# set_spat %>%
#   st_transform(32633) %>%
#   ggplot() +
#   geom_stars(data = ce, downsample = 16) +
#   scale_fill_gradient2(low = "#006837",
#                        mid = "#ffffbf",
#                        high = "#a50026",
#                        midpoint = 500, "Elevation (m)") +
#   geom_sf(data = riv2, color = "darkblue") +
#   geom_sf(data = bord2, fill = NA, linetype = 3) +
#   geom_sf(data = mask2, fill = "white",
#           color = NA, alpha = 0.6) +
#   geom_sf(alpha = 0.2) +
#   theme(legend.position = c(0.88, 0.9),
#         legend.justification = c(1, 1),
#         legend.direction = "horizontal",
#         legend.background =
#           # element_rect(fill = "#FFFFFF90", color = "#FFFFFF90")
#           element_blank()
#         ) +
#   ggspatial::annotation_scale(pad_x = unit(2, "cm"),
#                               pad_y = unit(2, "cm")) +
#   ggspatial::annotation_north_arrow(pad_x = unit(19, "cm"),
#                                     pad_y = unit(2, "cm"),
#                                     width = unit(1.4, "cm"))

ggsave(here::here("analysis/figures/map_joad.png"), width = 10, height = 10)


# inset map --------------------------------------

countries <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf")

bbox_eur <- c(xmin = -10, ymin = 35,
              xmax = 37, ymax = 59) %>%
  st_bbox()

bg_countries <- countries %>%
  st_transform(4326) %>%
  st_geometry() %>%
  st_make_valid() %>%
  st_crop(bbox_eur) %>%
  st_transform(32633)

ggplot() +
  geom_sf(data = bg_countries,
          color = "white", fill = "gray80") +
  geom_sf(data = bbox,
          fill = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"))

ggsave(here::here("analysis/figures/map_joad_inset.png"), width = 6, height = 3)

