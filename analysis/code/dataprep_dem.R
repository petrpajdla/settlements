# Project "Settlements"
# Script nr.
# DEM PREP
# author: Petr Pajdla
# Preparation of rasters for slope calculation

library(sf)
library(raster)


# data --------------------------------------------------------------------

raw_data <- "analysis/data/raw_data"
derived_data <- "analysis/data/derived_data"

path_raw <- here::here(raw_data, "dem/")
path_der <- here::here(derived_data, "dem/")
path_maps <- here::here(derived_data, "maps/")

if (!dir.exists(path_der)) {
  dir.create(path_der)
}

# mask
mask <- st_read(paste0(path_maps, "mask.geojson"), quiet = TRUE)
bbox <- st_read(paste0(path_maps, "bbox.geojson"), quiet = TRUE)

# dem
dem_aster <- raster(paste0(path_raw, "DEM_digital_elevation.tif"))

# crop
dem_aster_cropped <- dem_aster %>%
  crop(st_transform(bbox, st_crs(dem_aster)))

# reproject to sjtsk
dem_aster_sjtsk <- dem_aster_cropped %>%
  projectRaster(crs = crs(bbox))

dem_aster_sjtsk_cropped <- dem_aster_sjtsk %>%
  crop(bbox)

# write raster for analysis
writeRaster(dem_aster_sjtsk_cropped,
            filename = paste0(path_der, "dem_aster_sjtsk.tif"),
            overwrite = TRUE)


# exploratory plots -------------------------------------------------------

# plot(dem_aster_sjtsk_cropped)
# plot(mask, col = NA, add = TRUE)
#
# res(dem_aster_sjtsk_cropped)
#
# plot(dem_aster_cropped)
# plot(dem_aster_sjtsk)
# plot(dem_aster_sjtsk_cropped)

