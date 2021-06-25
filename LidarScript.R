
# This script reads in LIDAR raster tiles from a specific year (NEON data)
# and exports a mosaic raster of all the raster tiles based on a specific extent

#https://data.neonscience.org/data-products/DP3.30015.001

library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(rgeos)

setwd("/Users/benjaminglass/Downloads/NEON_struct-ecosystem_2/NEON.D01.HARV.DP3.30015.001.2019-08.basic.20210625T151941Z.RELEASE-2021")
path <- "/Users/benjaminglass/Downloads/NEON_struct-ecosystem_2/NEON.D01.HARV.DP3.30015.001.2019-08.basic.20210625T151941Z.RELEASE-2021"
f <- list.files(path=path, pattern='tif$', full.names=TRUE)
r <- lapply(f, raster)

r$fun <- mean

rasters <- list(r)
rasters

rasterMosaic <- do.call(mosaic,r)

blankRaster <- raster(extent(731595,732280,4713222,4713723))
proj4string(blankRaster) <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

writeRaster(rasterMosaic, file="LidarHF21.tif", format="GTiff",overwrite=TRUE)
