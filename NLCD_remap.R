setwd("/Users/benjaminglass/Downloads")
library(raster)

rasterImage <- "NLCD2013_0804.tif"
rast <- raster(rasterImage)

print(values(rast))

rc <- reclassify(rast, c(-Inf,41,0,42,Inf,0))

print(values(rc))

rc <- reclassify(rc, c(41,42,1))

print(values(rc))

writeRaster(rast,"newRaster_second.tif",overwrite=TRUE)



      