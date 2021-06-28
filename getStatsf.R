
getStats <- function(raster) {
  print(paste0("mean: ",cellStats(raster, stat='mean', na.rm=TRUE)))
  print(paste0("std: ",cellStats(raster, stat='sd', na.rm=TRUE)))
}
