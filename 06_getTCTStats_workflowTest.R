
library(raster)






# inputs: raster tif, FFP_output file, dayVar, and rVar
getTCTStats <- function(FFP_file,dayVar,rVar,rast) {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
  bandStack <- stack("test.tif")
  bright <- subset(bandStack,1)
  green <- subset(bandStack,2)
  wet <- subset(bandStack,3)
  
  brightnessRast <- projectRaster(bright, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  greennessRast <- projectRaster(green, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  wetnessRast <- projectRaster(wet, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  
  #plot(greennessRast)
  
  
  # go to FFP outputs file (EMS tower FFP output files)
  #setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_outputs")
  
  #(getFootprint(FFP_file,dayVar,rVar))
  
  #reads in EMS FFP file
  #FFP_file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  #print("FFP output file READ IN.")
  #print(FFP_file)
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_outputs")
  FFP_loaded <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #filters by dec.day and r
  FFP_filtered <- FFP_loaded %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  #print("FFP output FILTERED.")
  
  #define coords as coordinates of all the vector points in FFP filtered
  coords = cbind(FFP_filtered[5],FFP_filtered[6])
  #print(coords)
  
  #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
  p = Polygon(coords)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  #print("Spatial Polygon CREATED.")
  #plot(sps)
  
  #set working directory to where outputs are
  
  #create raster stack from landsat8 imagery
  #rast <- raster(rasterImage)
  #print("Raster Image READ IN.")
  
  
  
  #mask landsat imagery by FFP spatial polygon
 
  
  par(mfrow=c(1,2))
  plot(rast,axes=FALSE)
  plot(raster,axes=FALSE)
  
  
  maskedB <- mask(brightnessRast,sps)
  cmBright <- crop(maskedB,sps)
  
  maskedG <- mask(greennessRast,sps)
  cmGreen <- crop(maskedG,sps)
  
  maskedW <- mask(wetnessRast,sps)
  cmWet <- crop(maskedW,sps)
  
  
  return(data.frame(
          first = c("Brightness", cellStats(cmBright, stat='mean', na.rm=TRUE), cellStats(cmBright, stat='sd', na.rm=TRUE)),
          second = c("Greenness", cellStats(cmGreen, stat='mean', na.rm=TRUE), cellStats(cmGreen, stat='sd', na.rm=TRUE)),
          third = c("Wetness", cellStats(cmWet, stat='mean', na.rm=TRUE), cellStats(cmWet, stat='sd', na.rm=TRUE))
          
  )
  )
          
  
} 



library(tidyverse)
setwd("/Users/benjaminglass/HF21-Scripts")

#choose a single month of footprints

stats <- getTCTStats("FFP.hr.lines.10.12.csv",351,.75,rasterFile)
stats
