# Benjamin Glass
# Last Edits: June 30, 2021


##### ------------------------------- 00 FRONT MATTER -------------------------------------
library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(rgeos)

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/misc")

##### ------------------------------- 05: FUNCTION: extractStats ------------------------

# inputs: raster tif, FFP_output file, dayVar, and rVar, raster and raster path
extractStats <- function(FFP_file,dayVar,rVar,rasterImage,path) {
  
  #print(FFP_file)
  # go to FFP outputs file (EMS tower FFP output files)
  #setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_outputs")
  
  #(getFootprint(FFP_file,dayVar,rVar))
  
  #reads in EMS FFP file
  #FFP_output <- read.csv(FFP_file,stringsAsFactors=FALSE)
  #print("FFP output file READ IN.")
  
  #filters by dec.day and r
  FFP_filtered <- FFP_file %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  #print("FFP output FILTERED."
  if (nrow(FFP_filtered)==0) {
    print("NULL")
    return(c(NA,NA))
  } else {
  
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
  setwd(path)
  
  #create raster stack from landsat8 imagery
  rast <- raster(rasterImage)
  #print("Raster Image READ IN.")
  #print(rast)
  
  #mask landsat imagery by FFP spatial polygon
  masked <- mask(rast,sps)
  #print(masked)
  #FFP_masked <- crop(masked,sps))
  
  #par(mfrow=c(1,2))
  #plot(rast,axes=FALSE)
  #plot(masked,axes=FALSE)
  #return(crop(masked,sps))
  
  #setwd("/Users/benjaminglass/HF21-Scripts")
  #source("getStatsf.R")
  #print(summary(crop(masked,sps)))
  #return(getStats(crop(masked,sps)))
  
  #print(masked)
  raster <- crop(masked,sps)
   return(c(cellStats(raster, stat='mean', na.rm=TRUE),
        cellStats(raster, stat='sd', na.rm=TRUE)))

  }
  
  # print(c(extract(rast,sps,method='simple',buffer=NULL,fun=mean,na.rm=TRUE),
  #         (extract(rast,sps,method='simple',buffer=NULL,fun=sd,na.rm=TRUE))))
  
  # return(   c(extract(rast,sps,method='simple',buffer=NULL,fun=mean,na.rm=TRUE),
  #             (extract(rast,sps,method='simple',buffer=NULL,fun=sd,na.rm=TRUE))
  # )
  # )
  # 
} 







##### ------------------------------- 05: FUNCTION: extractStats_TCT ------------------------

# extractStats function, but for TCT files
extractStats_TCT <- function(FFP_file,dayVar,rVar,rasterImage,path) {
  
  # go to FFP outputs file (EMS tower FFP output files)
  #setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_outputs")
  
  #(getFootprint(FFP_file,dayVar,rVar))
  
  #reads in EMS FFP file
  #FFP_output <- read.csv(FFP_file,stringsAsFactors=FALSE)
  #print("FFP output file READ IN.")
  
  #filters by dec.day and r
  FFP_filtered <- FFP_file %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  #print("FFP output FILTERED.")
  
  #define coords as coordinates of all the vector points in FFP filtered
  coords = cbind(FFP_filtered[5],FFP_filtered[6])
  
  #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
  p = Polygon(coords)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  #print("Spatial Polygon CREATED.")
  #plot(sps)
  
  #set working directory to where outputs are
  setwd(path)
  
  #create raster stack from landsat8 imagery
  #rast <- raster(rasterImage)
  #print("Raster Image READ IN.")
  #print(rast)
  
  #mask landsat imagery by FFP spatial polygon
  masked <- mask(rasterImage,sps)
  #print(masked)
  #FFP_masked <- crop(masked,sps))
  
  #par(mfrow=c(1,2))
  #plot(rast,axes=FALSE)
  #plot(masked,axes=FALSE)
  #return(crop(masked,sps))
  
  #setwd("/Users/benjaminglass/HF21-Scripts")
  #source("getStatsf.R")
  #print(summary(crop(masked,sps)))
  #return(getStats(crop(masked,sps)))
  
  #print(masked)
  raster <- crop(masked,sps)
  return(c(cellStats(raster, stat='mean', na.rm=TRUE),
           cellStats(raster, stat='sd', na.rm=TRUE)))
  
  
  
  # print(c(extract(rast,sps,method='simple',buffer=NULL,fun=mean,na.rm=TRUE),
  #         (extract(rast,sps,method='simple',buffer=NULL,fun=sd,na.rm=TRUE))))
  
  # return(   c(extract(rast,sps,method='simple',buffer=NULL,fun=mean,na.rm=TRUE),
  #             (extract(rast,sps,method='simple',buffer=NULL,fun=sd,na.rm=TRUE))
  # )
  # )
  # 
} 




