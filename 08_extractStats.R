# Benjamin Glass
# Last Edits: July 2, 2021

# Script overview: this script actually does the spatial analysis of
  # create tabular data into spatial polygons, and extracting the
  # raster data from the polygon data when they are overlaid on 
  # eachother.

library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(rgeos)

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/misc")


# returns spatial stats for a single FFP decDay 
extractStats <- function(FFP_file,dayVar,rVar,rasterImage,path) {
  
  #filters by dec.day and r
  FFP_filtered <- FFP_file %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  
  # check to see if the resulting filtered data frame actually has rows in it
  if (nrow(FFP_filtered)==0) {
    
    #if the df is empty, that means there is no coordinate data to create a polygon from, and thus we just return Na,Na to the parent script
    print("NULL")
    return(c(NA,NA))
    
  # if there is data, then we move forward
  } else {
  
  #define coords as coordinates of all the vector points in FFP filtered
  coords = cbind(FFP_filtered[5],FFP_filtered[6])
  #print(coords)
  
  #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
  p = Polygon(coords)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  
  
  #PLOTTING PRINT CMD BELOW
  # =====================
  #plot(sps)
  
  #set working directory to where outputs are
  setwd(path)
  
  #create raster stack from landsat8 imagery
  rast <- raster(rasterImage)

  #mask landsat imagery by FFP spatial polygon
  masked <- mask(rast,sps)
  
  #crop masked raster by the outline of the polygon
  raster <- crop(masked,sps)
  
  #return mean and std statistics about the cropped raster
   return(c(cellStats(raster, stat='mean', na.rm=TRUE),
            cellStats(raster, stat='sd', na.rm=TRUE)))

  }
  
  # ======================================
  # ALTERNATE CODE: we can use the extract function also, but it does take longer than cellStats
  # ======================================
  
  
  # print(c(extract(rast,sps,method='simple',buffer=NULL,fun=mean,na.rm=TRUE),
  #         (extract(rast,sps,method='simple',buffer=NULL,fun=sd,na.rm=TRUE))))
  
  # return(   c(extract(rast,sps,method='simple',buffer=NULL,fun=mean,na.rm=TRUE),
  #             (extract(rast,sps,method='simple',buffer=NULL,fun=sd,na.rm=TRUE))
  # )
  # )
  # 
} 

# returns spatial stats for a single FFP decDay (for TCT files specifically)
extractStats_TCT <- function(FFP_file,dayVar,rVar,rasterImage,path) {
  
  #filters by dec.day and r
  FFP_filtered <- FFP_file %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  
  #define coords as coordinates of all the vector points in FFP filtered
  coords = cbind(FFP_filtered[5],FFP_filtered[6])
  
  #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
  p = Polygon(coords)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  
  
  #plot(sps)
  
  #set working directory to where outputs are
  setwd(path)
  
  #mask landsat imagery by FFP spatial polygon
  masked <- mask(rasterImage,sps)
  #print(masked)
  
  # crop by sps
  raster <- crop(masked,sps)
  
  #return stats
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




