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
    print(paste0("ERROR: NO DATA FOR ",rVar," AND ",dayVar," IN FFP FILE."))
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
  
  #print(rast)
  
  #mask landsat imagery by FFP spatial polygon
  masked <- mask(rast,sps)
  #plot(masked)
  
  #print(masked)
  #print(min(values(masked)))
  #print(all.equal(rast,masked))
  
  if (all(is.na(values(masked)))) {
    print(paste0("ERROR: NO SPATIAL OVERLAP BETWEEN FFP FILE AND ",rasterImage,"."))
    return(c(NA,NA))
  } else {
  
  #crop masked raster by the outline of the polygon
  raster <- crop(masked,sps)
  #plot(raster)
  
  #plot(masked)

  
  
  # ========= DUMMY RASTER TEST =============
  # this uses a blank "dummy" raster that is much larger than the megaplot raster layer to 
  # test whether the FFP polygon adequetely covers the megaplot raster layer. This calculates
  # the pixels in the dummy raster that has been masked by the FFP polygon (representing the
  # full amount of pixels in the FFP under normal conditions). Then we compare this to the
  # amount of pixels in the megaplot raster masked by FFP (which may have fewer pixels if the
  # footprint is off to the side). If this "percentValuePixels" is below a certain threshold value
  # then we discard the results.
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables")
  dummyRaster <- raster("dummyRaster.tif")
  dummyCrop <- crop(mask(dummyRaster,sps),sps)
  dummyTotal <- ncell(dummyCrop)
  total <- ncell(raster)
  percentValuePixels <- total/dummyTotal
  
  #plot(masked)
  #plot(dummyCrop)
  #print(paste0(dummyTotal," ",total,"  PVP: ",percentValuePixels))
  
  threshold <- .8
  
  if (percentValuePixels <= threshold ) {
    print(paste0("ERROR: SPATIAL OVERLAP WITH ",rasterImage," IS ",percentValuePixels,"."))
    print(paste0("THRESHOLD VALUE IS ",threshold,"."))
    return(c(NA,NA))
  } else {
    #return mean and std statistics about the cropped raster
    return(c(cellStats(raster, stat='mean', na.rm=TRUE),
             cellStats(raster, stat='sd', na.rm=TRUE)))
  }

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
} 

# returns spatial stats for a single FFP decDay (for TCT files specifically)
extractStats_TCT <- function(FFP_file,dayVar,rVar,rasterImage,path) {
  
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
} 




