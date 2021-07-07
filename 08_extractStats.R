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
  
  #print(rast)
  
  #mask landsat imagery by FFP spatial polygon
  masked <- mask(rast,sps)
  #plot(masked)
  
  #print(masked)
  #print(min(values(masked)))
  #print(all.equal(rast,masked))
  
  if (all(is.na(values(masked)))) {
    print("NO OVERLAP BETWEEN SPS AND RASTER")
    return(c(NA,NA))
  } else {
  
  #crop masked raster by the outline of the polygon
  raster <- crop(masked,sps)
  #plot(raster)
  
  #plot(masked)

  
  
  # NEED TO DEVELOP THIS
  #these calculate if the polygon shape covers at least 80% of the raster so that we are not getting
    # raster summary statistics for insignificant portions of the raster (i.e. only a few tiles). This
    # code will calculate this for all raster masks, but it is mainly for megaplot data.
  #Tpixels <- cellStats(raster,stat='count',na.rm=FALSE)
  
  #ValuePixels <- freq(raster, value=NA)
  #print(Vpixels)
  
  #testR <- raster(ncol=50, nrow=70, xmn=-730000, xmx=735000, ymn=-4720000, ymx=4725000,proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  #res(testR) <- 10
  
  
  #NEED TO DO THIS
  
  # setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables")
  # dummyRaster <- raster("dummyRaster.tif")
  # dummyCrop <- crop(mask(dummyRaster,sps),sps)
  # dummyTotal <- ncell(dummyCrop)
  # total <- ncell(raster)
  # percentValuePixels <- total/dummyTotal
  
  
  
  
  #print(percentValuePixels)
  
  #print(raster)
  
  #print(paste0(dummyTotal," ",total,"  PVP: ",percentValuePixels))
  
  if (percentValuePixels <= 0 ) {
    print("FFP NOT SIGNIF. COVERING RASTER")
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




