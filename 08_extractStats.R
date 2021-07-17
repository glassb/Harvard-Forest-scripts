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
library(terra)

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/misc")

# returns spatial stats for a single FFP decDay 
extractStats_wm <- function(FFP_file,dayVar,rVar,rasterImage,path,fwm_file) {
  
  #filters by dec.day and r
  FFP_filtered <- FFP_file %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  
  fwm_file_filter <- fwm_file %>%
    filter(dec.day == dayVar) %>%
    mutate(f_value = as.numeric(f),
           easting = as.numeric(x+732275),
           northing=as.numeric(y+4713368))
  
  #print(fwm_file_filter)
  
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
        #print(paste0("THRESHOLD VALUE IS ",threshold,"."))
        return(c(NA,NA))
      } else {
        
        
        #create the weighted mean raster
        fwm_raster <- raster()
        extent(fwm_raster) <- extent(raster)
        crs(fwm_raster) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
        res(fwm_raster) <- res(raster)
        
        fwm_raster <-rasterize(x=fwm_file_filter[, c('easting','northing')],
                               y=fwm_raster,
                               #field=1,
                               field=fwm_file_filter[, 'f_value'],
                               fun=mean)
        
      
        #print(fwm_file_filter)
        #plot(fwm_raster)
        
        #crs(fwm_raster) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"
        #print(fwm_raster)
        #fwm_raster <- crop(fwm_raster,sps)
        
        
        #crs(fwm_raster) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"
        
        
        
        # par(mfrow=c(2,2))
        # 
        # plot(fwm_raster)
        # plot(raster)
        
        
        
                               #na.rm=TRUE)
        #print(fwm_file_filter)
        #plot(fwm_raster)
        
        #fwm_raster <- rasterFromXYZ(fwm_file[, c('x', 'y','f')],res=res(raster),crs="+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")
        #extent(fwm_raster) <- extent(raster)
        
      
        #fwm_raster <- raster(xmn=731000, xmx=733000, ymn=4712000, ymx=4715000, res=10, crs="+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")
        #fwm_raster <- rasterize(fwm_file[, c('x', 'y')], fwm_raster,field=fwm_file$f,fun="last",na.rm=TRUE)
        
        
        raster_value_sums <- cellStats(fwm_raster, stat='sum', na.rm=TRUE)
        #print(raster_value_sums)
        #weighted mean
        
        raster_wm <- (raster*fwm_raster)/raster_value_sums
        
        #raster_wm <- raster
        
        print(c(cellStats(raster_wm, stat='sum', na.rm=TRUE),
                cellStats(raster, stat='sd', na.rm=TRUE)))
        
        #return mean and std statistics about the cropped raster
        return(c(cellStats(raster_wm, stat='sum', na.rm=TRUE),
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
extractStats_TCT_wm <- function(FFP_file,dayVar,rVar,rasterImage,path,fwm_file) {
  
  #filters by dec.day and r
  FFP_filtered <- FFP_file %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  
  fwm_file_filter <- fwm_file %>%
    filter(dec.day == dayVar) %>%
    mutate(f_value = as.numeric(f),
           easting = as.numeric(x+732275),
           northing=as.numeric(y+4713368))
  
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
    #print(raster)
    
    fwm_raster <- raster()
    extent(fwm_raster) <- extent(raster)
    crs(fwm_raster) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    res(fwm_raster) <- res(raster)
    
    fwm_raster <-rasterize(x=fwm_file_filter[, c('easting','northing')],
                           y=fwm_raster,
                           #field=1,
                           field=fwm_file_filter[, 'f_value'],
                           fun=mean)
    
    raster_value_sums <- cellStats(fwm_raster, stat='sum', na.rm=TRUE)
    print(raster_value_sums)
    #weighted mean
    
    raster_wm <- (raster*fwm_raster)/raster_value_sums
    
    print(c(cellStats(raster_wm, stat='sum', na.rm=TRUE),
            cellStats(raster, stat='sd', na.rm=TRUE)))
    
    #return stats
    return(c(cellStats(raster_wm, stat='sum', na.rm=TRUE),
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
    #print(paste0("THRESHOLD VALUE IS ",threshold,"."))
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




