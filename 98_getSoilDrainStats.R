# Benjamin Glass
# Last Update: July 2, 2021

# Script Overview: this master script iterates over all FFP output files
# and extracts spatial statistics for each decimal Day in each year of
# FFP output data. The spatial input data includes Megaplot proportions,
# Lidar, and TCT landsat 8 imagery.

library(tidyverse)
library(rgdal)

#read in source files
setwd("/Users/benjaminglass/HF21-Scripts")
source("10_getMGP_stats.R")
setwd("/Users/benjaminglass/HF21-Scripts")
source("12_getLIDAR_stats.R")
setwd("/Users/benjaminglass/HF21-Scripts")
source("13_getTCT_stats.R")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")

#get all FFP output files for EMS tower
temp = list.files(pattern="*.csv")

#create master results file (which will be our final product)
masterResults_all <- data.frame(year=NA,
                                decDay=NA,
                                SD_mean = NA,
                                SD_std = NA)

doneFiles <- data.frame(file=NA)

#variable to count files completed for debugging
iter <- 0


#===================== FOR LOOP ==========================

#looping over all files in temp
for (currentFile in temp) {
  iter <- iter+1
  
  #printing
  print("============================================================")
  print(paste0("FFP FILE ",iter," OF ",length(temp),": ",currentFile))
  print("============================================================")
  
  #get year of FFP file
  year <- substr(currentFile,14,15)
  
  #currentFile <- "FFP.hr.lines.00.3.csv"
  # FIRST SET OF FUNCTIONS FOR MEGAPLOT
  print("--------------------------------------Soil Drainage")
  results <- getSD_Stats(currentFile)
  
  print(paste0(currentFile,":----------------------------------------------------------------------------DONE"))
  masterResults_all <- rbind(masterResults_all,results)
  
  doneFile <- (currentFile)
  doneFiles <- rbind(doneFiles,doneFile)
  
}



write.csv(masterResults_all,"/Users/benjaminglass/Desktop/HF21/00_Datasets/all_results_0714_soildrainage.csv", row.names = FALSE)


library(tidyverse)


getSD_Stats <- function(FFP_file) {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create dataframe
  results <- data.frame(year=NA,decDay=NA,SD_mean=NA,SD_std=NA)
  year <- substr(FFP_file,14,15)
  #print(year)
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the DecProp.tif
    #stats25 <- extractStats(FFP,decDay,.25,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    stats50 <- extractStats_SD(FFP,decDay,.5,"SoilDrainage(0-ND)_utm.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/SoilDrainage_rasters")
    #stats75 <- extractStats(FFP,decDay,.75,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    
    # if (is.null(stats25[1]) || is.null(stats50[1] || is.null(stats75[1]))) {
    #   new_row <- c(year,decDay,NA,NA)
    #   results <- rbind(results,new_row)
    # } else {
    #   
    #   weighted_mean <- (stats25[1]*.6)+(stats50[1]*.3)+(stats75[1]*.1)
    #   weighted_std <- (stats25[2]*.6)+(stats50[2]*.3)+(stats75[2]*.1)
    #   
    #   new_row <- c(year,decDay,weighted_mean,weighted_std)
    print(stats50)
    new_row <- c(year,decDay,stats50[1],stats50[2])
    results <- rbind(results,new_row)
    
  }
  return(results)
  
  
}


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
extractStats_SD <- function(FFP_file,dayVar,rVar,rasterImage,path) {
  
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
    rast[rast==0] <- NA
    #print(rast)
    
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
      
      threshold <- 0
      
      if (percentValuePixels <= threshold ) {
        #print(paste0("ERROR: SPATIAL OVERLAP WITH ",rasterImage," IS ",percentValuePixels,"."))
        #print(paste0("THRESHOLD VALUE IS ",threshold,"."))
        #return(c(NA,NA))
        return(c(cellStats(raster, stat='mean', na.rm=TRUE),
                 cellStats(raster, stat='sd', na.rm=TRUE)))
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



