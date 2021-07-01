
library(raster)


# inputs: raster tif, FFP_output file, dayVar, and rVar
getTCTStats <- function(FFP_file) {

  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,TCTb_mean=NA,TCTb_std=NA,TCTg_mean=NA,TCTg_std=NA,TCTw_mean=NA,TCTw_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    
    
    
    
    #find out most recent landsat image
    #grab that landsat image
    #find out stats about the landsat image in the following rows
    
    # TCT_brightness, TCT_greenness, TCT_wetness
  
    setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    TCTimage <- stack("test.tif")
    #print(TCTimage)

    bright <- subset(TCTimage,1)
    green <- subset(TCTimage,2)
    wet <- subset(TCTimage,3)
    
    brightnessRast <- projectRaster(bright, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    greennessRast <- projectRaster(green, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    wetnessRast <- projectRaster(wet, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    #print(brightnessRast)
  
    
    #print(brightnessRast)
    
    
    stats_brightness <- extractStats_TCT(FFP,decDay,.75,brightnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    stats_greenness <- extractStats_TCT(FFP,decDay,.75,greennessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    stats_wetness <- extractStats_TCT(FFP,decDay,.75,wetnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    
    
    
    
    
    
    #print(stats[2])
    new_row <- c(year,decDay,stats_brightness[1],
                              stats_brightness[2],
                              stats_greenness[1],
                              stats_greenness[2],
                              stats_wetness[1],
                              stats_wetness[2]
                 )
    
    results <- rbind(results,new_row)
    print(results)
    
    
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    
  }
  
  return(results)
  
} 



