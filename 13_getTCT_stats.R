# Benjamin Glass
# Last Update: July 17, 2021

# Script Overview: getting spatial stats on TCT Landsat 8 imagery
library(raster)



# ======== EMS TOWER FUNCTIONS

# inputs: raster tif, FFP_output file, dayVar, and rVar
EMS_getTCTStats <- function(FFP_file) {

  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  
  #read in FFP file
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create blank df
  results <- data.frame(year=NA,decDay=NA,TCTb_mean=NA,TCTb_std=NA,TCTg_mean=NA,TCTg_std=NA,TCTw_mean=NA,TCTw_std=NA)
  
  #get year from given FFP file name
  year <- substr(FFP_file,14,15)
  month <- substr(FFP_file,17,18)
  #print(year)
  #print(month)
  
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  
  #get weighted mean file
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_f_17-20")
  fwm_file <- read.csv(paste0("FFP.hr.",year,".",month,".csv"))
  
  #loop over each decDay in FFP file
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
    
    # ===== GETS THE MOST RECENT LANDSAT IMAGE
    TCT_image <- EMS_getRightDataDate(FFP_file,decDay)
    
    # if there is no viable TCT image, then return a blank data frame row
    if (is.null(TCT_image)) {
      print("NO APPROPRIATE SPECTRAL IMAGES")
      
      new_row <- c(year,decDay,NA,NA,NA,NA,NA,NA)
      results <- rbind(results,new_row)
      #print(results)
    
    # otherwise, proceed as normal
    } else {
    
  
      setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802")
      TCTimage <- stack(TCT_image)
      
      # the following lines extract spatial stats based on tctb,tctg,tctw
      bright <- subset(TCTimage,1)
      green <- subset(TCTimage,2)
      wet <- subset(TCTimage,3)
      
      brightnessRast <- projectRaster(bright, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      greennessRast <- projectRaster(green, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      wetnessRast <- projectRaster(wet, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      
      #print(greennessRast)
      
      stats_brightness50 <- EMS_extractStats_TCT_wm(FFP,decDay,.50,brightnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802",FFP_f)
      stats_greenness50 <- EMS_extractStats_TCT_wm(FFP,decDay,.50,greennessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802",FFP_f)
      stats_wetness50 <- EMS_extractStats_TCT_wm(FFP,decDay,.50,wetnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802",FFP_f)
   
      new_row <- c(year,decDay,
                   stats_brightness50[1],
                   stats_brightness50[2],
                   stats_greenness50[1],
                   stats_greenness50[2],
                   stats_wetness50[1],
                   stats_wetness50[2])
      
      results <- rbind(results,new_row)
      #print(results)
    
    }
    }
  }
    
    return(results)
  }

# function to return the most recent L8 image based on the decDay of a given FFP_file
EMS_getRightDataDate <- function(FFP_file,decDay) {
  
  require(lubridate)
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802")
  temp = list.files(pattern="*.tif")
  
  #create blank dataframe for all the dates of TCT files
  TCT_dates <- data.frame(year=NA,julianDay=NA,month=NA,day=NA,fileName=NA)
  
  #load in all the files all their dates
  for (currentFile in temp) {
    year <- substr(currentFile,13,16)
    month <- substr(currentFile,17,18)
    day <- substr(currentFile,19,20)
    julianDay <- yday(as.Date(paste0(year,"-",month,"-",day)))
    #print(paste0(year," ",month," ",day," ",julianDay))
    
    new_row <- c(year,julianDay,month,day,currentFile)
    TCT_dates <- rbind(TCT_dates,new_row)
  }
  
  #print(TCT_dates)
  
  
  #get current FFP file and metrics
  
  #FFP_file <- "FFP.hr.lines.20.5"
  currentFile <- FFP_file
  FFPyear <- as.numeric(substr(FFP_file,14,15))
  FFPdecDay <- decDay
  
  #filter for the most recent file
  matches <- TCT_dates %>%
    mutate(yearDiff=FFPyear-as.numeric(substr(year,3,4)))%>%
    filter(yearDiff>=0)
  
  #print(matches)
  minYearDiff <- min(matches$yearDiff)
  
  matches <- matches %>%
    filter(yearDiff == minYearDiff) %>%
    mutate(DayDiff = as.numeric(FFPdecDay)-as.numeric(julianDay)) %>%
    filter(DayDiff >= 0)
  
  minDayDiff <- min(matches$DayDiff)
  
  matches <- matches %>%
    filter(DayDiff == minDayDiff)
  
  #print(matches)
  
  #print(dim(matches))
  if(nrow(matches) == 0) {
    print("NO MATCHES")
    return(NULL)
  } else {
    file <- head(matches,1)$fileName
    #print(file)
    return(file)
  }
  
  
}




# ======== NEON TOWER FUNCTIONS

# inputs: raster tif, FFP_output file, dayVar, and rVar
NEON_getTCTStats <- function(FFP_file) {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create blank df
  results <- data.frame(year=NA,decDay=NA,TCTb_mean=NA,TCTb_std=NA,TCTg_mean=NA,TCTg_std=NA,TCTw_mean=NA,TCTw_std=NA)
  
  #get year from given FFP file name
  year <- substr(FFP_file,19,20)
  month <- substr(FFP_file,22,23)
  print(year)
  print(month)
  
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  
  
  #get weighted mean file
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_f_17-20")
  fwm_file <- read.csv(paste0("FFP.neon.hr.",year,".",month,".csv"))
  
  #loop over each decDay in FFP file
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      # ===== GETS THE MOST RECENT LANDSAT IMAGE
      TCT_image <- NEON_getRightDataDate(FFP_file,decDay)
      
      # if there is no viable TCT image, then return a blank data frame row
      if (is.null(TCT_image)) {
        print("NO APPROPRIATE SPECTRAL IMAGES")
        
        new_row <- c(year,decDay,NA,NA,NA,NA,NA,NA)
        results <- rbind(results,new_row)
        #print(results)
        
        # otherwise, proceed as normal
      } else {
          
          setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802")
          TCTimage <- stack(TCT_image)
          
          # the following lines extract spatial stats based on tctb,tctg,tctw
          bright <- subset(TCTimage,1)
          green <- subset(TCTimage,2)
          wet <- subset(TCTimage,3)
          
          brightnessRast <- projectRaster(bright, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
          greennessRast <- projectRaster(green, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
          wetnessRast <- projectRaster(wet, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
          
          
          stats_brightness50 <- NEON_extractStats_TCT_wm(FFP,decDay,.50,brightnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802",FFP_f)
          stats_greenness50 <- NEON_extractStats_TCT_wm(FFP,decDay,.50,greennessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802",FFP_f)
          stats_wetness50 <- NEON_extractStats_TCT_wm(FFP,decDay,.50,wetnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802",FFP_f)
          
          new_row <- c(year,decDay,
                       stats_brightness50[1],
                       stats_brightness50[2],
                       stats_greenness50[1],
                       stats_greenness50[2],
                       stats_wetness50[1],
                       stats_wetness50[2])
          
          results <- rbind(results,new_row)
          #print(results)
        
      }
    }
  }
  
  return(results)
}

# function to return the most recent L8 image based on the decDay of a given FFP_file
NEON_getRightDataDate <- function(FFP_file,decDay) {
  
  require(lubridate)
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802")
  temp = list.files(pattern="*.tif")
  
  #create blank dataframe for all the dates of TCT files
  TCT_dates <- data.frame(year=NA,julianDay=NA,month=NA,day=NA,fileName=NA)
  
  #load in all the files all their dates
  for (currentFile in temp) {
    year <- substr(currentFile,13,16)
    month <- substr(currentFile,17,18)
    day <- substr(currentFile,19,20)
    julianDay <- yday(as.Date(paste0(year,"-",month,"-",day)))
    #print(paste0(year," ",month," ",day," ",julianDay))
    
    new_row <- c(year,julianDay,month,day,currentFile)
    TCT_dates <- rbind(TCT_dates,new_row)
  }
  
  #print(TCT_dates)
  
  
  #get current FFP file and metrics
  
  #FFP_file <- "FFP.hr.lines.20.5"
  currentFile <- FFP_file
  FFPyear <- as.numeric(substr(FFP_file,19,20))
  FFPdecDay <- decDay
  
  #filter for the most recent file
  matches <- TCT_dates %>%
    mutate(yearDiff=FFPyear-as.numeric(substr(year,3,4)))%>%
    filter(yearDiff>=0)
  
  #print(matches)
  minYearDiff <- min(matches$yearDiff)
  
  matches <- matches %>%
    filter(yearDiff == minYearDiff) %>%
    mutate(DayDiff = as.numeric(FFPdecDay)-as.numeric(julianDay)) %>%
    filter(DayDiff >= 0)
  
  minDayDiff <- min(matches$DayDiff)
  
  matches <- matches %>%
    filter(DayDiff == minDayDiff)
  
  #print(matches)
  
  #print(dim(matches))
  if(nrow(matches) == 0) {
    print("NO MATCHES")
    return(NULL)
  } else {
    file <- head(matches,1)$fileName
    #print(file)
    return(file)
  }
  
  
}


