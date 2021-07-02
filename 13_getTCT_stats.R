# Benjamin Glass
# Last Update: July 2, 2021

# Script Overview: getting spatial stats on TCT Landsat 8 imagery
library(raster)


# inputs: raster tif, FFP_output file, dayVar, and rVar
getTCTStats <- function(FFP_file) {

  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  
  #read in FFP file
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create blank df
  results <- data.frame(year=NA,decDay=NA,TCTb_mean=NA,TCTb_std=NA,TCTg_mean=NA,TCTg_std=NA,TCTw_mean=NA,TCTw_std=NA)
  
  #get year from given FFP file name
  year <- substr(FFP_file,14,15)
  
  #loop over each decDay in FFP file
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    
    # ===== GETS THE MOST RECENT LANDSAT IMAGE
    TCT_image <- getRightDataDate(FFP_file,decDay)
    
    # if there is no viable TCT image, then return a blank data frame row
    if (is.null(TCT_image)) {
      print("NO APPROPRIATE SPECTRAL IMAGES")
      
      new_row <- c(year,decDay,NA,NA,NA,NA,NA,NA)
      results <- rbind(results,new_row)
      print(results)
    
    # otherwise, proceed as normal
    } else {

      print("TCT IMAGE IS A GO")
      print(TCT_image)
    
  
    setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    TCTimage <- stack(TCT_image)
    
    # the following lines extract spatial stats based on tctb,tctg,tctw
    bright <- subset(TCTimage,1)
    green <- subset(TCTimage,2)
    wet <- subset(TCTimage,3)
    
    brightnessRast <- projectRaster(bright, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    greennessRast <- projectRaster(green, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    wetnessRast <- projectRaster(wet, crs = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    stats_brightness <- extractStats_TCT(FFP,decDay,.75,brightnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    stats_greenness <- extractStats_TCT(FFP,decDay,.75,greennessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    stats_wetness <- extractStats_TCT(FFP,decDay,.75,wetnessRast,"/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    
    new_row <- c(year,decDay,stats_brightness[1],
                              stats_brightness[2],
                              stats_greenness[1],
                              stats_greenness[2],
                              stats_wetness[1],
                              stats_wetness[2]
                 )
    
    results <- rbind(results,new_row)
    print(results)
    
    }
  }
  
  return(results)
  
} 

# function to return the most recent L8 image based on the decDay of a given FFP_file
getRightDataDate <- function(FFP_file,decDay) {
  
  require(lubridate)
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
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
  
  print(matches)
  
  print(dim(matches))
  if(nrow(matches) == 0) {
    print("NO MATCHES")
    return(NULL)
  } else {
    file <- head(matches,1)$fileName
    print(file)
    return(file)
  }
  
  
}






