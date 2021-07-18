# Benjamin Glass
# Last Update: July 17, 2021

library(tidyverse)
setwd("/Users/benjaminglass/HF21-Scripts")
source("08_extractStats.R")


# ======== EMS TOWER FUNCTIONS

#these functions all do the same spatial stats, but just for different years of LIDAR
EMS_getLIDAR_2014_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
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
    
    stats50 <- extractStats_wm(FFP,decDay,
                               .50,
                               "LIDAR_CHM_2014.tif",
                               "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                               FFP_f)
  
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    }
  }
  return(results)
  }

EMS_getLIDAR_2016_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
  
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "LIDAR_CHM_2016.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                 FFP_f)
   
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    }
      
    }
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    return(results)
  }

EMS_getLIDAR_2017_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {

      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "LIDAR_CHM_2017.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                 FFP_f)
    
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    
    }
      
    }
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    return(results)
    
  }

EMS_getLIDAR_2018_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
    
    
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "LIDAR_CHM_2018.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                 FFP_f)
    
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    
    }
    }
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    return(results)
  }
  
EMS_getLIDAR_2019_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
    
   
    stats50 <- extractStats_wm(FFP,decDay,
                               .50,
                               "LIDAR_CHM_2019.tif",
                               "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                               FFP_f)
  
    new_row <- c(year,decDay,stats50[1],stats50[2])
    results <- rbind(results,new_row)
    
    }
  }
    
  return(results)
    
}




# ======== NEON TOWER FUNCTIONS

NEON_getLIDAR_2014_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
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
      
        stats50 <- extractStats_wm(FFP,decDay,
                                   .50,
                                   "LIDAR_CHM_2014.tif",
                                   "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                   FFP_f)
   
        new_row <- c(year,decDay,stats50[1],stats50[2])
        results <- rbind(results,new_row)
    }
  }
  return(results)
}

NEON_getLIDAR_2016_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "LIDAR_CHM_2016.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                 FFP_f)
     
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    }
    
  }
  
  return(results)
}

NEON_getLIDAR_2017_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "LIDAR_CHM_2017.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                 FFP_f)
   
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
      
    }
    
  }

  return(results)
  
}

NEON_getLIDAR_2018_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "LIDAR_CHM_2018.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                 FFP_f)
   
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
      
    }
  }
  
  return(results)
}

NEON_getLIDAR_2019_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
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
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    #get stats for the DecProp.tif
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "LIDAR_CHM_2019.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters",
                                 FFP_f)
    
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
      
    }
  }
  
  return(results)
  
}
  


