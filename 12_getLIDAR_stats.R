# Benjamin Glass
# Last Update: July 2, 2021

library(tidyverse)
setwd("/Users/benjaminglass/HF21-Scripts")
source("08_extractStats.R")

#these functions all do the same spatial stats, but just for different years of LIDAR
getLIDAR_2014_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-19")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  year <- substr(FFP_file,14,15)
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the lidar raster image
    stats <- extractStats(FFP,decDay,.25,"LIDAR_CHM_2014.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
    new_row <- c(year,decDay,stats[1],stats[2])
    results <- rbind(results,new_row)
    
  }
  
  return(results)
  
}

getLIDAR_2016_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-19")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.25,"LIDAR_CHM_2016.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
    #print(stats[2])
    new_row <- c(year,decDay,stats[1],stats[2])
    results <- rbind(results,new_row)
    #print(results)
    
    
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    
  }
  
  return(results)
  
}

getLIDAR_2017_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-19")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.25,"LIDAR_CHM_2017.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
    #print(stats[2])
    new_row <- c(year,decDay,stats[1],stats[2])
    results <- rbind(results,new_row)
    #print(results)
    
    
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    
  }
  
  return(results)
  
}

getLIDAR_2018_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-19")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.25,"LIDAR_CHM_2018.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
    #print(stats[2])
    new_row <- c(year,decDay,stats[1],stats[2])
    results <- rbind(results,new_row)
    #print(results)
    
    
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    
  }
  
  return(results)
  
}

getLIDAR_2019_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-19")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L_mean=NA,L_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.25,"LIDAR_CHM_2019.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
    #print(stats[2])
    new_row <- c(year,decDay,stats[1],stats[2])
    results <- rbind(results,new_row)
    #print(results)
    
    
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    
  }
  
  return(results)
  
}


