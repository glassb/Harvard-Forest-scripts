

library(tidyverse)
setwd("/Users/benjaminglass/HF21-Scripts")
source("08_extractStats.R")

#choose a single month of footprints


getLIDAR_2014_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L14_mean=NA,L14_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.75,"LIDAR_CHM_2014.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
    #print(stats[2])
    new_row <- c(year,decDay,stats[1],stats[2])
    results <- rbind(results,new_row)
    print(results)
    
    
    # FFP_prime <- FFP %>%
    #   mutate(mean = stats[1],
    #          std = stats[2])
    
    
  }
  
  return(results)
  
}

getLIDAR_2016_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L15_mean=NA,L15_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.75,"LIDAR_CHM_2016.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
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
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L17_mean=NA,L17_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.75,"LIDAR_CHM_2017.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
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
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L18_mean=NA,L18_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.75,"LIDAR_CHM_2018.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
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
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,L19_mean=NA,L19_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.75,"LIDAR_CHM_2019.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
    
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


