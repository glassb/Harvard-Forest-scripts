

library(tidyverse)
setwd("/Users/benjaminglass/HF21-Scripts")
source("08_extractStats.R")

#choose a single month of footprints


getMGP_D_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,MGP_D_mean=NA,MGP_D_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  #print(year)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.75,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    #print(stats)
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

getMGP_C_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,MGP_C_mean=NA,MGP_C_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  
  for (decDay in unique(file$dec.day)) { 
    print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    #print(FFP)
    
    #get stats for the DecProp.tif
    
    stats <- extractStats(FFP,decDay,.75,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    
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







