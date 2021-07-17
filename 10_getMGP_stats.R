# Benjamin Glass
# Last Update: July 17, 2021

# Script Overview: this master script extracts spatial statistics based on
  # megaplot data inputs (percent coniferous and percent 
  # decidious) for EMS and NEON towers

library(tidyverse)

#read in source files
setwd("/Users/benjaminglass/HF21-Scripts")
source("08_extractStats.R")


# ======== EMS TOWER FUNCTIONS

# returns spatial stats on deciduous percentage for EMS tower data
EMS_getMGP_D_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create dataframe
  results <- data.frame(year=NA,decDay=NA,MGP_D_mean=NA,MGP_D_std=NA)
  year <- substr(FFP_file,14,15)
  month <- substr(FFP_file,17,18)
  
  #make sure month is correct
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  #print(year)
  #print(month)
  
  #get weighted mean file
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_f_17-20")
  fwm_file <- read.csv(paste0("FFP.hr.",year,".",month,".csv"))
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #create f weighted mean file with just decDay specified
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
    
      #xtract stats
      stats50 <- extractStats_wm(FFP,decDay,.5,
                                  "DecidiousProportionMegaPlot.tif",
                                  "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters",
                                  FFP_f)
   
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    }
      
  }
  return(results)
}

# returns spatial stats on coniferous percentage for EMS tower data
EMS_getMGP_C_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,MGP_C_mean=NA,MGP_C_std=NA)
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
    #print(FFP)
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
    
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "ConiferProportionMegaPlot.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters",
                                 FFP_f)
    
      new_row <- c(year,decDay,stats50[1],stats50[2])
      #print(new_row)
      results <- rbind(results,new_row)
      
    }
  }
  return(results)
}




# ========= NEON TOWER FUNCTIONS

# returns spatial stats on deciduous percentage for NEON tower data
NEON_getMGP_D_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create dataframe
  results <- data.frame(year=NA,decDay=NA,MGP_D_mean=NA,MGP_D_std=NA)
  year <- substr(FFP_file,19,20)
  month <- substr(FFP_file,22,23)
  print(year)
  print(month)
  
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  
  #print(month)
  
  #get weighted mean file
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_f_17-20")
  fwm_file <- read.csv(paste0("FFP.neon.hr.",year,".",month,".csv"))
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #create f weighted mean file with just decDay specified
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      
      stats50 <- extractStats_wm(FFP,decDay,
                                 .5,
                                 "DecidiousProportionMegaPlot.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters",
                                 FFP_f)
     
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    }
    
  }
  return(results)
}

# returns spatial stats on coniferous percentage for NEON tower data
NEON_getMGP_C_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create dataframe
  results <- data.frame(year=NA,decDay=NA,MGP_C_mean=NA,MGP_C_std=NA)
  year <- substr(FFP_file,19,20)
  month <- substr(FFP_file,22,23)
  print(year)
  print(month)
  
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  
  #print(month)
  
  #get weighted mean file
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_f_17-20")
  fwm_file <- read.csv(paste0("FFP.neon.hr.",year,".",month,".csv"))
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    #print(FFP)
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
     
      stats50 <- extractStats_wm(FFP,decDay,
                                 .50,
                                 "ConiferProportionMegaPlot.tif",
                                 "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters",
                                 FFP_f)
      
      new_row <- c(year,decDay,stats50[1],stats50[2])
      #print(new_row)
      results <- rbind(results,new_row)
      
    }
  }
  return(results)
}


