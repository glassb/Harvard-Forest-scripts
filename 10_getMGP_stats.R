# Benjamin Glass
# Last Update: July 2, 2021

# Script Overview: this master script gets statistics based on
  # megaplot data inputs (percent coniferous and percent 
  # decidious)

library(tidyverse)

#read in source files
setwd("/Users/benjaminglass/HF21-Scripts")
source("08_extractStats.R")


# this function returns dataframe of spatial stats
getMGP_D_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create dataframe
  results <- data.frame(year=NA,decDay=NA,MGP_D_mean=NA,MGP_D_std=NA)
  year <- substr(FFP_file,14,15)
  month <- substr(FFP_file,17,17)
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
    
    
    #get stats for the DecProp.tif
    #stats25 <- extractStats(FFP,decDay,.25,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    stats50 <- extractStats_wm(FFP,decDay,.5,"DecidiousProportionMegaPlot.tif",
                                          "/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters",
                                          FFP_f)
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
      
      new_row <- c(year,decDay,stats50[1],stats50[2])
      results <- rbind(results,new_row)
    }
      
  }
  return(results)
}

# see above
getMGP_C_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,MGP_C_mean=NA,MGP_C_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  month <- substr(FFP_file,17,17)
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
    #print(FFP)
    
    FFP_f <- fwm_file[fwm_file$dec.day == decDay, ]
    FFP_f <- FFP_f[complete.cases(FFP_f), ]
    
    if (nrow(FFP_f)==0) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
    
    #get stats for the ConProp.tif
    #stats25 <- extractStats(FFP,decDay,.25,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    stats50 <- extractStats_wm(FFP,decDay,.50,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters",FFP_f)
    #stats75 <- extractStats(FFP,decDay,.75,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    
    # if (is.null(stats25[1]) || is.null(stats50[1] || is.null(stats75[1]))) {
    #   new_row <- c(year,decDay,NA,NA)
    #   results <- rbind(results,new_row)
    # } else {
    #   
    #   weighted_mean <- (stats25[1]*.6)+(stats50[1]*.3)+(stats75[1]*.1)
    #   weighted_std <- (stats25[2]*.6)+(stats50[2]*.3)+(stats75[2]*.1)
    #   
    #   new_row <- c(year,decDay,weighted_mean,weighted_std)
      #print(new_row)
    
      new_row <- c(year,decDay,stats50[1],stats50[2])
      #print(new_row)
      results <- rbind(results,new_row)
      
    }
  }
  return(results)
  }


