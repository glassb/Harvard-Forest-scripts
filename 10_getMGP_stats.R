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
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-19")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  
  #create dataframe
  results <- data.frame(year=NA,decDay=NA,MGP_D_mean=NA,MGP_D_std=NA)
  year <- substr(FFP_file,14,15)
  #print(year)
  
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    
    #get stats for the DecProp.tif
    stats25 <- extractStats(FFP,decDay,.25,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    stats50 <- extractStats(FFP,decDay,.5,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    stats75 <- extractStats(FFP,decDay,.75,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    
    if (is.null(stats25[1]) || is.null(stats50[1] || is.null(stats75[1]))) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      weighted_mean <- (stats25[1]*.6)+(stats50[1]*.3)+(stats75[1]*.1)
      weighted_std <- (stats25[2]*.6)+(stats50[2]*.3)+(stats75[2]*.1)
      
      new_row <- c(year,decDay,weighted_mean,weighted_std)
      results <- rbind(results,new_row)
      
    }
  }
  
  return(results)
  
}

# see above
getMGP_C_stats <- function(FFP_file)  {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-19")
  file <- read.csv(FFP_file,stringsAsFactors=FALSE)
  results <- data.frame(year=NA,decDay=NA,MGP_C_mean=NA,MGP_C_std=NA)
  #for each unique day in the FFP data file
  year <- substr(FFP_file,14,15)
  
  for (decDay in unique(file$dec.day)) { 
    #print(decDay)
    
    #create a variable that has just the decDay that is being looped over right now
    FFP <- file[file$dec.day == decDay, ]
    FFP <- FFP[complete.cases(FFP), ]
    #print(FFP)
    
    #get stats for the ConProp.tif
    stats25 <- extractStats(FFP,decDay,.25,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    stats50 <- extractStats(FFP,decDay,.50,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    stats75 <- extractStats(FFP,decDay,.75,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
    
    if (is.null(stats25[1]) || is.null(stats50[1] || is.null(stats75[1]))) {
      new_row <- c(year,decDay,NA,NA)
      results <- rbind(results,new_row)
    } else {
      
      weighted_mean <- (stats25[1]*.6)+(stats50[1]*.3)+(stats75[1]*.1)
      weighted_std <- (stats25[2]*.6)+(stats50[2]*.3)+(stats75[2]*.1)
      
      new_row <- c(year,decDay,weighted_mean,weighted_std)
      #print(new_row)
      results <- rbind(results,new_row)
      
    }
  }
  
  return(results)
  
}

