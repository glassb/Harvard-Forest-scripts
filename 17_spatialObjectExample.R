# Benjamin Glass
# Last Update: July 2, 2021

# Script Overview: this master script iterates over all FFP output files
# and extracts spatial statistics for each decimal Day in each year of
# FFP output data. The spatial input data includes Megaplot proportions,
# Lidar, and TCT landsat 8 imagery.

library(tidyverse)
library(rgdal)

#read in source files
setwd("/Users/benjaminglass/HF21-Scripts")
source("10_getMGP_stats.R")
setwd("/Users/benjaminglass/HF21-Scripts")
source("12_getLIDAR_stats.R")
setwd("/Users/benjaminglass/HF21-Scripts")
source("13_getTCT_stats.R")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")

#get all FFP output files for EMS tower
temp = list.files(pattern="*.csv")

#create master results file (which will be our final product)
masterResults_all <- data.frame(year=NA,
                                decDay=NA,
                                MGP_C_mean = NA,
                                MGP_C_std = NA,
                                MGP_D_mean = NA,
                                MGP_D_std = NA,
                                L_mean = NA,
                                L_std = NA,
                                TCTb_mean=NA,
                                TCTb_std=NA,
                                TCTg_mean=NA,
                                TCTg_std=NA,
                                TCTw_mean=NA,
                                TCTw_std=NA)

doneFiles <- data.frame(file=NA)

#variable to count files completed for debugging
iter <- 0


#===================== FOR LOOP ==========================

#looping over all files in temp
for (currentFile in temp) {
  iter <- iter+1
  
  #printing
  print("============================================================")
  print(paste0("FFP FILE ",iter," OF ",length(temp),": ",currentFile))
  print("============================================================")
  
  #get year of FFP file
  year <- substr(currentFile,14,15)

  
}





