# Benjamin Glass
# Last Update: July 17, 2021

# Script Overview: this master script iterates over all EMS FFP output files
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
  
  #currentFile <- "FFP.hr.lines.17.10.csv"
  # FIRST SET OF FUNCTIONS FOR MEGAPLOT
  
  # print("--------------------------------------MGPD")
  # results_MGP_D <- EMS_getMGP_D_stats(currentFile)
  # 
  # print("--------------------------------------MGPC")
  # results_MGP_C <- EMS_getMGP_C_stats(currentFile)
  # 
  # 
  # 
  # #SECOND SET OF FUNCTIONS FOR LIDAR
  # #if statement that decides which lidar year to get
  # 
  # print("--------------------------------------LIDAR")
  # if (as.numeric(year)>= 80 | as.numeric(year) <= 14 ) {
  #   print(paste0("ERROR: CURRENT FILE YEAR ",year," DOES NOT HAVE APPROP. LIDAR DATA"))
  #   print("USING 2014 LIDAR DATA")
  #   results_LIDAR <- EMS_getLIDAR_2014_stats(currentFile)
  # } else if (as.numeric(year) < 16) {
  #   print(paste0(year," Lidar 2014"))
  #   results_LIDAR <- EMS_getLIDAR_2014_stats(currentFile)
  # } else if (as.numeric(year) < 17) {
  #   print(paste0(year," Lidar 2016"))
  #   results_LIDAR <- EMS_getLIDAR_2016_stats(currentFile)
  # } else if (as.numeric(year) < 18) {
  #   print(paste0(year," Lidar 2017"))
  #   results_LIDAR <- EMS_getLIDAR_2017_stats(currentFile)
  # } else if (as.numeric(year) < 19) {
  #   print(paste0(year," Lidar 2018"))
  #   results_LIDAR <- EMS_getLIDAR_2018_stats(currentFile)
  # } else if (as.numeric(year) < 20) {
  #   print(paste0(year," Lidar 2019"))
  #   results_LIDAR <- EMS_getLIDAR_2019_stats(currentFile)
  # } else {
  #   print("ERROR")
  #   print(year)
  #   results_LIDAR <- EMS_getLIDAR_2014_stats(currentFile)
  # }

  
  # THIRD FUNCTION FOR TCT FILES
  #if statement to decide which TCT file to get
  
  print("--------------------------------------TCT")
  results_TCT <- EMS_getTCTStats(currentFile)

  #MERGE RESULTS
  #get results all together. This is a not-so-elegant way of doing this, but it does work (for now)
  results <- merge(merge(merge(results_MGP_C,results_MGP_D,by=c("year","decDay")),results_LIDAR,by=c("year","decDay")),results_TCT,by=c("year","decDay"))

  #add new results to the master results file
  
  #just mgp
  #results <- merge(results_MGP_C,results_MGP_D,by=c("year","decDay"))
  
  
  print(paste0(currentFile,":----------------------------------------------------------------------------DONE"))
  masterResults_all <- rbind(masterResults_all,results)
  
  doneFile <- (currentFile)
  doneFiles <- rbind(doneFiles,doneFile)

}



write.csv(masterResults_all,"/Users/benjaminglass/Desktop/HF21/00_Datasets/DUMMY_all_results_0730_weightedmean.csv", row.names = FALSE)

# ======= SCRIPT IN PSEUDOCODE ============

#loop over the first ffp line file
    # feed the ffp file into megaplot function with specified megaplot (returns stats about that megaplot)
    # feed polygon into different megaplot function returning stats
    # feed polygon into all other fucntions

    #you'll then have a ton of dataframes with (year,decDay,mean,std)
    #from this merge all the stats into a single dataframe with (year, DecDay,mean_megaplot1,std_megaplot1,etc.)
    #add this to the master dataframe created before the for loop






