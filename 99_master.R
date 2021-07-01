

library(tidyverse)

#read in all the fpp lines
#create dataframe 
setwd("/Users/benjaminglass/HF21-Scripts")
source("10_getMGP_stats.R")
source("12_getLIDAR_stats.R")
source("13_getTCT_stats.R")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")

temp = list.files(pattern="*.csv")

#create a master results file
  #masterResults <- df()

for (currentFile in temp) {
  print(currentFile)
  year <- substr(currentFile,14,15)
  
  # FIRST SET OF FUNCTIONS FOR MEGAPLOT
  #results_MGP_C <- getMGP_C_stats(currentFile)
  #results_MGP_D <- getMGP_D_stats(currentFile)
  
  
  # SECOND SET OF FUNCTIONS FOR LIDAR
  #if statement that decides which lidar year to get
  # if (as.numeric(year)>= 90 | as.numeric(year) <= 14 ) {
  #   print(paste0(year," Lidar 2014"))
  #   results_LIDAR <- getLIDAR_2014_stats(currentFile)
  # } else if (as.numeric(year) <= 16) {
  #   print(paste0(year,"Lidar 2016"))
  #   results_LIDAR <- getLIDAR_2016_stats(currentFile)
  # } else if (as.numeric(year) <= 17) {
  #   print(paste0(year,"Lidar 2017"))
  #   results_LIDAR <- getLIDAR_2017_stats(currentFile)
  # } else if (as.numeric(year) <= 18) {
  #   print(paste0(year,"Lidar 2018"))
  #   results_LIDAR <- getLIDAR_2018_stats(currentFile)
  # } else if (as.numeric(year) <= 19) {
  #   print(paste0(year,"Lidar 2019"))
  #   results_LIDAR <- getLIDAR_2019_stats(currentFile)
  # } else {
  #   print("ERROR: CURRENT FILE YEAR ",year,"HAS NOT BEEN FED THROUGH FUNCTION")
  # }
  # 
  
  
  # THIRD FUNCTION FOR TCT FILES
  #if statement to decide which TCT file to get
  results_TCT <- getTCTStats(currentFile)
  
  
  #MERGE RESULTS
  #get results all together
  #results <- merge(merge(merge(results_MGP_C,results_MGP_D,by=c("year","decDay")),results_LIDAR,by=c("year","decDay")),results_TCT,by=c("year","decDay"))
  
  #add new results to the master results file


  # results_LIDAR_2016 <- getLIDAR_2016_stats(currentFile)
  # results_LIDAR_2017 <- getLIDAR_2017_stats(currentFile)
  # results_LIDAR_2018 <- getLIDAR_2018_stats(currentFile)
  # results_LIDAR_2019 <- getLIDAR_2019_stats(currentFile)


}


#myfiles = lapply(temp, read.delim)





# results_MGP_C <- getMGP_C_stats("FFP.hr.lines.10.12.csv")
# results_MGP_D <- getMGP_D_stats("FFP.hr.lines.10.12.csv")
# 
# results_LIDAR_2014 <- getLIDAR_2014_stats("FFP.hr.lines.10.12.csv")
# results_LIDAR_2016 <- getLIDAR_2016_stats("FFP.hr.lines.10.12.csv")
# results_LIDAR_2017 <- getLIDAR_2017_stats("FFP.hr.lines.10.12.csv")
# results_LIDAR_2018 <- getLIDAR_2018_stats("FFP.hr.lines.10.12.csv")
# results_LIDAR_2019 <- getLIDAR_2019_stats("FFP.hr.lines.10.12.csv")



#loop over the first ffp line file
    # feed the ffp file into megaplot function with specified megaplot (returns stats about that megaplot)
    # feed polygon into different megaplot function returning stats
    # feed polygon into all other fucntions

    #you'll then have a shit ton of dataframes with (year,decDay,mean,std)
    #from this merge all the stats into a single dataframe with (year, DecDay,mean_megaplot1,std_megaplot1,etc.)
    #add this to the master dataframe created before the for loop


#master <- df()
# currentFile <- "FFP.hr.lines.03.8.csv"
# year <- substr(currentFile,14,15)




# results_MGP_C <- getMGP_C_stats(currentFile)
# results_MGP_D <- getMGP_D_stats(currentFile)
# results_LIDAR_2014 <- getLIDAR_2014_stats(currentFile)
# results_TCT <- getTCTStats(currentFile)





