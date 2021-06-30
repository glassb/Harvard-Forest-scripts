

#read in all the fpp lines
#create dataframe 
setwd("/Users/benjaminglass/HF21-Scripts")

source("10_getMGP_stats.R")
source("12_getLIDAR_stats.R")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines")

temp = list.files(pattern="*.csv")


# for (currentFile in temp) {
#   print(currentFile)
#   
#   results_MGP_C <- getMGP_C_stats(currentFile)
#   results_MGP_D <- getMGP_D_stats(currentFile)
#   
#   results_LIDAR_2014 <- getLIDAR_2014_stats(currentFile)
#   results_LIDAR_2016 <- getLIDAR_2016_stats(currentFile)
#   results_LIDAR_2017 <- getLIDAR_2017_stats(currentFile)
#   results_LIDAR_2018 <- getLIDAR_2018_stats(currentFile)
#   results_LIDAR_2019 <- getLIDAR_2019_stats(currentFile)
# 
#   
# }


#myfiles = lapply(temp, read.delim)





results_MGP_C <- getMGP_C_stats("FFP.hr.lines.10.12.csv")
results_MGP_D <- getMGP_D_stats("FFP.hr.lines.10.12.csv")

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

