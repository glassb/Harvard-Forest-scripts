#this is a workflow test

library(tidyverse)
setwd("/Users/benjaminglass/HF21-Scripts")
source("00_projectFunctions.R")

#choose a single month of footprints
setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_outputs")

FFPDec2010 <- read.csv("FFP.hr.lines.10.12.csv",stringsAsFactors=FALSE)
results <- data.frame(decDay=NA,mean=NA,std=NA)
#for each unique day in the FFP data file
for (decDay in unique(FFPDec2010$dec.day)) { 
  print(decDay)
  
  #create a variable that has just the decDay that is being looped over right now
  FFP <- FFPDec2010[FFPDec2010$dec.day == decDay, ]
  
  #get stats for the DecProp.tif
  stats <- RasterClipbyFFP(FFP,decDay,.75,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
  
  #print(stats[2])
  new_row <- c(decDay,stats[1],stats[2])
  results <- rbind(results,new_row)
  #print(results)
  
  
  # FFP_prime <- FFP %>%
  #   mutate(mean = stats[1],
  #          std = stats[2])
    
}



# for (decDay in unique(FFP_prime$dec.day)) { 
#   
#   #create a variable that has just the decDay that is being looped over right now
#   FFP_ <- FFP_prime[FFP_prime$dec.day == decDay, ]
#   
#   print(decDay)
#   
# }
#find out each unique decimal day

#run these footprints through the neccesary functions (create spatial footprint, extract specified raster data, summarized clipped raster data)
#put the summary statistics back in a format that is usable
