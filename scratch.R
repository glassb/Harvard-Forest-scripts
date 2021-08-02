# DEVELOPED BY TIM WHITBY
# EDITED BY BEN GLASS

#finds the landsat images with more than 80% coverage (based on cloud filtered images)

library(sp)
library(raster)
library(tidyverse)
library(NLP)
# library(filenamer)

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
temp = list.files(pattern="*.tif")

for(TCTimage in temp){
    setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
    print(TCTimage)
    #print(TCTimage[1])
    #print(filenamer::as.filename(TCTimage))
    #file <- as.String(TCTimage)
    #print(file)
    #print(typeof(file))
    #print(typeof(as.String(TCTimage)))
    
    green <- subset(stack(TCTimage),2)
    ncell(green)
      # plot(green)
    pct.data <- sum(is.finite(values(green)))/ncell(green) * 100
    print(pct.data)
      if (pct.data > 80) {
        #setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802")
        tempdir <- "/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs_0802"
        writeRaster(stack(TCTimage), filename=paste0(tempdir,"/",TCTimage),format="GTiff", overwrite=TRUE)
      }
}

  

# # example
# p <- mutate(p,
#   year = as.numeric(year),
#   date = substr(files, 38, 45),
#   month = as.numeric(substr(date, 5, 6)),
#   eco.yr = ifelse(month < 4, as.numeric(year)-1, year))
