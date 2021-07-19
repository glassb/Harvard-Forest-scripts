library(tidyverse)

NEON_file <- ""
EMS_file <- ""



#import EMS data
setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
resultsEMS <- as.data.frame(read.csv(EMS_file))

#mutate resultsEMS
resultsEMS <- resultsEMS %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

#read in hfm
setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hfmaster_0713.RDS")

#merge EMS data and hfm
EMS_results_merged <- merge(hfm,resultsEMS,by=c("Year.Year","time_days"))





#import NEON data
setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
resultsNEON <- as.data.frame(read.csv(NEON_file))

#mutate resultsNEON
resultsNEON <- resultsNEON %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

#read in ameriflux dataset
setwd("/Users/benjaminglass/Downloads")
amf <- as.data.frame(read.csv(file="AMF_US-xHA_BASE_HH_4-5.csv",header=TRUE,skip=2,stringsAsFactors=FALSE))

#merge NEON data and amf
NEON_results_merged <- merge(amf,resultsNEON,by=c("Year.Year","time_days"))


#merge NEONresults and EMSresults based on same time stamps, and include the CO2 flux data points for hfm and amf datasets
results_merged <- ()


#create empty column
results_merged <- results_merged %>%
  add_column(SpatOverlapPC = NA)


setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables")
dummyRaster <- raster("dummyRaster.tif")


for (row in results_merged) {
  
  #=========== get spatial object for EMS
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  year <- row$Year.Year
  month <- row$month.Month
  
  #make sure month is correct
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  
  EMS_file <- read.csv(paste0("FFP.hr.lines.",year,".",month,".csv"),stringsAsFactors=FALSE)
  
  EMS_filtered <- EMS_file %>%
    filter(r==.5,
           dec.day == row$time_days) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  
  #create EMS raster
  EMS_raster <- raster()
  extent(EMS_raster) <- extent(dummyRaster)
  crs(EMS_raster) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  res(EMS_raster) <- res(dummyRaster)
  
  EMS_raster <-rasterize(x=EMS_filtered[, c('easting','northing')],
                         y=EMS_raster,
                         #field=1,
                         field=fwm_file_filter[, 'r'],
                         fun=mean)
  
  EMS_total_cells <- ncell(EMS_raster)
  
  
  
  
  #========== get spatial object for NEON
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
  year <- row$Year.Year
  month <- row$month.Month
  
  #make sure month is correct
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  
  NEON_file <- read.csv(paste0("FFP.neon.hr.lines.",year,".",month,".csv"),stringsAsFactors=FALSE)
  
  NEON_filtered <- NEON_file %>%
    filter(r==.5,
           dec.day == row$time_days) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  
  #create EMS raster
  NEON_raster <- raster()
  extent(NEON_raster) <- extent(dummyRaster)
  crs(NEON_raster) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  res(NEON_raster) <- res(dummyRaster)
  
  NEON_raster <-rasterize(x=NEON_filtered[, c('easting','northing')],
                         y=NEON_raster,
                         #field=1,
                         field=fwm_file_filter[, 'r'],
                         fun=mean)
  
  NEON_total_cells <- ncell(NEON_raster)
  
  #========== CROP ONE FROM THE OTHER
  rasterIntersect <- crop(EMS_raster,NEON_raster)
  intersect_total_cells = ncell(rasterIntersect)
  
  #calculate their percent overlap
  percentOverlap <- intersect_total_cells/(EMS_total_cells+NEON_total_cells)
  
  results_merged <- replace(results_merged$SpatOverlapPC,
                            row_number(results_merged)==row_number(row),
                            percentOverlap)
  
}




