library(tidyverse)
library(raster)

NEON_file <- ""
EMS_file <- ""



# #import EMS data
# setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
# resultsEMS <- as.data.frame(read.csv(EMS_file))

# #mutate resultsEMS
# resultsEMS <- resultsEMS %>%
#   mutate(Year.Year = paste0("20",year),
#          time_days = decDay)

#read in hfm
setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hfmaster_0713.RDS")

# #merge EMS data and hfm
# EMS_results_merged <- merge(hfm,resultsEMS,by=c("Year.Year","time_days"))





# #import NEON data
# setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
# resultsNEON <- as.data.frame(read.csv(NEON_file))
# 
# #mutate resultsNEON
# resultsNEON <- resultsNEON %>%
#   mutate(Year.Year = paste0("20",year),
#          time_days = decDay)

#read in ameriflux dataset
setwd("/Users/benjaminglass/Downloads")
NEONmaster <- as.data.frame(read.csv(file="neon_hr.csv",header=TRUE,stringsAsFactors=FALSE))

#mutate resultsNEON

NEONmaster_mutated <- NEONmaster %>%
  mutate(Year.Year = year,
         month.Month = mon,
         time_days = dec.day)

# #merge NEON data and amf
# NEON_results_merged <- merge(NEONmaster,resultsNEON,by=c("Year.Year","time_days"))



#merge NEONresults and EMSresults based on same time stamps, and include the CO2 flux data points for hfm and amf datasets
results_merged <- merge(NEONmaster_mutated,hfm,by=c("Year.Year","month.Month","time_days"))

#results_merged

results_mutated <- results_merged %>%
  dplyr::select(Year.Year,month.Month,time_days,obs.FCO2.e.6mol.m2.s,PAR.28m.e.6mol.m2.s,FC)

#summary(results_mutated)


#create empty column
results_mutated <- results_mutated %>%
  add_column(SpatOverlapPC = NA,
             Poverlap_EMS = NA,
             Poverlap_NEON = NA) %>%
  mutate(Flux_diff = obs.FCO2.e.6mol.m2.s-FC)


setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables")
dummyRaster <- raster("dummyRaster.tif")




for(i in 1:nrow(results_mutated)) {
  row <- results_mutated[i,]
  print(paste0("row: ",i," Year: ",row$Year.Year," Month: ",row$month.Month," decDay: ",row$time_days))

  #=========== get spatial object for EMS
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_lines_17-20")
  
  year <- substr(row$Year.Year,3,4)
  #print(year)
  month <- row$month.Month
  #print(month)
  
  if (year == "17" & month == "1") {
    percentOverlap <- NA
    Poverlap_EMS <- NA
    Poverlap_NEON <- NA
  # } else if (year == "18" & as.numeric(month) <= 9) {
  #   percentOverlap <- NA
  
  } else if (year == "20" & as.numeric(month) >= 11) {
    percentOverlap <- NA
    Poverlap_EMS <- NA
    Poverlap_NEON <- NA
  } else if (year == "20" & month == "4") {
    percentOverlap <- NA
    Poverlap_EMS <- NA
    Poverlap_NEON <- NA
  } else if (year == "20" & month == "5") {
    percentOverlap <- NA
    Poverlap_EMS <- NA
    Poverlap_NEON <- NA
  } else {
  
  #make sure month is correct
  if (substr(month,2,2) == ".") {
    month <- substr(month,1,1)
  } else {
    month <- month
  }
  
  EMS_file <- read.csv(paste0("FFP.hr.lines.",year,".",month,".csv"),stringsAsFactors=FALSE)
  # 
  EMS_filtered <- EMS_file %>%
    filter(r==.75,
           dec.day == row$time_days) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  
  if (nrow(EMS_filtered)==0) {
    
    #if the df is empty, that means there is no coordinate data to create a polygon from, and thus we just return Na,Na to the parent script
    print(paste0("======== ERROR: NO DATA IN FFP FILE. EMS"))
    
    percentOverlap <- NA
    Poverlap_EMS <- NA
    Poverlap_NEON <- NA
  
  } else {
  

    #create EMS raster
    EMS_raster <- raster()
    extent(EMS_raster) <- extent(dummyRaster)
    crs(EMS_raster) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    res(EMS_raster) <- res(dummyRaster)
    # 
    EMS_raster <-rasterize(x=EMS_filtered[, c('easting','northing')],
                           y=EMS_raster,
                           #field=1,
                           field=1,
                           fun=mean)
    
    complete_coords <- EMS_filtered[complete.cases(EMS_filtered[, 5:6]), ]
    #print(complete_coords)
    coords = cbind(complete_coords[5],complete_coords[6])
    #print(coords)
    
    #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
    p = Polygon(coords)
    ps = Polygons(list(p),1)
    sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  
    EMS_total_cells <- ncell(crop(EMS_raster,sps))
    
    
    #========== get spatial object for NEON
    
    #NEON_FFP_lines_17-20
    setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/NEON_FFP_data/NEON_FFP_lines_17-20")
    # 
    NEON_file <- read.csv(paste0("FFP.neon.hr.lines.",year,".",month,".csv"),stringsAsFactors=FALSE)
    # 
    NEON_filtered <- NEON_file %>%
      filter(r==.75,
             dec.day == row$time_days) %>%
      mutate(easting = xr+732180,
             northing=yr+4713265)
    
    
    
      if (nrow(NEON_filtered)==0) {
      
          #if the df is empty, that means there is no coordinate data to create a polygon from, and thus we just return Na,Na to the parent script
          print(paste0("======== ERROR: NO DATA IN FFP FILE. NEON"))
          percentOverlap <- NA
          Poverlap_EMS <- NA
          Poverlap_NEON <- NA
      
      } else {
  # 
          #create NEON raster
          NEON_raster <- raster()
          extent(NEON_raster) <- extent(dummyRaster)
          crs(NEON_raster) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
          res(NEON_raster) <- res(dummyRaster)
        
          NEON_raster <-rasterize(x=NEON_filtered[, c('easting','northing')],
                                 y=NEON_raster,
                                 #field=1,
                                 field=1,
                                 fun=mean)
          
          complete_coords <- NEON_filtered[complete.cases(NEON_filtered[, 5:6]), ]
          coords = cbind(complete_coords[5],complete_coords[6])
          #print(coords)
          
          #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
          p = Polygon(coords)
          ps = Polygons(list(p),1)
          neon_sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
          
          #print(NEON_raster)
          # 
          NEON_total_cells <- ncell(crop(NEON_raster,neon_sps))
          # 
          #========== CROP ONE FROM THE OTHER
          
          
          #print("hello")
          rasterIntersect <- try(intersect(crop(EMS_raster,sps),
                                       crop(NEON_raster,neon_sps)),silent=TRUE)
          
          #print(class(rasterIntersect))
          
          if(class(rasterIntersect) == "try-error") {
              print("======== NO OVERLAP")
            percentOverlap <- 0
            Poverlap_EMS <- 0
            Poverlap_NEON <- 0
              
          } else {
            
              #print(rasterIntersect)
              intersect_total_cells <- ncell(rasterIntersect)
              
              #print(intersect(crop(EMS_raster,sps),
                              #crop(NEON_raster,neon_sps)))
              #print(rasterIntersect)
              #intersect_total_cells = ncell(rasterIntersect)
              # 
              #calculate their percent overlap
              print("=")
              print("=")
              print("=")
              #print(paste0("******** ",EMS_total_cells," ",NEON_total_cells," ",intersect_total_cells))
              percentOverlap <- 2*intersect_total_cells/(EMS_total_cells+NEON_total_cells)
              Poverlap_EMS <- intersect_total_cells/EMS_total_cells
              Poverlap_NEON <- intersect_total_cells/NEON_total_cells
              print(paste0("**************************************** percent overlap: ",percentOverlap))
              print(Poverlap_EMS)
              print(Poverlap_NEON)
              print("=")
          }
          
          # par(mfrow=c(2,2))
          # # 
          # plot(crop(EMS_raster,sps))
          # plot(crop(NEON_raster,neon_sps))
    
        }
    }
  }
  #print("hello")
  #print(results_mutated$SpatOverlapPC[i])
  results_mutated$SpatOverlapPC[i] <- percentOverlap
  results_mutated$Poverlap_EMS[i] <- Poverlap_EMS
  results_mutated$Poverlap_NEON[i] <- Poverlap_NEON
  
  results_mutated <- results_mutated %>%
    dplyr::select(1:10)
  #print("test")
  
  results_print <- results_mutated %>%
    dplyr::select(7:10)
  print(summary(results_print))
  
  
}







# # summary(results_mutated)
# #
# results_prime <- results_mutated %>%
#   dplyr::select(1:8)
#   filter(SpatOverlapPC>0) %>%
#   #mutate(Flux_diff = obs.FCO2.e.6mol.m2.s-FC)
# 
# summary(results_mutated)
# 
# 
# ggplot(data = results_mutated, aes(x=SpatOverlapPC,y=Flux_diff)) +
#   #facet_wrap(~ month.Month,ncol=4) +
#   geom_point(shape=20,cex=1,alpha=.5) +
#   scale_x_continuous() +
#   scale_y_continuous(limits=c(-10,10)) +
#   theme_classic()


write.csv(results_mutated,"/Users/benjaminglass/Desktop/HF21/00_Datasets/overlapScript_0722_75influence.csv", row.names = FALSE)


# summary(results_mutated$SpatOverlapPC)
# 
# ggplot(data=results_mutated) +
#   geom_point(aes=(x=SpatOverlapPC,y=Flux_diff))

