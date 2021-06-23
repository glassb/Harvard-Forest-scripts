# Benjamin Glass
# Last Edits: June 23, 2021

# Script overview: the goal of this script is to allow our raster data to interact with
#     our flux footprint prediction (FFP) model data.

##### ------------------------------- 00 FRONT MATTER -------------------------------------
library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(rgeos)

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/misc")


##### ------------------------------- 01: FUNCTION getFootprint -------------------------------------
# function that puts in decimal day, r, and file and gets out a single vector ring
getFootprint <- function(file,dayVar,rVar) {
  
  #reads in file
  FFP_output <- read.csv(file,stringsAsFactors=FALSE)
  
  #filters by dec.day and r
  FFP_filtered <- FFP_output %>%
    filter(r==rVar,
           dec.day == dayVar)
  
  #creates a spatial layer
  FFP <- SpatialPointsDataFrame(FFP_filtered[,6:7],
                                FFP_filtered,
                                proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  print(FFP)
  plot(FFP,pch=20,col="red")
  
  return(FFP)
  
}

##### ------------------------------- TESTS AND OTHER MISC -------------------------------------
#calling the function with a specific file and other params
FFP_test <- getFootprint("FFP_test.csv",363,.75)

# CREATES POLYGON AND MASKS RASTER TO IT
fprint <- read.csv("FFP_test.csv",stringsAsFactors=FALSE)

fprint <- fprint %>%
  filter(r==.75,
         dec.day == 363)

coords = cbind(fprint[6],fprint[7])
print(coords)

p = Polygon(coords)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
print(sps)


##### ------------------------------- 02: FUNCTION nat/inf composites  -------------------------------------

# Benjamin Glass
# June 22, 2021


# Function overview: this function loads in Landsat8 image files from our GEE editor. 
#     It takes in raster image collections of all the bands in an IC and offers
#     some functions in order to manipulate that function. 


setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/Landsat8_outputs")

#turn off factors
options(stringsAsFactors = FALSE)

#list files
list.files("/Users/benjaminglass/Desktop/HF21/00_Datasets/Landsat8_outputs")

#function that takes in a .tif Landsat 8 file and outputs the natural composite of it
natComp <- function(file) {
  
  #get file
  bandStack <- stack(file)
  
  #Scale to 0-255 range for display device
  bandStack_stretched <- stretch(x=bandStack, minv=0, maxv=255)
  
  #returns natural composite of file
  plotRGB(bandStack_stretched,r=4,g=3,b=2)
  
}

infComp <- function(file) {
  #get file
  bandStack <- stack(file)
  #Scale to 0-255 range for display device
  bandStack_stretched <- stretch(x=bandStack, minv=0, maxv=255)
  #returns natural composite of file
  plotRGB(bandStack_stretched,r=5,g=4,b=3)
}

#specify the file name
file = "2017-05-20-140.tif"

#specify the function calls
#natComp(file)
#infComp(file)


##### ------------------------------- TESTS AND OTHER MISC -------------------------------------

masked <- mask(stack(file),sps)
natComp(masked)

#FINAL PRODUCT
FFP_masked <- infComp(crop(masked,sps))



##### ------------------------------- 03: FUNCTION getRasterMask * -------------------------------------

# inputs: raster tif, FFP_output file, dayVar, and rVar
getRasterMask <- function(FFP_file,dayVar,rVar,rasterImage) {
  
  # go to FFP outputs file (EMS tower FFP output files)
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_outputs")
  
  #(getFootprint(FFP_file,dayVar,rVar))
  
  #reads in EMS FFP file
  FFP_output <- read.csv(FFP_file,stringsAsFactors=FALSE)
  print("FFP output file READ IN.")
  
  #filters by dec.day and r
  FFP_filtered <- FFP_output %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  print("FFP output FILTERED.")
  
  #define coords as coordinates of all the vector points in FFP filtered
  coords = cbind(FFP_filtered[5],FFP_filtered[6])
  
  #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
  p = Polygon(coords)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  print("Spatial Polygon CREATED.")
  
  #set working directory to where Landsat8 outputs are
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/Landsat8_outputs")
  
  #create raster stack from landsat8 imagery
  bandStack <- stack(rasterImage)
  print("Raster Image READ IN.")
  
  #mask landsat imagery by FFP spatial polygon
  masked <- mask(stack(bandStack),sps)
  print(masked)
  FFP_masked <- natComp(crop(masked,sps))
  #plot(FFP_masked)
  return(crop(masked,sps))
  
} 

##### ------------------------------- TESTS -------------------------------------

read <- getRasterMask("FFP.hr.lines.96.11.csv",306.5417,.75,"2017-05-20-140.tif")

#the resulting output is a raster that we can now do raster calculations on

##### ------------------------------- 04: FUNCTION getRasterMask (with megaplot.tif) -----------------------------

#gets pixels of Megaplot tif file based on FFP output parameters
getRasterMaskMEGAPLOT <- function(FFP_file,dayVar,rVar) {
  
  # go to FFP outputs file (EMS tower FFP output files)
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/FFP_data/EMS_FFP_outputs")
  
  #(getFootprint(FFP_file,dayVar,rVar))
  
  #reads in EMS FFP file
  FFP_output <- read.csv(FFP_file,stringsAsFactors=FALSE)
  print("FFP output file READ IN.")
  
  #filters by dec.day and r
  FFP_filtered <- FFP_output %>%
    filter(r==rVar,
           dec.day == dayVar) %>%
    mutate(easting = xr+732275,
           northing=yr+4713368)
  print("FFP output FILTERED.")
  
  #define coords as coordinates of all the vector points in FFP filtered
  coords = cbind(FFP_filtered[5],FFP_filtered[6])
  
  #create spatial polygon with all FFP filtered vector points (vector points -> polygon)
  p = Polygon(coords)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps),proj4string = CRS("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  print("Spatial Polygon CREATED.")
  
  #set working directory to where Landsat8 outputs are
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/GIS")
  
  #create raster stack from landsat8 imagery
  megaplotDP <- raster("DecidiousProportionMegaPlot.tif")
  
  print("Raster Image READ IN.")
  
  #mask landsat imagery by FFP spatial polygon
  masked <- mask(megaplotDP,sps)
  
  par(mfrow=c(1,2))
  plot(megaplotDP,axes=FALSE)
  plot(masked,axes=FALSE)
  #FFP_masked <- crop(masked,sps)
  #plot(FFP_masked)
  return(crop(masked,sps))
  
} 

##### ------------------------------- TESTS ----

read <- getRasterMaskMEGAPLOT("FFP.hr.lines.96.11.csv",307,.75)
