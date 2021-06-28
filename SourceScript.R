
setwd("/Users/benjaminglass/HF21-Scripts")

source("RasterProducts.R")
source("FFP-Raster_interactions.R")
source("getStatsf.R")



RasterClipbyFFP("FFP.hr.lines.96.11.csv",307,.75,"Lidar_CHM_2019.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/LIDAR_CHM_rasters")
RasterClipbyFFP("FFP.hr.lines.96.11.csv",307,.75,"ConiferProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
RasterClipbyFFP("FFP.hr.lines.96.11.csv",307,.75,"DecidiousProportionMegaPlot.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/Megaplot_rasters")
RasterClipbyFFP("FFP.hr.lines.96.11.csv",307,.75,"NLCD_2016_utm_clipped.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/NLCD_rasters")
test <- RasterClipbyFFP("FFP.hr.lines.96.11.csv",307,.75,"SoilDrainage(0-ND)_utm.tif","/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables/SoilDrainage_rasters")



#turns the SoilDrainage 0 values to NA
test[test == 0] <- NA

test

getStats(test)



