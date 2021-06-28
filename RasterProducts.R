#this file just previews all the raster input data that we are collecting


library(raster)
setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/00-spatial-deliverables")


conProp <- raster("Megaplot_rasters/ConiferProportionMegaplot.tif")
decidProp <- raster("Megaplot_rasters/DecidiousProportionMegaPlot.tif")
soilDrainage <- raster("SoilDrainage_rasters/SoilDrainage(0-ND)_utm.tif")


plot(conProp)
plot(decidProp)
plot(soilDrainage)


