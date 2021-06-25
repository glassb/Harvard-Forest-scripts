
library(raster)
setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/00 spatial deliverables")


conProp <- raster("ConiferProportionMegaplot.tif")
decidProp <- raster("DecidiousProportionMegaPlot.tif")
soilDrainage <- raster("SoilDrainageRaster(0-ND).tif")
pht <- raster("ProspectHillTract_test.tif")

plot(conProp)
plot(decidProp)
plot(soilDrainage)
plot(pht)

soilDrainage
pht
