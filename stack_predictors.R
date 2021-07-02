#rm(list=ls() )
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet")

library(sp)
library(sf) 
library(mapview)
library(raster)
library(rgdal)

urbancz <- raster("FE_UCZ/ucz_ms_100m.tif")
#dlm <-raster("dlm/dlm_raster_100m.tif")
dlm <-raster("dlm/dlm_raster.tif")
cop <- raster("Copernicus/copernicus_tree_cover_MS_100m.tif")

extent(dlm) <- extent(cop)
extent(urbancz) <- extent(cop)

dlm <- resample(dlm,cop, method="ngb")
urbancz <- resample(urbancz,cop, method="ngb")
pred <- stack(cop,dlm_test,urbancz)
mapview(pred)
unique(dlm)

writeRaster(pred, "pred_stack", overwrite = T)
modis_test <- raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020157_22_23_ .tif")

pred_resample <- resample(pred,modis_test)
test <- stack(modis_test,pred_resample)
