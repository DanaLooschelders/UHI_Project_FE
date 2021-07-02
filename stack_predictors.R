#rm(list=ls() )
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/")

library(sp)
library(sf) 
library(mapview)
library(raster)
library(rgdal)
library(tmap)
library(tmaptools)
library(stars)
library(CAST)
library(caret)
library(randomForest)
library(latticeExtra)
#load training data
training_dat<-spatial_list[grep(names(spatial_list),pattern="MOD11A1_A2020189_12_47")]
training_dat<-training_dat[[1]]

#load predictor variables
urbancz <- raster("FE_UCZ/ucz_ms_100m.tif")
#dlm <-raster("dlm/dlm_raster_100m.tif")
dlm <-raster("dlm/dlm_raster.tif")
cop <- raster("Copernicus/copernicus_tree_cover_MS_100m.tif")

extent(dlm) <- extent(cop)
extent(urbancz) <- extent(cop)

dlm <- resample(dlm,cop, method="ngb")
urbancz <- resample(urbancz,cop, method="ngb")
pred <- stack(cop,dlm,urbancz)
mapview(pred)
unique(dlm)
writeRaster(pred, "pred_stack", overwrite = T)

#test with one modis file
modis_test <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
values(modis_test)
pred_resample <- resample(pred,modis_test)

pred_stack <- stack(modis_test,pred_resample)
mapview(test)

#extract predictor values for trainin gdata
extr <- extract(pred_stack,training_dat,df=TRUE)
#create ID by row names
training_dat$ID<-row.names(training_dat)
#merge
extr <- merge(extr,training_dat@data,by.x="ID")
training_dat<-extr
saveRDS(extr,file="C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/Training_dat.RDS")
#sicherstellen, dass keine NAs in Daten sind
training_dat <- training_dat[complete.cases(training_dat$Temp),]


#Training_dat = alle Daten
#traintemps = für die Validierung


#training_dat <- training_dat[training_part,]
