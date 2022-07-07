#model metrics
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

#RMSE
plot(model)

#Variable importance
varImp(model)
#as plot
plot(varImp(model))

#predict
sample<-sample(1:nrow(meteo),5)
#random sample is 170 341  438 438 547

#load static pred stack
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
pred_stack_06<-stack("all_static_pred_06.grd")
names(pred_stack_06)[1:2]<-c("albedo","ndvi") #rename to match model
pred_stack_07<-stack("all_static_pred_07.grd")
names(pred_stack_07)[1:2]<-c("albedo","ndvi") #rename to match model

#load dynamic meteo preds (5 randomly sampled from all meteo files)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prediction/Meteo_for_prediction")
meteo_438<-stack("Meteo__438.grd")

#load time_of_day data and write as raster
times_06 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Time_of_day/times_06.csv")
times_07 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Time_of_day/times_07.csv")
#combine
times<-rbind(times_06, times_07)
#change NA values to 0
times$hours_sss[is.na(times$hours_sss)]<-0
times$hours_ssr[is.na(times$hours_ssr)]<-0
#create Raster
#get shape of polygon
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)
#load refrence raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness")
ref_raster<-raster("copernicus_imperviousness_crop_MS_10m.tif")
#transform polygon into Raster
r <- raster(ncol=ncol(ref_raster), nrow=nrow(ref_raster), crs = "+proj=longlat +datum=WGS84 +no_defs")
extent(r) <- extent(ref_raster)
raster_Steinf<-rasterize(gadm, r)
#write to raster (only samples)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prediction/sunshinehours_for_prediction")

for(i in sample){
  #hours_sss
  raster_Steinf_sss<-raster_Steinf
  values(raster_Steinf_sss)<-times$hours_sss[i]
  #hours_ssr
  raster_Steinf_ssr<-raster_Steinf
  values(raster_Steinf_ssr)<-times$hours_ssr[i]
  #stack values
  time_stack<-stack(raster_Steinf_sss, raster_Steinf_ssr)
  #set layer names
  names(time_stack)<-c("hours_sss", "hours_ssr")
  #write raster into file
  writeRaster(time_stack, filename = paste("time_", i,
                                            sep="_"), overwrite=T)
}

#load time raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prediction/sunshinehours_for_prediction")
time_438<-stack("time__438.grd")


#load lidar data
#stack with lidar data 
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar")
lidar <- "C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar"
list.files(lidar)
lidar <- stack("Lidar_building_height.grd",
               "Lidar_building_sd_3x3.grd",
               "Lidar_building_sd_5x5.grd")

names(lidar) <- c("building_height", "building_height_sd_3x3", "building_height_sd_5x5")

lidar_crs <- projectRaster(lidar,crs = "+proj=longlat +datum=WGS84 +no_defs",
                           method = "ngb" ,r)

#set height of non-existing buildings to 0 
values(lidar_crs$building_height)[is.na(values(lidar_crs$building_height))]<-0
values(lidar_crs$building_height_sd_3x3)[is.na(values(lidar_crs$building_height_sd_3x3))]<-0
values(lidar_crs$building_height_sd_5x5)[is.na(values(lidar_crs$building_height_sd_5x5))]<-0

#stack all
pred_stack_all_438 <- stack(pred_stack_07, lidar_crs, meteo_438, time_438)

#mapview(pred_stack_all_438, maxpixels =  5073950)
#plot(pred_stack_all_438)

names(pred_stack_all_438)


names(pred_stack_07)
plot(pred_stack_07)

names(lidar_crs)
plot(lidar_crs)

names(meteo_438)
plot(meteo_438)

names(time_438)
plot(time_438)

#predict
model_438_predict<-predict(pred_stack_all_438, model, savePrediction=TRUE)

mapview(model_438_predict, maxpixels =  5073950)

any(!is.na(values(model_438_predict)))

total_stack[438,]
