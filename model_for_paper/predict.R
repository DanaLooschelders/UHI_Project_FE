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
#parallel
library(parallel)
#install.packages("doParallel")
library(doParallel)

no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores) 
registerDoParallel(cl)

#load model
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modell")
load(file = "result_20220713.RData")

#RMSE
plot(model)

#Variable importance
varImp(model)
#as plot
plot(varImp(model))

#load meteo data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Meteorologie")
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Meteorologie")
meteo<-read.csv("meteo_for_raster.csv")
str(meteo)
#predict
sample<-sample(1:nrow(meteo),10)
sample<-c(163, 317, 697, 438, 431, 263, 329, 609,  36,  17)
#random sample is 163 317 697 438 431 263 329 609  36  17

#load static pred stack
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
pred_stack_06<-stack("all_static_pred_06.grd")
names(pred_stack_06)[1:2]<-c("albedo","ndvi") #rename to match model
pred_stack_07<-stack("all_static_pred_07.grd")
names(pred_stack_07)[1:2]<-c("albedo","ndvi") #rename to match model


#load dynamic meteo preds (5 randomly sampled from all meteo files)
setwd("E:/meteo_raster/")
meteo_163<-stack("Meteo__163.grd")
meteo_609<-stack("Meteo__609.grd")
meteo_36<-stack("Meteo__36.grd")

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
time_163<-stack("time__163.grd")
time_609<-stack("time__609.grd")
time_36<-stack("time__36.grd")

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
pred_stack_all_163 <- stack(pred_stack_06, lidar_crs, meteo_163, time_163)
pred_stack_all_609<- stack(pred_stack_07, lidar_crs, meteo_609, time_609)
pred_stack_all_36<- stack(pred_stack_06, lidar_crs, meteo_36, time_36)
#mapview(pred_stack_all_547, maxpixels =  5073950)
#plot(pred_stack_all_547)

names(pred_stack_all_163)
plot(pred_stack_all_163)
plot(pred_stack_all_609)

#save as raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modell/pred_stacks/")
writeRaster(pred_stack_all_163,filename= "pred_stack_all_163.tif", bylayer=F,format="raster",overwrite=T)
writeRaster(pred_stack_all_609,filename= "pred_stack_all_609.tif", bylayer=F,format="raster",overwrite=T)
writeRaster(pred_stack_all_36,filename= "pred_stack_all_36.tif", bylayer=F,format="raster",overwrite=T)

#load raster
pred_stack_all_163<-stack("pred_stack_all_163.grd")
pred_stack_all_609<-stack("pred_stack_all_36.grd")
pred_stack_all_36<-stack("pred_stack_all_609.grd")

range_pred<-data.frame(rep(NA, times=nlayers(pred_stack_all_609)))
range_pred$highest<-NA
for(i in 1:nlayers(pred_stack_all_609)){
  range_pred[i,]<-range(values(pred_stack_all_609[[i]]), na.rm=T)
}
range_pred$names<-names(pred_stack_all_609)
#extract a smaller area
plot(pred_stack_all_36$albedo)
extent(pred_stack_all_36)
coords<-c(7.473961, 7.484256 , 51.8402 , 51.85021)
pred_stack_small<-crop(pred_stack_all_36, coords)
pred_stack_small_2<-crop(pred_stack_all_609, coords)
mapview(pred_stack_small)
aoa_test<-aoa(pred_stack_small, model)
aoa_test_2<-aoa(pred_stack_small_2, model)

plot(aoa_test_2)

plot(pred_stack_small[[1:10]])
plot(pred_stack_small[[11:22]])

plot(model$trainingData)
for(i in 1:length(model$trainingData)){
  print(range(model$trainingData[,i]))
}

range_td<-lapply(model$trainingData, range)
range_df<-data.frame( "range"=range_td)
test<-do.call(rbind.data.frame, range_td)
test$names<-names(range_td)
names(test)<-c("lowest", "highest", "name")

#predict
model_163_predict<-predict(pred_stack_all_163, model, savePrediction=TRUE)
model_609_predict<-predict(pred_stack_all_609, model, savePrediction=TRUE)
model_36_predict<-predict(pred_stack_all_36, model, savePrediction=TRUE)

#view
mapview(model_163_predict, maxpixels =  5073950)
plot(model_609_predict)
mapview(model_609_predict)

plot(model_36_predict)
mapview(model_36_predict)

#check which time was predicted
all_temp[163,] #2020-06-16 12:00:00
all_temp[609,] #2020-07-23 04:00:00 
all_temp[36,] #2020-06-11 05:00:00 

#calculate AOA
model_163_aoa<-aoa(pred_stack_all_163, model, cl=cl)
mapview(model_163_aoa$DI)

#aoa only NAs
summary(model$trainingData)
summary(model_163_aoa$AOA)
summary(model_163_aoa$DI)
barplot(values(model_163_aoa$DI))

plot(model_163_aoa$AOA)
which.max(values(model_163_aoa$DI))

summary(values(model_163_aoa$DI))
sum(!is.na(values(model_163_aoa$DI)))
