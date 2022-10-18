#model metrics

library(RStoolbox)
library(dplyr)
library(terra)

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
library(beepr)
#parallel
library(parallel)
#install.packages("doParallel")
library(doParallel)

no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores) 
registerDoParallel(cl)

#load model
setwd("C:/Users/Dana/sciebo/ndom/klaus/")
model<-readRDS(file = "ffs_Model_2022-10-13.RDS")

#RMSE
png(file="RMSE_klaus.png", width = 300, height=200, units="mm", res = 200)
plot(model)
dev.off()

#Variable importance
varImp(model)
png(file="VarImp_klaus.png", width = 300, height=200, units="mm", res = 200)
#as plot
plot(varImp(model))
dev.off()

#load meteo data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie")
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Meteorologie")
meteo<-read.csv("meteo_all.csv")
str(meteo)
meteo$datetime<-as.POSIXct(meteo$datetime)
#predict
set.seed(6)
sample<-sample(1:nrow(meteo),4)
sample<-c(283, 387, 149, 392)
#random sample is 283 387 149 392

#load static pred stack
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren")
pred_stack_06<-stack("pred_stack_06_20221012.grd")
names(pred_stack_06)[1:2]<-c("albedo","ndvi") #rename to match model
pred_stack_07<-stack("pred_stack_07_20221012.grd")
names(pred_stack_07)[1:2]<-c("albedo","ndvi") #rename to match model


#write meteo data into raster stack
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Copernicus_grün_blau_grau/Imperviousness")
#load ref raster
ref_raster<-raster("copernicus_imperviousness_crop_MS_10m.tif")
#transform polygon into Raster
r <- raster(ncol=ncol(ref_raster), nrow=nrow(ref_raster), 
            crs = "+proj=longlat +datum=WGS84 +no_defs")
extent(r) <- extent(ref_raster)
raster_Steinf<-rasterize(gadm, r)
#set wd
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction/Stacks_for_prediction/")
#run loop for all samples
for(i in sample){
  #Temperature
  raster_Steinf_temp<-raster_Steinf
  values(raster_Steinf_temp)<-meteo$meteo_Temp[i]
  #relative humidity
  raster_Steinf_RH<-raster_Steinf
  values(raster_Steinf_RH)<-meteo$meteo_rH[i]
  #stability
  raster_Steinf_stability<-raster_Steinf
  values(raster_Steinf_stability)<-meteo$meteo_stability[i]
  #cloudcover
  raster_Steinf_cloudcover<-raster_Steinf
  values(raster_Steinf_cloudcover)<-meteo$meteo_cloudcover[i]
  #radiation
  raster_Steinf_radiation<-raster_Steinf
  values(raster_Steinf_radiation)<-meteo$meteo_radiation[i]
  #cum_radiation
  raster_Steinf_cum_radiation<-raster_Steinf
  values(raster_Steinf_cum_radiation)<-meteo$meteo_cum_radiation[i]
  #precipitation current
  raster_Steinf_precip<-raster_Steinf
  values(raster_Steinf_precip)<-meteo$meteo_precip[i]
  #precipitation last 3h
  raster_Steinf_precip3h<-raster_Steinf
  values(raster_Steinf_precip3h)<-meteo$meteo_precip3hour[i]
  #precipitation last day
  raster_Steinf_precip1day<-raster_Steinf
  values(raster_Steinf_precip1day)<-meteo$meteo_precip1day[i]
  #wind speed
  raster_Steinf_windspeed<-raster_Steinf
  values(raster_Steinf_windspeed)<-meteo$meteo_windspeed[i]
  #wind direction
  raster_Steinf_winddir<-raster_Steinf
  values(raster_Steinf_winddir)<-meteo$meteo_winddirection[i]
  #stack values
  meteo_stack<-stack(raster_Steinf_RH, raster_Steinf_temp, raster_Steinf_stability,
                     raster_Steinf_cloudcover, raster_Steinf_radiation, 
                     raster_Steinf_cum_radiation, raster_Steinf_precip, raster_Steinf_precip3h,
                     raster_Steinf_precip1day,raster_Steinf_windspeed, raster_Steinf_winddir)
  #set layer names
  names(meteo_stack)<-c("meteo_RH", "meteo_Temp", "meteo_stability", 
                        "meteo_cloudcover", "meteo_radiation", "meteo_cum_radiation",
                        "meteo_precip", "meteo_precip3hour", "meteo_precip1day",
                        "meteo_windspeed","meteo_winddirection")
  #write raster into file
  writeRaster(meteo_stack, filename = paste("Meteo_", i,
                                            sep="_"), overwrite=T)
}

#load time_of_day data and write as raster
times_06 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Time_of_day/times_06.csv")
times_07 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Time_of_day/times_07.csv")
#combine
times<-rbind(times_06, times_07)
#add column with datetime
times$date_time <- as.POSIXct(paste(times$day, times$time), format="%Y-%m-%d %H:%M:%S")
range(times$date_time, na.rm=T)
range(meteo$datetime)
#cut times to length of meteo
times<-times[times$date_time>=range(meteo$datetime)[1]&times$date_time<=range(meteo$datetime)[2],]
any(duplicated(times$date_time)==TRUE) #no duplicated values
which(diff(meteo$datetime)>1)
which(diff(times$date_time)>1)
names(meteo)[1]<-"date_time"
#cut times to length of meteo
times_tidy<-left_join(meteo[,c("date_time", "meteo_Temp")], times, by = "date_time")
times_tidy<-times_tidy[,-2]

#change NA values to 0
times$hours_sss[is.na(times$hours_sss)]<-0
times$hours_ssr[is.na(times$hours_ssr)]<-0
#create Raster
#get shape of polygon
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)

#write to raster (only samples)
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction/Stacks_for_prediction/")

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
beep()
#load time raster
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction/Stacks_for_prediction/")
time_149<-stack("time__149.grd")
time_283<-stack("time__283.grd")
time_387<-stack("time__387.grd")

#load meteo raster
#load time raster
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction/Stacks_for_prediction/")
meteo_149<-stack("meteo__149.grd")
meteo_283<-stack("meteo__283.grd")
meteo_387<-stack("meteo__387.grd")

#set height of non-existing buildings to 0 
#for pred_stack_06
values(pred_stack_06$building_height)[is.na(values(pred_stack_06$building_height))]<-0
values(pred_stack_06$building_height_sd_3x3)[is.na(values(pred_stack_06$building_height_sd_3x3))]<-0
values(pred_stack_06$building_height_sd_5x5)[is.na(values(pred_stack_06$building_height_sd_5x5))]<-0
#for pred_stack_07
values(pred_stack_07$building_height)[is.na(values(pred_stack_07$building_height))]<-0
values(pred_stack_07$building_height_sd_3x3)[is.na(values(pred_stack_07$building_height_sd_3x3))]<-0
values(pred_stack_07$building_height_sd_5x5)[is.na(values(pred_stack_07$building_height_sd_5x5))]<-0
#check with month
meteo[149,] #2020-06-11 04:00:00  
meteo[283,] #2020-06-16 18:00:00
meteo[387,] #2020-07-05 01:00:00 
#stack all
pred_stack_all_149 <- stack(pred_stack_06,  meteo_149, time_149)
pred_stack_all_283<- stack(pred_stack_06, meteo_283, time_283)
pred_stack_all_387<- stack(pred_stack_07, meteo_387, time_387)

#check
names(pred_stack_all_149)
png(file="pred_stack_149.png",
    width = 300, height=200, units="mm", res = 200)
plot(pred_stack_all_149[[18:30]])
dev.off()

#save as raster
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction/Stacks_for_prediction/")
writeRaster(pred_stack_all_149,filename= "pred_stack_all_149.tif", bylayer=F,format="raster",overwrite=T)
writeRaster(pred_stack_all_283,filename= "pred_stack_all_283.tif", bylayer=F,format="raster",overwrite=T)
writeRaster(pred_stack_all_387,filename= "pred_stack_all_387.tif", bylayer=F,format="raster",overwrite=T)
beep()
#load raster
pred_stack_all_149<-stack("pred_stack_all_149.grd")
pred_stack_all_609<-stack("pred_stack_all_283.grd")
pred_stack_all_36<-stack("pred_stack_all_387.grd")

#range_pred<-data.frame(rep(NA, times=nlayers(pred_stack_all_609)))
#range_pred$highest<-NA
#for(i in 1:nlayers(pred_stack_all_609)){
#  range_pred[i,]<-range(values(pred_stack_all_609[[i]]), na.rm=T)
#}
#range_pred$names<-names(pred_stack_all_609)
#extract a smaller area
#plot(pred_stack_all_36$albedo)
#extent(pred_stack_all_36)
#coords<-c(7.473961, 7.484256 , 51.8402 , 51.85021)
#pred_stack_small<-crop(pred_stack_all_36, coords)

#plot(model$trainingData)
#for(i in 1:length(model$trainingData)){
#  print(range(model$trainingData[,i]))
#}

#range_td<-lapply(model$trainingData, range)
#range_df<-data.frame( "range"=range_td)
#test<-do.call(rbind.data.frame, range_td)
#test$names<-names(range_td)
#names(test)<-c("lowest", "highest", "name")

#predict
model_149_predict<-predict(pred_stack_all_149, model, savePrediction=TRUE)
model_283_predict<-predict(pred_stack_all_283, model, savePrediction=TRUE)
model_387_predict<-predict(pred_stack_all_387, model, savePrediction=TRUE)
beep()

#view
mapview(model_149_predict) #2020-06-11 04:00:00
spplot(model_149_predict)
mapview(model_283_predict) #2020-06-16 18:00:00
mapview(model_387_predict) #2020-07-05 01:00:00

png(file="predict_283_klaus.png", width = 300, height=200, units="mm", res = 200)
spplot(model_283_predict)
dev.off()

#calculate AOA
model_149_aoa<-aoa(pred_stack_all_149, model, cl=cl)
model_283_aoa<-aoa(pred_stack_all_283, model, cl=cl)
model_387_aoa<-aoa(pred_stack_all_387, model, cl=cl)

#plot aoa
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction")
#DI
mapview(model_283_aoa$DI)
png(file="DI_283_klaus.png", width = 300, height=200, units="mm", res = 200)
spplot(model_283_aoa$DI)
dev.off()

png(file="DI_hist_283_klaus.png", width = 300, height=200, units="mm", res = 200)
hist(model_283_aoa$DI)
dev.off()
#AOA
png(file="AOA_283_klaus.png", width = 300, height=200, units="mm", res = 200)
spplot(model_283_aoa$AOA)
dev.off()
png(file="AOA_hist_283_klaus.png", width = 300, height=200, units="mm", res = 200)
hist(model_283_aoa$AOA, breaks=100)
dev.off()

saveRDS(file="model_aoa_283.RDS", model_283_aoa)
saveRDS(file= "pred_stack_283.RDS",pred_stack_all_283)

range(values(model_283_aoa$AOA), na.rm=T) # 1 1
range(values(model_283_aoa$DI), na.rm=T) #1.732454e-07 7.434506e-01

plot(model_149_aoa$AOA)
which.max(values(model_149_aoa$DI))

summary(values(model_149_aoa$DI))
sum(!is.na(values(model_149_aoa$DI)))
