#Model validation and visualization
library(mapview)
library(raster)

#plot variable importance
plot(model_ffs) # see tuning results
plot(varImp(model_ffs)) # variablenwichtigkeit

#model performance
plot_ffs(model_ffs)
plot_ffs(model_ffs, type="selected")

model_ffs$modelType
model_ffs$results
model_ffs$bestTune
model_ffs$selectedvars
model_ffs$selectedvars_perf_SE
model_ffs$metric
model_ffs$selectedvars_perf

#load model 1 and 2
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modelle")
model_1<-readRDS(file="ffs_Model_2021-07-07.RDS")
model_2<-readRDS(file="ffs_Model_2021-07-08.RDS")

#load modis scene day and  night
modis_day <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
#mapview(modis_day)
modis_night <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed_resampled/aqua_ aqua__MYD11A1_A2020158_04_11_ .tif")
#mapview(modis_night)

####Predict Model 1 - Day####
pred_stack_1 <- stack(modis_day,pred_resample)
names(pred_stack_1)<-c("modis", "copernicus", "dlm", "ucz")
#predict
model_1_day_predict<-predict(pred_stack_1, model_1, savePrediction=TRUE)
#calculate AOA
model_1_day_aoa<-aoa(pred_stack_1, model_1)
#calculate percentage of areas outside AOA
ncell(model_1_day_aoa$AOA[model_1_day_aoa$AOA==1])/ncell(model_1_day_aoa$AOA)*100
#remove values outside of AOA
model_1_day_predict[model_1_day_aoa$AOA == 0] <- NA

####Predict Model 1 - Night####
pred_stack_1 <- stack(modis_night,pred_resample)
names(pred_stack_1)<-c("modis", "copernicus", "dlm", "ucz")
#predict
model_1_night_predict<-predict(pred_stack_1, model_1, savePrediction=TRUE)
#calculate AOA
model_1_night_aoa<-aoa(pred_stack_1, model_1)
#calculate percentage of areas outside AOA
ncell(model_1_night_aoa$AOA[model_1_night_aoa$AOA==1])/ncell(model_1_night_aoa$AOA)*100
#remove values outside of AOA
model_1_night_predict[model_1_night_aoa$AOA == 0] <- NA

####Predict Model 2 - Day####
meteo_day<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_Steinf/", 
                   "Meteo_", "MOD11A1_A2020189_12_47", sep=""))
pred_stack_2 <- stack(modis_day,pred_resample, meteo_day)
names(pred_stack_2)<-c("modis", "copernicus", "dlm", "ucz", 
                     "meteo_RH", "meteo_Temp", "meteo_SWup")
#predict
model_2_day_predict<-predict(pred_stack_2, model_2, savePrediction=TRUE)
mapview(model_2_day_predict)
#calculate AOA
model_2_day_aoa<-aoa(pred_stack_2, model_2)
#calculate percentage of areas outside AOA
ncell(model_2_day_aoa$AOA[model_2_day_aoa$AOA==1])/ncell(model_2_day_aoa$AOA)*100
#remove values outside of AOA
model_2_day_predict[model_2_day_aoa$AOA == 0] <- NA

####Predict Model 2 - Night####
meteo_night<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_Steinf/", 
                   "Meteo_", "MYD11A1_A2020158_04_11", sep=""))
pred_stack_2 <- stack(modis_night,pred_resample, meteo_night)
names(pred_stack_2)<-c("modis", "copernicus", "dlm", "ucz", 
                     "meteo_RH", "meteo_Temp", "meteo_SWup")
#predict
model_2_night_predict<-predict(pred_stack_2, model_2, savePrediction=TRUE)
mapview(model_2_night_predict)
#calculate AOA
model_2_night_aoa<-aoa(pred_stack_2, model_2)
model_2_night_aoa
#calculate percentage of areas outside AOA
ncell(model_2_night_aoa$AOA[model_2_night_aoa$AOA==1])/ncell(model_2_night_aoa$AOA)*100
#remove values outside of AOA
model_2_night_predict[model_2_night_aoa$AOA == 0] <- NA
