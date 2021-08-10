#predict Model 3

#load model 1 and 2
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modelle")

model_3<-readRDS(file="ffs_Model_2021-08-10.RDS")
####Predict with datasets from training data####
pred_stack<-stack(copernicus, lcz_10m)
meteo_day<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_steinf_geo/", 
                       "Meteo__", "1", sep=""))
#resample meteo
meteo<-resample(meteo, pred_stack, method="ngb")
#stack modis and pred_stack and meteo_stack
pred_stack_all <- stack(pred_stack, meteo)

####Predict Model 3####

#meteo_day<-stack(paste("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Meteo_data_Steinf/", 
#"Meteo_", "MOD11A1_A2020189_12_47", sep=""))
names(pred_stack_all)<-c( "copernicus", "ucz", 
                          "meteo_RH", "meteo_Temp", "meteo_stability",
                          "meteo_cloudcover", "meteo_wind")
#predict
model_3_predict<-predict(pred_stack_all, model_3, savePrediction=TRUE)
mapview(model_3_predict)
#calculate AOA
model_3_aoa<-aoa(pred_stack_all, model_3)
#calculate percentage of areas outside AOA
ncell(model_2_day_aoa$AOA[model_2_day_aoa$AOA==1])/ncell(model_2_day_aoa$AOA[!is.na(model_2_day_aoa$AOA)])*100
#remove values outside of AOA
model_2_day_predict[model_2_day_aoa$AOA == 0] <- NA

####Predict Model 2 - Night####
meteo_night<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_Steinf/", 
                         "Meteo_", "MYD11A1_A2020193_02_59", sep=""))

#meteo_night<-stack(paste("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Meteo_data_Steinf/", 
#"Meteo_", "MYD11A1_A2020158_04_11", sep=""))
pred_stack_2 <- stack(modis_night,pred_resample, meteo_night)
names(pred_stack_2)<-c("modis", "copernicus", "dlm", "ucz", 
                       "meteo_RH", "meteo_Temp", "meteo_SWup")
#predict
model_2_night_predict<-predict(pred_stack_2, model_2, savePrediction=TRUE)
#mapview(model_2_night_predict)+mapview(modis_night)
#calculate AOA
model_2_night_aoa<-aoa(pred_stack_2, model_2, returnTrainDI = T)
mapview(model_2_night_aoa)

#calculate percentage of areas inside AOA
ncell(model_2_night_aoa$AOA[model_2_night_aoa$AOA==1])/ncell(model_2_night_aoa$AOA[!is.na(values(model_2_night_aoa$AOA))])*100
#ncell(model_2_day_aoa$AOA[model_2_day_aoa$AOA==1])/ncell(model_2_day_aoa$AOA)*100
#remove values outside of AOA
model_2_night_predict[model_2_night_aoa$AOA == 0] <- NA

