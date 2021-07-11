#Model validation and visualization
library(mapview)
library(raster)
library(CAST)

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
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Modelle")
model_1<-readRDS(file="ffs_Model_2021-07-07.RDS")
model_2<-readRDS(file="ffs_Model_2021-07-08.RDS")

#load modis scene day and  night
modis_day <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
modis_day <- raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
#mapview(modis_day)
modis_night <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed_resampled/aqua_ aqua__MYD11A1_A2020158_04_11_ .tif")
modis_night <- raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/FE_LST/aqua_processed_resampled/aqua_ aqua__MYD11A1_A2020158_04_11_ .tif")
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
meteo_day<-stack(paste("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Meteo_data_Steinf/", 
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
meteo_night<-stack(paste("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Meteo_data_Steinf/", 
                         "Meteo_", "MYD11A1_A2020158_04_11", sep=""))
pred_stack_2 <- stack(modis_night,pred_resample, meteo_night)
names(pred_stack_2)<-c("modis", "copernicus", "dlm", "ucz", 
                     "meteo_RH", "meteo_Temp", "meteo_SWup")
#predict
model_2_night_predict<-predict(pred_stack_2, model_2, savePrediction=TRUE)
mapview(model_2_night_predict)+mapview(modis_night)
#calculate AOA
model_2_night_aoa<-aoa(pred_stack_2, model_2, returnTrainDI = T)
mapview(model_2_night_aoa)
#calculate percentage of areas outside AOA
ncell(model_2_night_aoa$AOA[model_2_night_aoa$AOA==1])/ncell(model_2_night_aoa$AOA)*100
ncell(model_2_day_aoa$AOA[model_2_day_aoa$AOA==1])/ncell(model_2_day_aoa$AOA)*100
#remove values outside of AOA
model_2_night_predict[model_2_night_aoa$AOA == 0] <- NA

###### new Modis data from 2021 #### 
library(MODIS)
library(MODISTools)
library(sf)
library(tmap)
library(tmaptools)
EarthdataLogin("Peppermint_Patty", 
               "MilchreisApfelmus.1")

lap = "/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Vailiderung_modis"
MODISoptions(lap, outDirPath = file.path(lap, "PROCESSED")
             , MODISserverOrder = c("LPDAAC", "LAADS"), quiet = TRUE)

### download data##
getHdf("MOD11A1",begin = "2021.06.18", end = "2021.06.22",
       tileH = 18, tileV = 3)

### process data- extract LST 
runGdal(job="LST_Germany","MOD11A1",begin = "2021.06.18", end = "2021.06.22",
        tileH = 18, tileV = 3
        , SDSstring = "100000000000")

modis_19_06 <- raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Vailiderung_modis/PROCESSED/LST_Germany/MOD11A1.A2021170.LST_Day_1km.tif")
modis_19_06_celsius <- (modis_19_06 *0.02-273)

#crop to muenster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm <- as(gadm,"sf")

modis_19_06_proj <- projectRaster(modis_19_06_celsius,crs = crs(gadm))
modis_19_06_crop <- crop(modis_19_06_proj, gadm)
modis_19_06_disagg <- disaggregate(modis_19_06_crop, fact = 10)

setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Vailiderung_modis")
writeRaster(modis_19_06_disagg, "modis_19062021", overwrite =T)

#load modis, model and predstack
modis_2021 <- raster("modis_19062021")
model <- readRDS("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Modelle/ffs_Model_2021-07-07.RDS")
pred <- stack("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/pred_stack.grd")
pred_resample <- resample(pred,modis_2021,method ="ngb")
pred_stack <- stack(modis_2021,pred_resample)
names(pred_stack)<-c("modis", "copernicus", "dlm", "ucz")
predict_2021 <-predict(pred_stack, model, savePrediction=TRUE)

map <- tm_shape(predict_2021,
                raster.downsample = FALSE) +
  tm_raster(title = "Air Temperature")+
  tm_scale_bar(bg.color="white",position = c("right", "bottom"))+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8,
            legend.outside=T)+
  tm_compass(position = c("left","bottom"))
map



