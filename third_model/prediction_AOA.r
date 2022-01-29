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
#predict Model 3

#load model 1 and 2
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modelle")

model_3<-readRDS(file="ffs_Model_2021-08-10.RDS")
####Predict with datasets from training data####
pred_stack<-stack(copernicus, lcz_10m)
mapview(pred_stack)
#use meteo__12: 2020-07-07 13:00:00
meteo_day<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_steinf_geo/", 
                       "Meteo__", "12", sep=""))
mapview(meteo_day)
#resample meteo
meteo<-raster::resample(meteo_day, pred_stack, method="ngb")
#stack modis and pred_stack and meteo_stack
pred_stack_all <- stack(pred_stack, meteo)
mapview(pred_stack_all)
####Predict Model 3####

#meteo_day<-stack(paste("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Meteo_data_Steinf/", 
#"Meteo_", "MOD11A1_A2020189_12_47", sep=""))
names(pred_stack_all)<-c( "copernicus", "ucz", 
                          "meteo_RH", "meteo_Temp", "meteo_stability",
                          "meteo_cloudcover", "meteo_wind")
#predict
model_3_day_predict<-predict(pred_stack_all, model_3, savePrediction=TRUE)
mapview(model_3_day_predict)
#calculate AOA
model_3_aoa<-aoa(pred_stack_all, model_3)
#calculate percentage of areas outside AOA
ncell(model_3_aoa$AOA[model_3_aoa$AOA==1])/ncell(model_3_aoa$AOA[!is.na(model_3_aoa$AOA)])*100
#remove values outside of AOA
model_3_day_predict[model_3_aoa$AOA == 0] <- NA
mapview(model_3_day_predict)

####Predict Model 3 - Night####
#2020-07-11 03:00:00
meteo_night<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_steinf_geo/", 
                       "Meteo__", "98", sep=""))
#resample meteo
meteo_night<-raster::resample(meteo_night, pred_stack, method="ngb")
#mapview(meteo_night)
#meteo_night<-stack(paste("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Meteo_data_Steinf/", 
#"Meteo_", "MYD11A1_A2020158_04_11", sep=""))
pred_stack_all <- stack(pred_stack, meteo_night)
names(pred_stack_all)<-c( "copernicus", "ucz", 
                          "meteo_RH", "meteo_Temp", "meteo_stability",
                          "meteo_cloudcover", "meteo_wind")
#predict
model_3_night_predict<-predict(pred_stack_all, model_3, savePrediction=TRUE)
mapview(model_3_night_predict)

mapview(test)
#mapview(model_2_night_predict)+mapview(modis_night)
#calculate AOA
model_3_night_aoa<-aoa(pred_stack_all, model_3)
mapview(model_3_night_aoa)
mapview(pred_stack_all)
#calculate percentage of areas inside AOA
ncell(model_3_night_aoa$AOA[model_3_night_aoa$AOA==1])/ncell(model_3_night_aoa$AOA[!is.na(values(model_3_night_aoa$AOA))])*100
#ncell(model_2_day_aoa$AOA[model_2_day_aoa$AOA==1])/ncell(model_2_day_aoa$AOA)*100
#remove values outside of AOA
model_3_night_predict[model_3_night_aoa$AOA == 0] <- NA

####Visualisation####
#set colors
cols_day<-mapviewPalette("mapviewSpectralColors")(40)[22:40]
cols_night<-mapviewPalette("mapviewSpectralColors")(40)[12:22]
#cols<-mapviewPalette("mapviewSpectralColors")(55)[25:55]
#visualize model 3 day
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Maps")
map <-   tm_shape(shp = gadm)+
  tm_polygons(col="black")+
  tm_shape(model_3_day_predict, 
           raster.downsample = FALSE) +
  tm_raster(title = "Predicted Air \nTemperature [°C]",
            legend.hist=T,
            palette = cols_day,
            breaks=seq(18,30, 1))+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            bg.color="white",
            legend.bg.alpha = 0.8,
            legend.outside=T,
            legend.title.size = 1,
            legend.outside.size = 0.5)+
  tm_add_legend(type = "fill",
                col="black",
                labels = "Outside AOA")+
  tm_compass(position = c("left","bottom"))
map
#save
tmap_save(map, "map_model_3_day.png", width=10, height=7)

ncell(model_3_day_predict)
ncell(model_2_day_predict)
mapview(model_2_day_predict)

model_3_night_predict<-mask(model_3_night_predict, gadm)
map <-   tm_shape(shp = gadm)+
  tm_polygons(col="black")+
  tm_shape(model_3_night_predict,
           raster.downsample = FALSE,) +
  tm_raster(title = "Predicted Air \nTemperature [°C]",
            legend.hist=T,
            palette = cols_night,
            breaks=seq(8,17, 1)
  )+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            bg.color="white",
            legend.bg.alpha = 0.8,
            legend.outside=T,
            legend.title.size = 1,
            legend.outside.size = 0.5)+
  tm_add_legend(type = "fill",
                col="black",
                labels = "Outside AOA")+
  tm_compass(position = c("left","bottom"))
map
tmap_save(map, "map_model_3_night.png", width=10, height=7)

model_3$results

values(pred_stack_all)
