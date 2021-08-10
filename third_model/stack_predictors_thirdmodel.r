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

####LCZ (disaggregate to 10m)####
#read in data 
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_UCZ")
lcz<-raster("ucz_ms_100m.tif")
#disaggregate to 10 m resolution
lcz_10m<-disaggregate(lcz, fact=10)
#####Tree cover (10m)####
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Copernicus")

#read in 10m Copernicus data
copernicus<-raster("copernicus_tree_cover_crop_MS_10m.tif")
#get cop and lcz to same resolution
lcz_10m <- resample(lcz_10m,copernicus, method="ngb")
pred_stack<-stack(copernicus, lcz_10m)

####prep training data: logger and netatmo (1h res)####

#Training_dat = alle Daten
#traintemps = für die Validierung

#remoce temp items
remove(total_stack, total_stack_temp)
#load dynamic predictors 

for(i in names(spatial_list_all)){
  if(!exists("total_stack")){
      #index logger list to get one element
      logger_dat<-spatial_list_all[[i]]
      #load meteo raster stack
      meteo<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_steinf_geo/", "Meteo__", i, ".grd", sep=""))
      #resample meteo
      meteo<-resample(meteo, pred_stack, method="ngb")
      #stack modis and pred_stack and meteo_stack
      pred_stack_all <- stack(pred_stack, meteo)
      #extract predictor values for training gdata
      extr <- raster::extract(pred_stack_all,logger_dat,df=TRUE)
      #create ID by row names
      logger_dat$ID<-row.names(logger_dat)
      #merge
      extr <- merge(extr,logger_dat@data,by.x="ID")
      #create column with time span value
      extr$time<-all_temp$datetime[as.numeric(i)]
      #rename
      total_stack<-extr
      #change colnames
      colnames(total_stack)<-c("ID", "copernicus", "ucz", 
                               "meteo_RH", "meteo_Temp", "meteo_stability",
                               "meteo_cloudcover", "meteo_wind", 
                               "temp", "time")
    }
  else{
    #index logger list to get one element
    logger_dat<-spatial_list_all[[i]]
    #load meteo raster stack
    meteo<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_steinf_geo/", "Meteo__", i, ".grd", sep=""))
    #resample meteo
    meteo<-resample(meteo, pred_stack, method="ngb")
    #stack modis and pred_stack and meteo_stack
    pred_stack_all <- stack(pred_stack, meteo)
    #extract predictor values for training gdata
    extr <- raster::extract(pred_stack_all,logger_dat,df=TRUE)
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    #rename
    total_stack_temp<-extr
    #change colnames
    colnames(total_stack_temp)<-c("ID", "copernicus", "ucz", 
                             "meteo_RH", "meteo_Temp", "meteo_stability",
                             "meteo_cloudcover", "meteo_wind", 
                             "temp", "time")
      #rbind both dataframes
      total_stack<-rbind(total_stack, total_stack_temp)
    }
  }

str(total_stack$time)
total_stack$time<-as.POSIXct(total_stack$time)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/")
write.csv(total_stack, file="total_stack_data_thirdmodel.csv")
