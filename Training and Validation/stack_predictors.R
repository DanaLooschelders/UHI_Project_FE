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
#build dataframe for total model
#combine terra and aqua temp dataframe to index time span by name of scene 
modis_times<-rbind(terra_times, aqua_times)
#load static predictor stack
pred<-stack("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/pred_stack.grd")

#change extent o pred stack
modis_test <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
extent(pred)<-extent(modis_test)
#remoce temp items
remove(total_stack, total_stack_temp)
i <- names(spatial_list)[4]

#load dynamic predictors (modis, temp)
for(i in names(spatial_list)){
  if(!exists("total_stack")){
    #split for terra and aqua
    if(substr(i,start = 1, stop=7)=="MYD11A1"){ #aqua
      #index logger list to get one element
      logger_dat<-spatial_list[grep(names(spatial_list),pattern=i)]
      #convert element from list to dataframe
      logger_dat<-logger_dat[[1]]
      #load corresponding modis scene
      modis <- raster(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed_resampled/aqua_ aqua__",i,"_ .tif", sep=""))
      #resample modis
      pred_resample <- resample(pred,modis)
      #stack modis and pred_stack
      pred_stack <- stack(modis,pred_resample)
      #extract predictor values for trainin gdata
      extr <- extract(pred_stack,logger_dat,df=TRUE)
      #create ID by row names
      logger_dat$ID<-row.names(logger_dat)
      #merge
      extr <- merge(extr,logger_dat@data,by.x="ID")
      #create column with time span value
      extr$time<-modis_times$datetime[modis_times$filename==i]
      #rename
      total_stack<-extr
      #change colnames
      colnames(total_stack)<-c("ID", "modis", "copernicus", "dlm", "ucz", "temp", "time")
      
    }else{ #terra
      #index logger list to get one element
      logger_dat<-spatial_list[grep(names(spatial_list),pattern=i)]
      #convert element from list to dataframe
      logger_dat<-logger_dat[[1]]
      #load corresponding modis scene
      modis <- raster(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__",i,"_ .tif", sep=""))
      #resample modis
      pred_resample <- resample(pred,modis)
      #stack modis and pred_stack
      pred_stack <- stack(modis,pred_resample)
      #extract predictor values for trainin gdata
      extr <- extract(pred_stack,logger_dat,df=TRUE)
      #create ID by row names
      logger_dat$ID<-row.names(logger_dat)
      #merge
      extr <- merge(extr,logger_dat@data,by.x="ID")
      #create column with time span value
      extr$time<-modis_times$datetime[modis_times$filename==i]
      #rename
      total_stack<-extr
      #change colnames
      colnames(total_stack)<-c("ID", "modis", "copernicus", "dlm", "ucz", "temp", "time")
      
    }
  }else{
    #split for terra and aqua
    if(substr(i,start = 1, stop=7)=="MYD11A1"){ #aqua
      #index logger list to get one element
      logger_dat<-spatial_list[grep(names(spatial_list),pattern=i)]
      #convert element from list to dataframe
      logger_dat<-logger_dat[[1]]
      #load corresponding modis scene
      modis <- raster(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed_resampled/aqua_ aqua__",i,"_ .tif", sep=""))
      #resample modis
      pred_resample <- resample(pred,modis)
      #stack modis and pred_stack
      pred_stack <- stack(modis,pred_resample)
      #extract predictor values for trainin gdata
      extr <- extract(pred_stack,logger_dat,df=TRUE)
      #create ID by row names
      logger_dat$ID<-row.names(logger_dat)
      #merge
      extr <- merge(extr,logger_dat@data,by.x="ID")
      #create column with time span value
      extr$time<-modis_times$datetime[modis_times$filename==i]
      #rename
      total_stack_temp<-extr
      #change colnames
      colnames(total_stack_temp)<-c("ID", "modis", "copernicus", "dlm", "ucz", "temp", "time")
      #rbind both dataframes
      total_stack<-rbind(total_stack, total_stack_temp)
    }else{ #terra
      #index logger list to get one element
      logger_dat<-spatial_list[grep(names(spatial_list),pattern=i)]
      #convert element from list to dataframe
      logger_dat<-logger_dat[[1]]
      #load corresponding modis scene
      modis <- raster(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__",i,"_ .tif", sep=""))
      #resample modis
      pred_resample <- resample(pred,modis)
      #stack modis and pred_stack
      pred_stack <- stack(modis,pred_resample)
      #extract predictor values for trainin gdata
      extr <- extract(pred_stack,logger_dat,df=TRUE)
      #create ID by row names
      logger_dat$ID<-row.names(logger_dat)
      #merge
      extr <- merge(extr,logger_dat@data,by.x="ID")
      #create column with time span value
      extr$time<-modis_times$datetime[modis_times$filename==i]
      #rename
      total_stack_temp<-extr
      #change colnames
      colnames(total_stack_temp)<-c("ID", "modis", "copernicus", "dlm", "ucz", "temp", "time")
      #rbind both dataframes
      total_stack<-rbind(total_stack, total_stack_temp)
    }
  }
}

str(total_stack$time)
total_stack$time<-as.POSIXct(total_stack$time)

write.csv(total_stack, file="total_stack_data.csv")
