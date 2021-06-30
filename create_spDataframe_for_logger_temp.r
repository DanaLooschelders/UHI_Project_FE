library(readxl)
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
library(tidyverse)
library(lubridate)
#create spatial points dataframe with Logger Temp for a certain time
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Temp_Logger")
#load logger data
logger=read.csv(file="Logger_2020.csv", header=T)
logger=logger[,-1] #remove first column that contained rownames
colnames(logger)[1:32]=substr(colnames(logger)[1:32], start=2, stop=10)
logger$datetime<-as.POSIXct(logger$datetime)
#load coordinates of logger
#read in metadata
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Temp_Logger/")
des=read_excel(path = "Sensortabelle_Kartierung_Stand_22.07.2020_DL_ohne_meta.xlsx", 
               col_names = T, na = "NA")
#exclude water temp logger
waterlogger=des$Logger_ID[des$Loggertyp=="WL"]
logger <- logger[ , ! names(logger) %in% c(waterlogger, "69")] 
#load modis times and dates to match with logger temp
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/")
aqua<-read.csv(file="aqua_processed/aqua_times.csv")
aqua$datetime<- as.POSIXct(aqua$datetime)
terra<-read.csv(file="terra_processed/terra_times.csv")
terra$datetime<- as.POSIXct(terra$datetime)
#round time to nearest 10 mins
aqua$datetime_round<- round_date(aqua$datetime,unit="10 minutes")
terra$datetime_round<-round_date(terra$datetime,unit="10 minutes")
#bind together
modis<-rbind(aqua, terra)
#create a metadata table for logger
#set ID as first column
metadata=data.frame("Logger_ID"=as.integer(colnames(logger)[1:26]))
#get metadata from loggers actually used (by names of loggers in list)
metadata=merge(metadata, des,by = "Logger_ID")
#correct lat/lon values
metadata$Lat=metadata$Lat/1000000
metadata$Lat=metadata$Lon/1000000

#create dataframe per time
for(i in 1:length(modis$filename)){
  if(any(logger$datetime==modis$datetime_round[i], na.rm = T))
  {if(i==1){
    logger_match<-list()
    logger_match[[i]]<-data.frame(ID<-colnames(logger)[1:length(colnames(logger))-1], 
         temperature<-t(logger[logger$datetime==modis$datetime_round[i],
                               1:ncol(logger)-1]), row.names=F)
    names(logger_match[[i]])<-c("ID", "Temperature")
    names(logger_match)[[i]]<-modis$filename[i]
  }else{
    logger_match[[i]]<-data.frame(ID=colnames(logger)[1:length(colnames(logger))-1], 
         temperature<-t(logger[logger$datetime==modis$datetime_round[i],
                               1:ncol(logger)-1]))
    names(logger_match[[i]])<-c("ID", "Temperature")
    names(logger_match)[[i]]<-modis$filename[i]
  }
  }else{}
}

names(logger_match)<-modis$filename
#add Temperture to metadata

metadata$Temperature=NA
for(i in as.integer(colnames(logger)[1:26])){
  index=which(colnames(logger)==as.character(i))
  metadata$Temperature[metadata$Logger_ID==i]<-logger[index,1]
}

metadata$Temperature
#create spatialpointsdataframe with logger coordinates
test=SpatialPointsDataFrame(coords = metadata[,4:5], data=)

#match temperature for a certain coordinate by logger ID
