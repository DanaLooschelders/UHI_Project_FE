library(readxl)
library(raster)
library(sp)
library(MODIS)
library(MODISTools)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
library(tidyverse)
#create spatial points dataframe with Logger Temp for a certain time
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Temp_Logger")
#load logger data
logger=read.csv(file="Logger_2020.csv", header=T)
logger=logger[,-1] #remove first column that contained rownames
colnames(logger)[1:32]=substr(colnames(logger)[1:32], start=2, stop=10)
#load coordinates of logger
#read in metadata
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Temp_Logger/")
des=read_excel(path = "Sensortabelle_Kartierung_Stand_22.07.2020_DL_ohne_meta.xlsx", 
               col_names = T, na = "NA")
#create a metadata table for logger
#set ID as first column
metadata=data.frame("Logger_ID"=as.integer(colnames(logger)[1:32]))
#get metadata from loggers actually used (by names of loggers in list)
metadata=merge(metadata, des,by = "Logger_ID")
#correct lat/lon values
metadata$Lat=metadata$Lat/1000000
metadata$Lat=metadata$Lon/1000000
#add Temperture to metadata
metadata$Temperature=NA
for(i in as.integer(colnames(logger)[1:32])){
  index=which(colnames(logger)==as.character(i))
  metadata$Temperature[metadata$Logger_ID==i]<-logger[index,1]
}

metadata$Temperature
#create spatialpointsdataframe with logger coordinates
test=SpatialPointsDataFrame(coords = metadata[,4:5], data=)

#match temperature for a certain coordinate by logger ID
