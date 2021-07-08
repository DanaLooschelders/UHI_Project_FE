#create Polygon with shape of MS for steinfurter point data for a certain date

#load librarys
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
library(readxl)
library(tidyverse)
library(lubridate)
library(openair)
#get modis sample
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed_resampled/")
modisfiles=list.files(pattern="*.tif")
modis=raster(modisfiles[8])


#load modis times and dates to match with logger temp
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/")
aqua<-read.csv(file="aqua_processed/aqua_times.csv")
aqua$datetime<- as.POSIXct(aqua$datetime)
terra<-read.csv(file="terra_processed/terra_times.csv")
terra$datetime<- as.POSIXct(terra$datetime)
#combine terra and aqua temp dataframe to index time span by name of scene 
modis_times<-rbind(terra_times, aqua_times)
#round to ten min to match meteodata
modis_times$datetime_10min<-round_date(as.POSIXct(modis_times$datetime), unit="10mins")
#remove NAs
modis_times<-modis_times[complete.cases(modis_times),]
#get shape of polygon
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")

#transform polygon into Raster
r <- raster(ncol=ncol(modis), nrow=nrow(modis))
extent(r) <- extent(modis)
raster_Steinf<-rasterize(gadm, r)
values(raster_Steinf)

#read in meteodata
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_SteinfurterStr/2020")
meteo=read.table(file="meteo_data_2020.csv", sep=";", dec=",", header=T)

#subset meteodata to needed rows (times where modis data is available)
meteo<-meteo[as.POSIXct(meteo$timestamp)%in%as.POSIXct(modis_times$datetime_10min),]
#use for loop to create raster layer (stack for each point in time)
#use raster_Steinf as dummy raster
output.names<-paste("Steinf_", modis_times$filename, sep="")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_Steinf")
i=1
for(i in 1:nrow(meteo)){
    #Temperature
    raster_Steinf_temp<-raster_Steinf
    values(raster_Steinf_temp)<-meteo$AirTC2m_Avg[i]
    #relative humidity
    raster_Steinf_RH<-raster_Steinf
    values(raster_Steinf_RH)<-meteo$RH2m_Avg[i]
    #short wave radiation
    raster_Steinf_SW_up<-raster_Steinf
    values(raster_Steinf_SW_up)<-meteo$SUp_Avg[i]
    #stack values
    meteo_stack<-stack(raster_Steinf_RH, raster_Steinf_temp, raster_Steinf_SW_up)
    #set layer names
    names(meteo_stack)<-c("meteo_RH", "meteo_Temp", "meteo_SWup")
    #get matching modis name
    filename=modis_times$filename[modis_times$datetime_10min==meteo$timestamp[i]]
    #write raster into file
    writeRaster(meteo_stack, filename = paste("Meteo_", filename, sep=""), overwrite=T)
}



