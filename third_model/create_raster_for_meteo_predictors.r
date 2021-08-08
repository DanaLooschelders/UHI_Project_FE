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
#get shape of polygon
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")

#transform polygon into Raster
r <- raster(ncol=ncol(modis), nrow=nrow(modis))
extent(r) <- extent(modis)
raster_Steinf<-rasterize(gadm, r)
values(raster_Steinf)

#use for loop to create raster layer (stack for each point in time)
#use raster_Steinf as dummy raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_steinf_geo/")
i=1
for(i in 1:nrow(meteo)){
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
  #wind
  raster_Steinf_wind<-raster_Steinf
  values(raster_Steinf_wind)<-meteo$meteo_windspeed[i]
  #stack values
  meteo_stack<-stack(raster_Steinf_RH, raster_Steinf_temp, raster_Steinf_stability,
                     raster_Steinf_cloudcover, raster_Steinf_wind)
  #set layer names
  names(meteo_stack)<-c("meteo_RH", "meteo_Temp", "meteo_stability", 
                        "meteo_cloudcover", "meteo_wind")
  #write raster into file
  writeRaster(meteo_stack, filename = paste("Meteo_", i, 
                                            sep="_"), overwrite=F)
}



