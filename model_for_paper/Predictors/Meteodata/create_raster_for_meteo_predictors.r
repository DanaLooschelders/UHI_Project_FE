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
#load meteo data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Meteorologie")
meteo<-read.csv("meteo_all.csv")
str(meteo)

#get shape of polygon
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")

#load refrence raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness")
ref_raster<-raster("copernicus_imperviousness_crop_MS_10m.tif")
#transform polygon into Raster
r <- raster(ncol=ncol(ref_raster), nrow=nrow(ref_raster))
extent(r) <- extent(ref_raster)
raster_Steinf<-rasterize(gadm, r)
values(raster_Steinf)

#use for loop to create raster layer (stack for each point in time)
#use raster_Steinf as dummy raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Meteorologie/Rasterdata/")

for(i in 1:nrow(meteo)){
  #Temperature
  raster_Steinf_temp<-raster_Steinf
  values(raster_Steinf_temp)<-meteo$meteo_Temp[i]
  #relative humidity
  raster_Steinf_RH<-raster_Steinf
  values(raster_Steinf_RH)<-meteo$meteo_rH[i]
  #stability
  raster_Steinf_stability<-raster_Steinf
  values(raster_Steinf_stability)<-as.factor(meteo$meteo_stability[i])
  #cloudcover
  raster_Steinf_cloudcover<-raster_Steinf
  values(raster_Steinf_cloudcover)<-meteo$meteo_cloudcover[i]
  #radiation
  raster_Steinf_radiation<-raster_Steinf
  values(raster_Steinf_radiation)<-meteo$meteo_radiation[i]
  #cum_radiation
  raster_Steinf_cum_radiation<-raster_Steinf
  values(raster_Steinf_cum_radiation)<-meteo$meteo_cum_radiation[i]
  #wind
  raster_Steinf_wind<-raster_Steinf
  values(raster_Steinf_wind)<-meteo$meteo_windspeed[i]
  #stack values
  meteo_stack<-stack(raster_Steinf_RH, raster_Steinf_temp, raster_Steinf_stability,
                     raster_Steinf_cloudcover, raster_Steinf_radiation, 
                     raster_Steinf_cum_radiation, raster_Steinf_wind)
  #set layer names
  names(meteo_stack)<-c("meteo_RH", "meteo_Temp", "meteo_stability", 
                        "meteo_cloudcover", "meteo_radiation", "meteo_cum_radiation",
                        "meteo_wind")
  #write raster into file
  writeRaster(meteo_stack, filename = paste("Meteo_", i, 
                                            sep="_"), overwrite=F)
}



