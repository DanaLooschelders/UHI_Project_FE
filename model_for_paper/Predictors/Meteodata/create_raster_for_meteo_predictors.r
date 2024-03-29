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
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Meteorologie")
meteo<-read.csv("meteo_all.csv")
str(meteo)


#get shape of polygon
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)
#load refrence raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness")
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness")
ref_raster<-raster("copernicus_imperviousness_crop_MS_10m.tif")
ncell(ref_raster)
res(ref_raster)
#transform polygon into Raster
r <- raster(ncol=ncol(ref_raster), nrow=nrow(ref_raster), crs = "+proj=longlat +datum=WGS84 +no_defs")
extent(r) <- extent(ref_raster)
raster_Steinf<-rasterize(gadm, r)
#values(raster_Steinf)

#shorten meteo to only the times the training data is available
setwd( "C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger")
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger")
all_temp<-read.csv("all_temp.csv")
trainingtimes<-data.frame("datetime"=as.POSIXct(all_temp$datetime))
meteo$datetime<-as.POSIXct(meteo$datetime)
meteo<-left_join(trainingtimes, meteo, "datetime")
str(meteo)
write.csv(meteo, "meteo_for_raster.csv", row.names = F)#save
#use for loop to create raster layer (stack for each point in time)
#use raster_Steinf as dummy raster
#setwd("C:/Users/Dana/sciebo/UHI_Meteo_Raster")
#setwd("/Volumes/work/UHI_Meteo_Raster")
setwd("E:/meteo_raster") #use external hard drive to not kill sciebo

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
                                            sep="_"), overwrite=T)
}



