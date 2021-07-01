#create Polygon with shape of MS for steinfurter point data for a certain date

#load librarys
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)

#get modis sample
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed_resampled/")
modisfiles=list.files(pattern="*.tif")
modis=raster(modisfiles[8])

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

raster_Steinf_temp<-raster_Steinf
values(raster_Steinf_temp)<-meteo$AirTC2m_Avg[1]

raster_Steinf_RH<-raster_Steinf
values(raster_Steinf_RH)<-meteo$RH2m_Avg[1]

meteo_stack<-stack(raster_Steinf_RH, raster_Steinf_temp)
