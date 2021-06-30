rm(list=ls() ) 
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_UCZ")

library(sp)
library(sf)
library(mapview)
library(raster)
library(rgdal)
ucz <-raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_UCZ/13322450/EU_LCZ_map.tif")
mapview(ucz)  
resolu

gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)

ucz_ext <- projectExtent(ucz, crs="+proj=longlat +datum=WGS84 +no_defs")
#Achtung, das dauert lange! 
ucz_proj=projectRaster(ucz, ucz_ext, crs="+proj=longlat +datum=WGS84 +no_defs") 

ucz_proj_crop <- crop(ucz_proj, gadm_sf)
mapview(ucz_proj_crop)

setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/FE_UCZ")
writeRaster(ucz_proj_crop,"ucz_ms_100m.tif", overwrite = T)
