rm(list=ls() ) 
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_UCZ")

library(sp)
library(sf)
library(mapview)
library(raster)
library(rgdal)

#load data 
ucz <-raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_UCZ/13322450/EU_LCZ_map.tif")
#mapview(ucz)  
#crop roughly to lower calculation time 
e <- extent(4100000, 4250000, 3150000, 3230000) 
ucz_crop <- crop(ucz, e)
#mapview(ucz_crop)

#load shape of münster 
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
#mapview(gadm_sf)

#project to right crs
ucz_ext <- projectExtent(ucz_crop, crs="+proj=longlat +datum=WGS84 +no_defs")
ucz_proj=projectRaster(ucz_crop, ucz_ext, method="ngb") 
#mapview(ucz_proj)

#crop to shape of münster 
ucz_proj_crop <- crop(ucz_proj, gadm_sf) 
#mapview(ucz_proj_crop)
 
#save as raster 
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/FE_UCZ")
writeRaster(ucz_proj_crop,"ucz_ms_100m.tif", overwrite = T)




