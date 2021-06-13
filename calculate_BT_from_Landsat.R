library(LST)
library(raster)
library(mapview)
library(sf)
library(sp)
library(rgdal)
library(gdalUtils)
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung")

LS10 <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B10.TIF")
LS11 <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B11.TIF")

gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
mapview(LS10)
gadm_sf <- as(gadm,"sf")

crs(gadm_sf)
crs(LS10) 

gadm_sf <- st_transform(gadm_sf, crs = st_crs(LS10))

LS10_crop <- crop(y=gadm_sf,x=LS10)

e <- extent(395103.5,415705.1,5744177,5768658)
LS10_crop <- crop(LS10,e)
extent(gadm_sf)
extent(LS10)
mapview(LS10_crop)

BT(Landsat_10 = LS10 , Landsat_11 = LS11)
