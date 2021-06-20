rm(list=ls() ) 
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/ucz")

library(sp)
library(sf)
library(mapview)
library(raster)
library(rgdal)
ucz <- raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/ucz/13322450/EU_LCZ_map.tif")

gadm <- getData('GADM',country='DEU', level =2)
ms <- gadm[gadm$NAME_2 == "Münster",]
ms_sf <- as(ms,"sf")
plot(ms_sf)
crs(ms_sf) <- CRS('+init=EPSG:25832')

e <- extent(395103.5,415705.1,5744177, 5768658)
projection <- CRS('+init=EPSG:25832')
ucz_ms <- raster(e,
            crs = projection)

#raster::crs(ucz) <- CRS('+init=EPSG:25832')
ucz_ms <- projectRaster(ucz, ucz_ms, projection, alignOnly = F)

writeRaster(ucz_ms,"ucz_ms.grd", overwrite = T)
