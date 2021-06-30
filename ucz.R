rm(list=ls() ) 
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_UCZ")

library(sp)
library(sf)
library(mapview)
library(raster)
library(rgdal)
ucz <- raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_UCZ/13322450/EU_LCZ_map.tif")
e <- extent(2735200, 4030800, 3279800, 4854600)#roughly cropped to make it faster to calculate 
ucz <- crop(ucz,e)
mapview(ucz)  

gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)

ucz_proj=projectRaster(ucz, crs="+proj=longlat +datum=WGS84 +no_defs") 

crs(ucz) <- crs(gadm_sf) 

extent(ucz) <- extent(7.4739637,7.774222 ,51.84019,52.06018)


projection <- CRS('+init=EPSG:25832')
ucz_ms <- raster(e,
            crs = projection)
res(ucz_ms) <- 100

ucz_ms <- projectRaster(ucz, projection, alignOnly = F)

#ucz_ms <- projectRaster(from=ucz_ms, crs=crs(gadm))
#crs(ucz_ms) <- "+proj=longlat +datum=WGS84 +no_defs "
mapview(ucz_ms) #check 

ucz_ms_crop <- crop(ucz, gadm_sf)
mapview(ucz_ms_crop)

setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/FE_UCZ")
writeRaster(ucz_ms,"ucz_ms.grd", overwrite = T)
