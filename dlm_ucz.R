rm(list=ls() )
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt")

library(sp)
library(sf)
library(mapview)
library(raster)
library(rgdal)

####### land use classification#####
download.file("https://www.opengeodata.nrw.de/produkte/geobasis/lm/dlm50/dlm50_EPSG25832_Shape.zip", destfile ="dlm50.zip")
unzip("dlm50.zip",exdir="dlm")

gadm <- getData('GADM',country='DEU', level =2)
ms <- gadm[gadm$NAME_2 == "Münster",]
plot(ms)
####### einladen #####
gew01 <- st_read("dlm/gew01_f.shp")
#gew02 <- st_read("dlm/gew02_f.shp")

geb02 <- st_read("dlm/geb02_f.shp")
geb03 <- st_read("dlm/geb03_f.shp")

sie01 <- st_read("dlm/sie01_f.shp")
sie02 <- st_read("dlm/sie02_f.shp")
sie03 <- st_read("dlm/sie03_f.shp")
sie04 <- st_read("dlm/sie04_f.shp")

veg01 <- st_read("dlm/veg01_f.shp")
veg02 <- st_read("dlm/veg02_f.shp")
veg03 <- st_read("dlm/veg03_f.shp")
veg04 <- st_read("dlm/veg04_f.shp")

ver01 <- st_read("dlm/ver01_f.shp")
ver03 <- st_read("dlm/ver03_f.shp")
#ver04 <- st_read("dlm/ver04_f.shp")
ver05 <- st_read("dlm/ver05_f.shp")
ver06 <- st_read("dlm/ver06_f.shp")

#crs(ms)
#crs(gew01)
#class(ms)
ms <- as(ms, "sf")
ms <- st_transform(ms, crs(gew01))

####### zuschneiden ######
#st_agr(gew01) = "identity "
gew01 <- st_intersection(ms,gew01)
#gew02 <- st_intersection(ms,gew02) keine Daten für ms 

geb02 <- st_intersection(ms,geb02)
geb03 <- st_intersection(ms,geb03)

sie01 <- st_intersection(ms,sie01)
sie02 <- st_intersection(ms,sie02)
sie03 <- st_intersection(ms,sie03)
sie04 <- st_intersection(ms,sie04)

veg01 <- st_intersection(ms,veg01)
veg02 <- st_intersection(ms,veg02)
veg03 <- st_intersection(ms,veg03)
veg04 <- st_intersection(ms,veg04)

ver01 <- st_intersection(ms,ver01)
ver03 <- st_intersection(ms,ver03)
#ver04 <- st_intersection(ms,ver04)
ver05 <- st_intersection(ms,ver05)
ver06 <- st_intersection(ms,ver06)
####### ausschreiben ######

st_write(sie02,"dlm_ms.gpkg",append =TRUE, layer = "sie02")
st_write(sie03,"dlm_ms.gpkg",append =TRUE, layer = "sie03")
st_write(sie04,"dlm_ms.gpkg",append =TRUE, layer = "sie04")

st_write(veg01,"dlm_ms.gpkg",append =TRUE, layer = "veg01")
st_write(veg02,"dlm_ms.gpkg",append =TRUE, layer = "veg02")
st_write(veg03,"dlm_ms.gpkg",append =TRUE, layer = "veg03")
st_write(veg04,"dlm_ms.gpkg",append =TRUE, layer = "veg04")

st_write(ver01,"dlm_ms.gpkg",append =TRUE, layer = "ver01")
st_write(ver03,"dlm_ms.gpkg",append =TRUE, layer = "ver03")
st_write(ver05,"dlm_ms.gpkg",append =TRUE, layer = "ver05")
st_write(ver06,"dlm_ms.gpkg",append =TRUE, layer = "ver06")

st_write(gew01,"dlm_ms.gpkg",append =TRUE, layer = "gew01")

st_write(geb02,"dlm_ms.gpkg",append =TRUE, layer = "geb02")
st_write(geb03,"dlm_ms.gpkg",append =TRUE, layer = "geb03")
####### wieder einladen #####

dlm_ms <- readOGR("dlm_ms.gpkg", layer="sie04")
ogrListLayers("dlm_ms.gpkg")
x <- readOGR("dlm_ms.gpkg", layers[14]) 
mapview(dlm_ms)
#mapview(dlm_ms)

?readOGR

######## urban climate zones #####
ucz <- raster("ucz/13322450/EU_LCZ_map.tif")
ms <- st_transform(ms,crs(ucz))
crs(ms)

?crop
mapview(ucz_ms)
eckkoor <- 
  e <- extent(4130000,4180000,
              3180000,3230000)
ucz_ms <- crop(ucz,e)
mapview(ucz_ms)

writeRaster(ucz_ms,"ucz_ms.grd", overwrite = T)


