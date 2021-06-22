rm(list=ls() )
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_LUC")

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

sie02 <- readOGR("dlm_ms.gpkg", layer = "sie02")
sie03 <- readOGR("dlm_ms.gpkg", layer = "sie03")
sie04 <- readOGR("dlm_ms.gpkg", layer = "sie04")
veg01 <- readOGR("dlm_ms.gpkg", layer = "veg01")
veg02 <- readOGR("dlm_ms.gpkg", layer = "veg02")
veg03 <- readOGR("dlm_ms.gpkg", layer = "veg03")
veg04 <- readOGR("dlm_ms.gpkg", layer = "veg04")
ver01 <- readOGR("dlm_ms.gpkg", layer = "ver01")
ver03 <- readOGR("dlm_ms.gpkg", layer = "ver03")
ver05 <- readOGR("dlm_ms.gpkg", layer = "ver05")
ver06 <- readOGR("dlm_ms.gpkg", layer = "ver06")
gew01 <- readOGR("dlm_ms.gpkg", layer = "gew01")
geb02 <- readOGR("dlm_ms.gpkg", layer = "geb02")
geb03 <- readOGR("dlm_ms.gpkg", layer = "geb03")

sie02<- sie02[,names(sie02)%in%c("OBJART","OBJART_TXT")]
sie03<- sie03[,names(sie03)%in%c("OBJART","OBJART_TXT")]
sie04<- sie04[,names(sie04)%in%c("OBJART","OBJART_TXT")]
veg01<- veg01[,names(veg01)%in%c("OBJART","OBJART_TXT")]
veg02<- veg02[,names(veg02)%in%c("OBJART","OBJART_TXT")]
veg03<- veg03[,names(veg03)%in%c("OBJART","OBJART_TXT")]
veg04<- veg04[,names(veg04)%in%c("OBJART","OBJART_TXT")]
ver01<- ver01[,names(ver01)%in%c("OBJART","OBJART_TXT")]
ver03<- ver03[,names(ver03)%in%c("OBJART","OBJART_TXT")]
ver05<- ver05[,names(ver05)%in%c("OBJART","OBJART_TXT")]
ver06<- ver06[,names(ver06)%in%c("OBJART","OBJART_TXT")]
gew01<- gew01[,names(gew01)%in%c("OBJART","OBJART_TXT")]
geb02<- geb02[,names(geb02)%in%c("OBJART","OBJART_TXT")]
geb03<- geb03[,names(geb03)%in%c("OBJART","OBJART_TXT")]

dlm_ms <- rbind(sie02, sie03,sie04, 
                veg01, veg02, veg03,veg04,
                ver01,ver03,ver05,ver06, 
                gew01,geb02,geb03,  makeUniqueIDs = TRUE) 

st_write(dlm_ms,"dlm_ms_all.shp")
writeOGR(obj=dlm_ms, dsn="dlm_ms_all",layer = "OBJART", driver="GPKG")


#xmn=395103.5,xmx= 415705.1, ymn =5744177, ymx=5768658


