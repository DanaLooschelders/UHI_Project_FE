rm(list=ls() )
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_LUC")

library(sp)
library(sf) 
library(mapview)
library(raster)
library(rgdal)

dlm_ms_all<- read_sf("dlm_ms_all")
mapview(dlm_ms_all)

unique(dlm_ms_all$OBJART)
unique(dlm_ms_all$Art)
dlm_ms_all$PolyID <- 1:nrow(dlm_ms_all)

e <- extent(395103.5,415705.1,5744177, 5768658)
projection <- crs( "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
r <- raster(e,
            crs = projection)
res(r) <- 100
dlm_raster <- rasterize(dlm_ms_all, r,field=dlm_ms_all$PolyID, fun='count' ,getCover = F)
mapview(dlm_raster)
