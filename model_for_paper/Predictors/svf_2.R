setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")

library(raster)
library(rgdal)
library(mapview)
library(RStoolbox)
library(sf)
library(dplyr)
library(sp)
library(stars)
#install.packages("shadow")
library(shadow)

?SVF

building_height <- raster("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar/Lidar_building_height.grd")
mapview(building_height)
building_height

setwd("/Users/ameliewendiggensen/Desktop/3d-gm_lod2_kacheln")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/test_gml/")
files<-list.files(pattern=".gml")
files_list<-vector(mode='list', length=length(files))
names(files_list)<-files

for(i in files){
  tryCatch({
    #read in files and set crs
    files_list[[i]]<-read_sf(i, layer="Building",
                             crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    
  })
}

length(Filter(is.null, files_list))
index_NULL_files<-names(Filter(is.null, files_list))

for (i in index_NULL_files){
  tryCatch(expr={
    #read in files and set crs
    files_list[[i]]<-read_sf(i,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" )
  }, error=function(e){message("Caught an error")})
}

length(Filter(is.null, files_list))
files_list<-files_list[vapply(files_list, Negate(is.null), NA)]

for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- layer[,"measuredHeight"]
  files_list[[i]] <- layer
}

files_list[15] 

#only do once! 
#remove z dimension
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- st_zm(layer) 
  files_list[[i]] <- layer
}

files_list_zm <- files_list
files_list_zm[1:3]  


#test mit einer datei
test<- files_list$LoD2_32_396_5755_1_NW.gml
test<-files_list$LoD2_32_413_5762_1_NW.gml
sf <- st_as_sf(test) 
sf_polygons <- st_polygonize(sf)
shp<- as(sf_polygons, "Spatial")

#shp <- NA #klappt nicht -> "long vectors are not suppported 

shp<- vector(mode='list', length=length(files_list)) #create empty list
names(shp)<-names(files_list)

#sonst mit files_list[[i]]<- layer macht er das auch manchmal nur mit dem ersten... 
for (i in length(files_list)){
  layer <- files_list[[i]]
  layer <- st_as_sf(layer)
  layer <- st_polygonize(layer)
  layer <- as(layer, "Spatial")
  shp[[i]] <- layer
}

files_list$LoD2_32_395_5755_1_NW.gml


#SVF(location = r, )

#crs(layer) <- NA
#crs(layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
