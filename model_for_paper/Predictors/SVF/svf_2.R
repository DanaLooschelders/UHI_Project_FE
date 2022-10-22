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
#setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/test_gml/")
files<-list.files(pattern=".gml")
files_list<-vector(mode='list', length=length(files))
names(files_list)<-files

#load files
for(i in files){
  tryCatch({
    #read in files and set crs
    files_list[[i]]<-read_sf(i, layer="Building",
                             crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    
  })
}

length(Filter(is.null, files_list))
index_NULL_files<-names(Filter(is.null, files_list))

#laod faulty files
for (i in index_NULL_files){
  tryCatch(expr={
    #read in files and set crs
    files_list[[i]]<-read_sf(i,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" )
  }, error=function(e){message("Caught an error")})
} 

length(Filter(is.null, files_list))
files_list<-files_list[vapply(files_list, Negate(is.null), NA)]

#choose important variable
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- layer[,"measuredHeight"]
  files_list[[i]] <- layer
}

#only do once! 
#remove z dimension
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- st_zm(layer) 
  files_list[[i]] <- layer
}

files_list_backup <- files_list

#files_list <- files_list_backup

#test mit einer datei ####
test_5 <- files_list[5]
test_5 <- test_5$LoD2_32_396_5756_1_NW.gml

#remove nas 
test_5_na <- test_5[!sf::st_is_empty(test_5), ] %>% na.omit() #unknown wkb type 15
test_5_na <- test_5 %>% filter( is.na(st_dimension(.)) == FALSE ) #klappt 

#into spdf 
sf_5 <- st_as_sf(test_5_na) 
sf_polygons_5 <- st_polygonize(sf_5)
shp_test <- as(sf_polygons_5, "Spatial")

#test mit einer Problemdatei 23 ####
test_23 <- files_list[23]
test_23 <- test_23$LoD2_32_398_5752_1_NW.gml
test_23_na <- test_23[!sf::st_is_empty(test_23), ] %>% na.omit() #unknown wkb type 15
sf <- st_as_sf(test_23) 
sf_polygons <- st_polygonize(sf)
shp_test <- as(sf_polygons, "Spatial")

#### mit allen #### 
shp<- vector(mode='list', length=length(files_list)) #create empty list
names(shp)<-names(files_list)

for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer_na <- layer[!sf::st_is_empty(layer), ] %>% na.omit()
  files_list[[i]] <- layer_na
}

#or second way to remove NAs
i=23
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer_na <-  layer %>% filter( is.na(st_dimension(.)) == FALSE )
  files_list[[i]] <- layer_na
}

i= 21
remove(i)
for (i in seq(files_list)[1:23]){
  layer <- files_list[[i]]
  layer$ID <- 1:length(layer$measuredHeight)
  layer <- st_as_sf(layer)
  layer <- st_polygonize(layer)
  layer <- as(st_geometry(layer), "Spatial") 
 # crs(layer) <- NA 
#  crs(layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
  shp[[i]] <- layer
}

i = seq(1:25)

test_22 <- files_list[22]
test_22 <- test_22$LoD2_32_398_5751_1_NW.gml
test_23 <-(files_list)[23]
test_23 <- test_23$LoD2_32_398_5752_1_NW.gml

#shp_funkt <- shp[1:22]
#r <- raster(xmn = 7.473961, xmx = 7.774256, ymn =51.8402, ymx = 52.06021, 
#            ncols= 2071, nrows = 2450, crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs ")

