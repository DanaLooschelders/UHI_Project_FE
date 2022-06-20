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

#only do once! 
#remove z dimension
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- st_zm(layer) 
  files_list[[i]] <- layer
}

files_list_backup <- files_list



#test mit einer datei
test<- files_list$LoD2_32_396_5755_1_NW.gml
test<-files_list$LoD2_32_413_5762_1_NW.gml
sf <- st_as_sf(test) 
sf_polygons <- st_polygonize(sf)
shp<- as(sf_polygons, "Spatial")

shp<- vector(mode='list', length=length(files_list)) #create empty list
names(shp)<-names(files_list)

for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer_na <- layer %>% filter( is.na(st_dimension(.)) == FALSE )
  files_list[[i]] <- layer_na
}

i=6
for (i in seq(files_list)[1:10]){
  layer <- files_list[[i]]
  layer <- st_as_sf(layer)
  layer <- as(st_geometry(layer), "Spatial")
  layer <- st_polygonize(layer)
  crs(layer) <- NA 
  crs(layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
  shp[[i]] <- layer
}

files_list$LoD2_32_396_5756_1_NW.gml
test_geo <- files_list$LoD2_32_396_5757_1_NW.gml
class(test_geo)
test_empty <- test_geo %>% filter( is.na(st_dimension(.)) == FALSE )

#SVF(location = r, )
gitcreds_set(url = "https://github.com/DanaLooschelders/UHI_Project_FE")
gitcreds::gitcreds_set()
usethis::git_sitrep()
usethis::use_git_config(user.name = "ameliewe")
credentials::set_github_pat("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")

#crs(layer) <- NA
#crs(layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"