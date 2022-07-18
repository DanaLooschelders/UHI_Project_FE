#Sky view factor
library(sf)
library(sfheaders)
library(sp)
library(tidyverse)
library(shadow)
#prep data
#setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/test_gml/")
#setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/3d-gm_lod1_kacheln")
#setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/gml/")
setwd("E:/3d-gm_lod2_kacheln")

files<-list.files(pattern=".gml")
files_list<-vector(mode='list', length=length(files))
names(files_list)<-files

for(i in files){
  tryCatch({
    #read in files and set crs
    files_list[[i]]<-read_sf(i, layer="Building",
                             crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    
  }, error=function(e){message("Caught an error")})
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

class(files_list[[1]])
plot(files_list[[2]])

?st_zm
#only do once! 
#remove z dimension
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- st_zm(layer) 
  files_list[[i]] <- layer
}

files_list_backup <- files_list

#####transform to spatial polygon dataframe####
shp<- vector(mode='list', length=length(files_list)) #create empty list
names(shp)<-names(files_list)

#remove empty geometries
i=23
x=3
class(layer)
for (i in seq(files_list)){
  tryCatch(expr={
  print(i) #see where error occurs
  layer <- files_list[[i]]
  #layer$ID <- as.factor(1:length(layer$measuredHeight)) #add ID
  layer <- st_as_sf(layer)
  empty_geometry<-rep(NA, length=nrow(layer))
  for(x in 1:nrow(layer)){ #check every row in layer
    if(class(layer[x,]$geometry)[1]=="sfc_POLYHEDRALSURFACE"){ #check if geometry type is correct
      empty_geometry[x]<-x
    }else{}
    if(is.na(layer[x,]$measuredHeight)){ #check if height is NA
      empty_geometry[x]<-x 
    }else{}
  }
  empty_geometry<-empty_geometry[!is.na(empty_geometry)] #drop NAs
  if(length(empty_geometry)!=0){ #if there are empty geometries
  layer<-layer[-c(empty_geometry),] #drop rows with wrong geometry type
    }
  layer <- st_polygonize(layer)
  if(any(st_is_empty(layer))){ #check if there are still empty geometries
    layer<-layer[!st_is_empty(layer),] #remove empty geometries
  }
  layer <- as(st_geometry(layer), "Spatial") 
  crs(layer) <- NA #delete original crs
  crs(layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" #set new crs
  shp[[i]] <- layer #write into new list
 }, error=function(e){message("WHops! Caught an error")})
}

length(which(sapply(shp, is.null))) #check how many list entries are NULL
#16 entries are NULL
shp[sapply(shp, is.null)] <- NULL #remove NULL entries

spdf<-sapply(shp, function(x) as(x, "SpatialPolygonsDataFrame"))
#create new list
spdf<- vector(mode='list', length=length(shp)) #create empty list
names(spdf)<-names(shp)

names(shp[14])

#tranform to SpatialPolygonsDataFrame
for(i in 1:length(shp)){
  tryCatch(expr={
    print(i)
    if(any(is.na(names(shp[[i]])))){ #check if any IDs are NA
      na_poly<-which(is.na(names(shp[[i]]))) #which ID is NA
      shp[[i]]@polygons[[na_poly]]<-NULL #remove polygon with NA as ID
    }else{}
  spdf[[i]]<-as(shp[[i]], "SpatialPolygonsDataFrame")
  }, error=function(e){message("WHops! Caught an error")})
}

length(which(sapply(spdf, is.null))) #check how many list entries are NULL
#none

#rowbind list of spatialPolygonsdatafarme
obstacles_df <- do.call("rbind", spdf)
#test
plot(obstacles_df[1:100,])

#load height raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar")
svf_raster<-raster("Lidar_building_height.grd")
plot(svf_raster) #check
#set all values to NA
values(svf_raster)<-NA
plot(svf_raster) #check

#check that crs is the same 
crs(svf_raster)
crs(obstacles_df)

####SVF####
#location: Raster* object, specifying the location(s) for which to calculate logical shadow values. Raster* cells are considered as ground location
#obstacles: SpatialPolygonsDataFrame object specifying the obstacles outline
#Name of attribute in obstacles with extrusion height for each feature


SVF(
  location=svf_raster,
  obstacles=obstacles_df,
  obstacles_height_field=,
  res_angle = 5,
  b = 0.01,
  parallel = getOption("mc.cores")
)
