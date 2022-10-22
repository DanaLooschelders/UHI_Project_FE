rm(list=ls() )

library(abind)
library(sf)
library(inlmisc)
library(sf2)
library(stars)
library(multiplex)
library(mapview)
library(shadow)
library(raster)
library(igraph)
library(ggplot2)
library(R.oo)

#load data
#as for loop
setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/gml")

setwd("/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/lod2/3d-gm_lod2_kacheln/")
#list files
files<-list.files(pattern=".gml")
#create empty files list
files_list<-vector(mode='list', length=length(files))
#set names of list to match files
names(files_list)<-files
#loop through all the files
for(i in files){
  tryCatch({
    files_list[[i]]<-read_sf(i, layer="Building")
  })
}

#extract important variables (measuredHeight)
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- layer[,"measuredHeight"]
  files_list[[i]] <- layer
}

#see if it worked
files_list$LoD2_32_395_5755_1_NW.gml$measuredHeight
colnames(files_list) 

plot(files_list$LoD2_32_395_5755_1_NW.gml)
plot(files_list$LoD2_32_401_5753_1_NW.gml)
extent(files_list$LoD2_32_401_5753_1_NW.gml)

#does not work (empty tiles?)
plot(files_list$LoD2_32_407_5754_1_NW.gml)
plot(files_list$LoD2_32_411_5757_1_NW.gml)
plot(files_list$LoD2_32_406_5760_1_NW.gml)
plot(files_list$LoD2_32_402_5762_1_NW.gml)
plot(files_list$LoD2_32_401_5760_1_NW.gml)
plot(files_list$LoD2_32_402_5744_1_NW.gml)

extent(files_list$LoD2_32_402_5762_1_NW.gml)
extent(files_list$LoD2_32_406_5760_1_NW.gml)

mapview(files_list$LoD2_32_401_5753_1_NW.gml)


#delete NAs 
#files_list[sapply(files_list, is.null)] <- NULL
#listraster<- lapply(X = files_list, FUN = st_rasterize)

#test for a tile (first as sp, delete NAs, new coordinate system and rasterize)
test5<-listraster$LoD2_32_395_5756_1_NW.gml #extract one test file
str(test5)
class(test5)
#with stars package
library(stars)
## Loading required package: abind
#read in file and set crs
test4<-read_sf("LoD2_32_395_5755_1_NW.gml", 
               layer="Building",
               crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
crs(test4)
plot(test4["measuredHeight"], axes=T)

#rasterize to stars raster with pixel size of 10
test5<-st_rasterize(test4["measuredHeight"], dx=10, dy=10)
test5<-st_rasterize(test4["measuredHeight"], 
                    st_as_stars(st_bbox(test4)),
                    dx=10, dy=10)
plot(test5, axes=T)

#convert to sp obejct
test6<-as(test5, "Spatial",) 
class(test6)
writeRaster(test6, "testraster")
plot(test6, axes=T)
mapview(test6)

res(test6)

str(test5)
class(test5)

test5<-st_transform(x =test5, crs =  "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
?read_sf
nc$dens = nc$BIR79 / units::set_units(st_area(nc), km^2)
(nc.st = st_rasterize(nc["dens"], dx = 5000, dy = 5000))

test5 <- as(listraster$LoD2_32_395_5756_1_NW.gml, "Spatial")
crs(test5) <- NA
crs(test5) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
mapview(test5)

#test for a tile (first as sp, delete NAs, new coordinate system and rasterize)
test9 <- as(listraster$LoD2_32_395_5756_1_NW.gml, "Spatial")
crs(test5) <- NA
crs(test5) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
mapview(test5)



# rasterize does not work (sf object)
test5raster<- rasterize(test5)
test5
test7<- Grid2Polygons(test5)
test7raster<- st_rasterize(test7)
plot(test7)
test7




#  for all   #######################################################################
#load shape of münster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm <- as(gadm,"sf")

#build empty raster 
e <- extent(395103.5,415705.1,5744177, 5768658)
projection <- crs(gadm)
r <- raster(e,
            crs = projection)
res(r) <- 10


# loop (first as sp, delete NAs, new coordinate system and rasterize)
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- as(layer, "Spatial")
  crs(layer) <- NA
  crs(layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
  files_list[[i]] <- layer
}


#### test 
for (i in seq(files_list)){
  x[[i]] <- rasterize(files_list[[i]], r)
}




