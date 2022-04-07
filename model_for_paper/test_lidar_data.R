setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/Lidar/testdaten_3d-gm-lod2_citygml/")
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(parallel)
#install.packages("multiplex")
library(multiplex)
library(tidyverse)
#TEST LoD1 Data
#install.packages("fields")
#read in data
#test<-readOGR("testdaten_3d-gm-lod1_citygml.gml",layer = "Building")
lod1<-read_sf("testdaten_3d-gm-lod1_citygml/testdaten_3d-gm-lod1_citygml.gml",layer = "Building")
#plot
plot(lod1, max.plot=28)
??auc
str(lod1$geometry)
ggplot(data=lod1)+
  geom_sf(aes(color=measuredHeight))
#TEST LoD2 Data
lod2<-read_sf("testdaten_3d-gm-lod2_citygml.gml",layer = "Building")

plot(lod2)

ggplot(data=lod2)+
  geom_sf(aes(color=measuredHeight))
rasterize(lod2)

#plot geometry of buildings
str(lod2$geometry)

class(lod2)

ggplot(data=lod2$geometry)+
  geom_sf()
install.packages("fasterize")
library(fasterize)
#convert to polygon
#test1<-st_multipolygon(lod2$geometry)

#to polygon
#test2<-st_polygonize(lod2$geometry)
lod2$geometry<-st_polygonize(lod2$geometry)
lod2$geometry<-st_make_valid(lod2$geometry)
lod2$geometry <- lod2$geometry %>% filter(!st_is_empty(.))
st_cast(x=lod2$geometry, to="MULTIPOLYGON")

class(test2)

class(lod2$geometry[["lod2"]])
extent(lod2)
ras<-raster()
extent(ras)<-extent(lod2)
res(ras)<-c(10,10) 
res(ras)
fasterize(sf = lod2, raster =ras, field = "measuredHeight", 
          background=0)
?rasterize

#plot
plot(test2)
#to shp
shp_test <- as(testne, "Spatial") #does not work
#make valid
test3<-st_make_valid(test2)
#to shp
shp_test <- as(test3, "Spatial")#still does not work

#remove empty polygons
testne = test3[!st_is_empty(test3),,drop=FALSE]
testne <- test3 %>% filter(!st_is_empty(.))
class(testne)

#to shp
shp_test1 <- as(object = testne, Class = "Spatial") #

SpatialPolygonsDataFrame(testne)
shp_test2<-spTransform(testne)

#calculate Sky View factor with shadow
#install.packages("shadow")
library(shadow)
#location: spatial points
#obstacles spatial polygonsdataframe
#obstacles height field
#res angle: default -> 5
#parallel
svf_test<-shadow::SVF(location = shp_test1,
            obstacles = shp_test1,
            obstacles_height_field = shp_test1$measuredHeight)

#Test raw Lidar Data
#install.packages("lidR")
library(lidR)
setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/Lidar")
lidar<-readLAS(files = "testdaten_las_3dm/3dm_32_480_5722_1_nw.laz")
#plot(lidar)
lidarCAT = readLAScatalog(folder = "dana/dana/")
plot(lidarCAT, chunk=T)
#rasterize terrain
test<-rasterize_terrain(
  las=lidarCAT,
  res = 1,
  algorithm = tin(),
  use_class = c(2L, 9L),
  shape = "convex")

plot(test)
#rasterize canopy
test_canopy<-rasterize_canopy(las = lidarCAT,
                 res=1,
                  algorithm = p2r())

plot(test_canopy)

