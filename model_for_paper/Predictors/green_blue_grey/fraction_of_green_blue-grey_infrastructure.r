library(raster)
library(mapview)
#Percentage in 3x3 and 5x5 pixelumgebung

####Water and Wettness####
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Copernicus_grün_blau_grau/Water_Wetness")
blue<-raster(x = "copernicus_water_wetness_crop_MS_10m.tif")
#(1) permanent water, (2) temporary water, (3) permanent wetness and (4) temporary wetness
mapview(blue)
#calculate fraction of 1 in pixel surrounding
#3x3
blue_3x3<-focal(blue, w=matrix(1, 3,3), fun=function(x) sum(x[x==1])/length(x), na.rm=FALSE)
mapview(blue_3x3)
#write raster
writeRaster(blue_3x3, filename = "copernicus_water_wetness_3x3_MS_10m.tif", 
            overwrite=T)
#5x5
blue_5x5<-focal(blue, w=matrix(1, 5,5), fun=function(x) sum(x[x==1])/length(x), na.rm=FALSE)
mapview(blue_5x5)
#write raster
writeRaster(blue_5x5, filename = "copernicus_water_wetness_5x5_MS_10m.tif", 
            overwrite=T)

####tree cover density####
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Copernicus_grün_blau_grau/Treecover/")
green <- raster(x="copernicus_tree_cover_crop_MS_10m.tif")
#proportional crown coverage per pixel at 10m spatial resolution and ranges from 0% (all non-tree covered areas) to 100%
mapview(green) #check
#3x3
green_3x3<-focal(green, w=matrix(1/9, 3,3), fun=mean, na.rm=FALSE)
mapview(green_3x3)
#write raster
writeRaster(green_3x3, filename = "copernicus_tree_cover_3x3_MS_10m.tif", 
            overwrite=T)
#5x5
green_5x5<-focal(green, w=matrix(1/25, 5,5), fun=mean, na.rm=FALSE)
mapview(green_5x5)
#write raster
writeRaster(green_5x5, filename = "copernicus_tree_cover_5x5_MS_10m.tif", 
            overwrite=T)

####imperviousness density####
#The Imperviousness degree is a thematic product showing the sealing density in the range from 0-100%
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness/")
grey <- raster(x="copernicus_imperviousness_crop_MS_10m.tif")
mapview(grey)
#3x3
grey_3x3<-focal(grey, w=matrix(1/9, 3,3), fun=mean, na.rm=FALSE)
mapview(grey_3x3)
#write into raster
writeRaster(grey_3x3, filename = "copernicus_imperviousness_3x3_MS_10m.tif", 
            overwrite=T)
#5x5
grey_5x5<-focal(grey, w=matrix(1/25, 5,5), fun=mean, na.rm=FALSE)
mapview(grey_5x5)
#write into raster
writeRaster(grey_5x5, filename = "copernicus_imperviousness_5x5_MS_10m.tif", 
            overwrite=T)
