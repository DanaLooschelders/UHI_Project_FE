#Copernicus data
#load librarys
library(sp)
library(raster)
library(cop)
library(copTools)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
#set directory
#for tree cover density
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_Urban_Atlas_Tree_Cover (Copernicus)/40c4f17ce65366ae2881787e2ae2a1d1680f5328/TCD_2018_010m_de_03035_v020/TCD_2018_010m_de_03035_v020/DATA/")
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_Urban_Atlas_Tree_Cover (Copernicus)/40c4f17ce65366ae2881787e2ae2a1d1680f5328/TCD_2018_010m_de_03035_v020/TCD_2018_010m_de_03035_v020/DATA")
#for water and wetness
setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/Water_wetness/WAW_2018_010m_de_03035_v020/DATA/")
#for imperviousness
setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/imperviousness/")


#read in files
files=list.files(pattern = ".tif")
#exclude dbf files
dbffiles = list.files(pattern='.dbf')
files=files[!files %in% dbffiles] 
#import all raster files in folder using lapply
cop <- lapply(files, raster)
#mosiac the two tiles that cover MS
cop_ms=mosaic(cop[[11]], cop[[12]], fun="mean")
mapview(cop_ms) #check
#remove other files
#remove(cop)
#crop to muenster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)+mapview(cop_ms)

#check if crs are matching
crs(gadm) 
crs(cop[[1]])
#transform coordinates of cop coordinate system to gadm

cop_ms_proj=projectRaster(from=cop_ms, crs=crs(gadm)) 

mapview(cop_ms_proj) #check 
crs(cop_ms_proj)

#crop 
cop_ms_crop=crop(cop_ms_proj, gadm_sf)
mapview(cop_ms_crop) +mapview(gadm_sf)

#save cropped but not aggregated file
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Copernicus/Water_Wetness/")
writeRaster(cop_ms_crop, filename = "copernicus_water_wetness_crop_MS_10m.tif", 
            overwrite=T)

#aggregate from 10m resolution to 100m resolution
cop_ms_agg=raster::aggregate(cop_ms_crop, 10, fun =mean)
mapview(cop_ms_agg)

#save file
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Copernicus")
writeRaster(cop_ms_crop, filename = "copernicus_tree_cover_crop_MS.tif", 
            overwrite=T)

setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Copernicus")
writeRaster(cop_ms_agg, filename = "copernicus_tree_cover_MS_100m.tif",overwrite =T)


