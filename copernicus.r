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
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_Urban_Atlas_Tree_Cover (Copernicus)/40c4f17ce65366ae2881787e2ae2a1d1680f5328/TCD_2018_010m_de_03035_v020/TCD_2018_010m_de_03035_v020/DATA/")
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_Urban_Atlas_Tree_Cover (Copernicus)/40c4f17ce65366ae2881787e2ae2a1d1680f5328/TCD_2018_010m_de_03035_v020/TCD_2018_010m_de_03035_v020/DATA")
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

#e <- extent(395103.5,415705.1,5744177, 5768658)
#projection <- CRS('+init=EPSG:25832')
#cop_ms_proj <- raster(e,
                     # crs = projection)
#res(cop_ms_proj) <- 100 
#cop_ms_proj=projectRaster(from=cop_ms, to = cop_ms_proj)
mapview(cop_ms_proj) #check 
crs(cop_ms_proj)

#crop 
#Amelie: das geht dann irgendwie nicht mehr, das liegt denke ich an dem crs 
#aber das oben ist (für mich) der einfachste Weg die Auflösung zu öndern und so 
#versucht gerne noch das zu cropen, wenn ihr wollt 
cop_ms_crop=crop(cop_ms_proj, gadm_sf)
mapview(cop_ms_crop)+mapview(gadm_sf)

#save file
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Copernicus")
writeRaster(cop_ms_crop, filename = "copernicus_tree_cover_crop_MS.tif", 
            overwrite=T)
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Copernicus")
writeRaster(cop_ms_proj, filename = "copernicus_tree_cover_MS_100m.tif")

#resample to modis resolution
#cop_ms_agg=raster::aggregate(cop_ms_crop, 10, fun =mean)
