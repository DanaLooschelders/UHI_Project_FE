library(raster)
library(mapview)
library(sp)
library(sf) 
library(rgdal)
library(sen2r)

library(remotes)
install_github("ranghetti/sen2r", force =TRUE)

setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/NDVI/")
setwd("/Volumes/work/l2_dat/")

### load shape of münster ####
gadm <- getData('GADM',country='DEU', level =2) 
extent <- gadm[gadm$NAME_2 == "Münster",]
extent <- as(extent, "sf")
extent <- st_transform(extent, crs ="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs ")
st_write(extent,"extent.shp",append=TRUE)

### download sentinel scenes ####
write_scihub_login("*******", "*******")

#level 1 dat
sen2020 <- sen2r(s2_levels = "l1c",
                  overwrite_safe = F,
                  max_cloud_safe = 20,
                  timewindow = as.Date(c("2020-06-01", "2020-06-23")),
                  timeperiod  = "full",
                  #extent = "extent.shp", #had to add it manually in console
                  extent_name = "ms",
                  s2tiles_selected = c("32ULC","32UMC"),
                  clip_on_extent = T,
                  res_s2 = "10m",
                  path_tiles = "2020_1/tiles",
                  path_out = "2020_1/out",
                  path_l1c = "2020_1/1C",
                  path_l2a ="2020_1/2A")

#from the fifth until the 19th only data with more than 40% cloud cover available
#therefore time interval was extended 

#load preoprocced data 
sen2020_06_01 <- stack("2020_1/out/TOA/S2B1C_20200601_008_ms_TOA_10.tif")
sen2020_06_23 <- stack("2020_1/out/TOA/S2A1C_20200623_108_ms_TOA_10.tif")

#calculate ndvi 
ndvi2020_06_01 <- ((sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.8 - sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.4)
                  /(sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.8 + sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.4))
ndvi2020_06_23 <- ((sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.8 - sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.4)
                   /(sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.8 + sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.4))

#check difference in ndvi 
dif <- (ndvi2020_06_01 - ndvi2020_06_23)
mapview(dif)

#calculate mean 
ndvi2020_06 <- mean(ndvi2020_06_01,ndvi2020_06_23)
writeRaster(ndvi2020_06, "ndvi_ms_final_2020_06.tif")

#second time span in july 
sen2020_2 <- sen2r(s2_levels = "l1c",
                 overwrite_safe = F,
                 max_cloud_safe = 20,
                 timewindow = as.Date(c("2020-07-03", "2020-07-31")),
                 timeperiod  = "full",
                 #extent = "extent.shp",
                 extent_name = "ms",
                 s2tiles_selected = c("32ULC","32UMC"),
                 clip_on_extent = T,
                 res_s2 = "10m",
                 path_tiles = "2020_2/tiles",
                 path_out = "2020_2/out",
                 path_l1c = "2020_2/1C",
                 path_l2a ="2020_2/2A")

sen2020_07_13 <- stack("2020_2/out/TOA/S2A1C_20200713_108_ms_TOA_10.tif") #cloudy
sen2020_07_23 <- stack("2020_2/out/TOA/S2A1C_20200723_108_ms_TOA_10.tif") #cloudy
sen2020_07_31 <- stack("2020_2/out/TOA/S2B1C_20200731_008_ms_TOA_10.tif")

ndvi2020_07 <- ((sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.8 - sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.4)
                /(sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.8 + sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.4))

writeRaster(ndvi2020_07, "ndvi_ms_final_2020_07.tif")

#download level 2 data 
sen2020 <- sen2r(s2_levels = "l2a",
                 overwrite_safe = F,
                 max_cloud_safe = 20,
                 timewindow = as.Date(c("2020-06-01", "2020-06-23")),
                 timeperiod  = "full",
                 #extent = "/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/ndvi/extent.shp",
                 extent_name = "ms",
                 s2tiles_selected = c("32ULC","32UMC"), 
                 clip_on_extent = T,
                 res_s2 = "10m",
                 path_tiles = "l2_dat/tiles",
                 path_merged = "l2_dat/out",
                 path_out = "l2_dat/out",
                 path_l2a ="l2_dat/2A")

boa_01_06 <- stack("/Volumes/work/l2_dat/out/BOA/S2B2A_20200601_008_ms_BOA_10.tif")

sen2020_07 <- sen2r(s2_levels = "l2a",
                 overwrite_safe = F,
                 max_cloud_safe = 20,
                 timewindow = as.Date(c("2020-07-03", "2020-07-31")),
                 timeperiod  = "full",
                 #extent = "/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/ndvi/extent.shp",
                 extent_name = "ms",
                 s2tiles_selected = c("32ULC","32UMC"), 
                 clip_on_extent = T,
                 res_s2 = "10m",
                 path_tiles = "l2_dat/tiles",
                 path_merged = "l2_dat/out",
                 path_out = "l2_dat/out",
                 path_l2a ="l2_dat/2A")

