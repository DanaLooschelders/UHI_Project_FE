library(raster)
library(mapview)
library(sen2r)
library(sp)
library(sf) 
library(rgdal)

setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/NDVI/")

### load shape of münster ####
gadm <- getData('GADM',country='DEU', level =2) 
extent <- gadm[gadm$NAME_2 == "Münster",]
extent <- as(extent, "sf")
extent <- st_transform(extent, crs ="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs ")
st_write(extent,"extent.shp",append=TRUE)

#extent <- system.file("extent.geojson", package ="sen2r")

### load sentinel data ####
write_scihub_login("ameliewe", "Apfelmus.1")

#### 2019 sen2r ######
test2019 <- sen2r(s2_levels = "l1c",
                  overwrite_safe = F,
                  max_cloud_safe = 20,
                  timewindow = as.Date(c("2019-08-20", "2019-09-30")),
                  timeperiod  = "full",
                  extent = extent,
                  extent_name = "ms",
                  s2tiles_selected = c("32ULC","32UMC"),
                 #list_indices = "NDVI",
                  clip_on_extent = T,
                  res_s2 = "10m",
                  path_tiles = "2019/tiles",
                  path_out = "2019/out",
                  path_l1c = "2019/1C",
                  path_l2a ="2019/2A")
 
sen_2019_08_31 <- stack("2019/out/TOA/S2A1C_20190831_008_ms_TOA_10.tif")
sen_2019_09_20 <- stack("2019/out/TOA/S2A1C_20190920_008_ms_TOA_10.tif")
sen_2019_08_23 <- stack("2019/out/TOA/S2B1C_20190823_108_ms_TOA_10.tif")
sen_2019_08_26 <- stack("2019/out/TOA/S2B1C_20190826_008_ms_TOA_10.tif")

ndvi_2019_08_31 <- ((sen_2019_08_31$S2A1C_20190831_008_ms_TOA_10.8-sen_2019_08_31$S2A1C_20190831_008_ms_TOA_10.4)
                    /(sen_2019_08_31$S2A1C_20190831_008_ms_TOA_10.8+sen_2019_08_31$S2A1C_20190831_008_ms_TOA_10.4))

ndvi_2019_09_20 <- ((sen_2019_09_20$S2A1C_20190920_008_ms_TOA_10.8-sen_2019_09_20$S2A1C_20190920_008_ms_TOA_10.4)
                    /(sen_2019_09_20$S2A1C_20190920_008_ms_TOA_10.8+sen_2019_09_20$S2A1C_20190920_008_ms_TOA_10.4))

ndvi_2019_08_23 <- ((sen_2019_08_23$S2B1C_20190823_108_ms_TOA_10.8 - sen_2019_08_23$S2B1C_20190823_108_ms_TOA_10.4)
                    /(sen_2019_08_23$S2B1C_20190823_108_ms_TOA_10.8 + sen_2019_08_23$S2B1C_20190823_108_ms_TOA_10.4))

dif <- ndvi_2019_09_20 - ndvi_2019_08_31
summary(dif)
mapview(dif)

dif23_31 <- ndvi_2019_08_31 - ndvi_2019_08_23
summary(dif23_31)
mapview(dif23_31)

### #2020_1 #####

sen2020 <- sen2r(s2_levels = "l1c",
                  overwrite_safe = F,
                  max_cloud_safe = 20,
                  timewindow = as.Date(c("2020-06-01", "2020-06-23")),
                  timeperiod  = "full",
                  #extent = "extent.shp",
                  extent_name = "ms",
                  s2tiles_selected = c("32ULC","32UMC"),
                  clip_on_extent = T,
                  res_s2 = "10m",
                  path_tiles = "2020_1/tiles",
                  path_out = "2020_1/out",
                  path_l1c = "2020_1/1C",
                  path_l2a ="2020_1/2A")

#from the fifth until the 19th only data with more than 40% cloud cover available
#therefore time interval extended 

sen2020_06_01 <- stack("2020_1/out/TOA/S2B1C_20200601_008_ms_TOA_10.tif")
sen2020_06_23 <- stack("2020_1/out/TOA/S2A1C_20200623_108_ms_TOA_10.tif")

ndvi2020_06_01 <- ((sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.8 - sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.4)
                  /(sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.8 + sen2020_06_01$S2B1C_20200601_008_ms_TOA_10.4))
ndvi2020_06_23 <- ((sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.8 - sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.4)
                   /(sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.8 + sen2020_06_23$S2A1C_20200623_108_ms_TOA_10.4))
dif <- (ndvi2020_06_01 - ndvi2020_06_23)
mapview(dif)

ndvi2020_06 <- mean(ndvi2020_06_01,ndvi2020_06_23)
writeRaster(ndvi2020_06, "ndvi_ms_final_2020_06.tif")

### 2020_2 ####
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

sen2020_07_13 <- stack("2020_2/out/TOA/S2A1C_20200713_108_ms_TOA_10.tif")
sen2020_07_23 <- stack("2020_2/out/TOA/S2A1C_20200723_108_ms_TOA_10.tif")
sen2020_07_31 <- stack("2020_2/out/TOA/S2B1C_20200731_008_ms_TOA_10.tif")

ndvi2020_07 <- ((sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.8 - sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.4)
                /(sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.8 + sen2020_07_31$S2B1C_20200731_008_ms_TOA_10.4))

writeRaster(ndvi2020_07, "ndvi_ms_final_2020_07.tif")
