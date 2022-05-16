#stack all static predictors 

library(raster)
library(rgdal)
library(mapview)
library(RStoolbox)

#load all raster stacks 
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
albedo_ndvi_06 <- stack("Albedo/albedo_ndvi_final_06.tif")
albedo_ndvi_07 <- stack("Albedo/albedo_ndvi_final_07.tif")

imperv <- ("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness")
list.files(imperv)
imperv<- stack("Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_3x3_MS_10m.tif",
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_5x5_MS_10m.tif" ,
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_crop_MS_10m.tif")
mapview(imperv)

treecov<- "/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Treecover"
list.files(treecov)
setwd(treecov)
treecov <- stack( "copernicus_tree_cover_3x3_MS_10m.tif",     
                  "copernicus_tree_cover_5x5_MS_10m.tif",    
                 "copernicus_tree_cover_crop_MS_10m.tif")


water_wetness <- ("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Water_Wetness")
list.files(water_wetness)
setwd(water_wetness)
water_wetness <- stack("copernicus_water_wetness_3x3_MS_10m.tif",
                      "copernicus_water_wetness_5x5_MS_10m.tif" ,
                       "copernicus_water_wetness_crop_MS_10m.tif")

#load shape of münster
gadm <- getData('GADM',country='DEU', level =2) 
extent <- gadm[gadm$NAME_2 == "Münster",]
extent <- as(extent, "sf")

all <- stack(water_wetness, treecov, imperv)

#create raster with right parameters
r <- raster(xmn = 7.473961, xmx = 7.774256, ymn =51.8402, ymx = 52.06021, 
            ncols= 2071, nrows = 2450)

#rename layer 
names(albedo_ndvi_06) <- c("albedo_06","ndvi_06")
names(albedo_ndvi_07) <- c("albedo_07","ndvi_07") 

#change projection of albedo and ndv raster 
albedo_crs_06 <- projectRaster(albedo_ndvi_06,crs = "+proj=longlat +datum=WGS84 +no_defs",
                           r)
albedo_crs_07 <- projectRaster(albedo_ndvi_07,crs = "+proj=longlat +datum=WGS84 +no_defs",
                            r)

#stack all 
all_06 <- stack(albedo_crs_06, all)
all_07 <- stack(albedo_crs_07, all)

#write raster stack
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")

writeRaster(all_07,filename= "all_static_pred_07.tif", bylayer=F,format="raster")
writeRaster(all_06,filename= "all_static_pred_06.tif", bylayer=F,format="raster")


test <- stack("all_static_pred_06.grd")
mapview(test$copernicus_imperviousness_3x3_MS_10m)
