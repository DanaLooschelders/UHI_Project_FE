#stack all static predictors 

library(raster)
library(rgdal)
library(mapview)
library(RStoolbox)
library(sf)

library(sp)
library(stars)
library(CAST)
library(caret)
library(latticeExtra)
library(terra)

#load all raster stacks 
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
albedo_ndvi_06 <- stack("Albedo/albedo_ndvi_final_06.tif")
albedo_ndvi_07 <- stack("Albedo/albedo_ndvi_final_07.tif")

imperv <- ("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness")
list.files(imperv)
imperv<- stack("Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_3x3_MS_10m.tif",
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_5x5_MS_10m.tif" ,
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_crop_MS_10m.tif")

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
                               method = "ngb",r)
albedo_crs_07 <- projectRaster(albedo_ndvi_07,crs = "+proj=longlat +datum=WGS84 +no_defs",
                               method = "ngb",r)

#stack all static
all_static_pred_06 <- stack(albedo_crs_06, all)
all_static_pred_07 <- stack(albedo_crs_07, all)

#write raster stack
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
#writeRaster(all_07,filename= "all_static_pred_07.tif", bylayer=F,format="raster",overwrite=T)
#writeRaster(all_06,filename= "all_static_pred_06.tif", bylayer=F,format="raster",overwrite=T)

#stack with lidar data 
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar")
lidar <- "/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar"
list.files(lidar)
lidar <- stack("Lidar_building_height.grd",
               "Lidar_building_sd_3x3.grd",
               "Lidar_building_sd_5x5.grd")

names(lidar) <- c("building_height", "building_height_sd_3x3", "building_height_sd_5x5")
lidar_crs <- projectRaster(lidar,crs = "+proj=longlat +datum=WGS84 +no_defs",
                           method = "ngb" ,r)

pred_stack_06 <- stack(all_static_pred_06, lidar_crs)
pred_stack_07 <- stack(all_static_pred_07, lidar_crs)

load("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/SpatialPoints_Temp_Data")
 
#load("/Users/ameliewendiggensen/Desktop/SpatialPoints_Temp_Data")
#alte_sp_list <- spatial_list

#######
test<- raster::extract(pred_stack_all,logger_dat,df=TRUE) #problemmooo 
test<- extract(pred_stack_all,logger_dat,df=TRUE) #problemmooo 

#load dynamic predictors 
for(i in 1:length(spatial_list)){   
  if(!exists("total_stack")){
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #load meteo raster stack
    meteo<-stack(paste("/Users/ameliewendiggensen/sciebo/UHI_Meteo_Raster/", "Meteo__", i, ".grd", sep=""))
    #resample meteo
    meteo<-resample(meteo, pred_stack_06, method="ngb")
    #stack pred_stack and meteo_stack
    pred_stack_all <- stack(pred_stack_06, meteo)
    #extract predictor values for training data
    extr <- raster::extract(pred_stack_all,logger_dat,df=TRUE) #problemmooo 
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    #rename
    total_stack<-extr
  }
  else{
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #load meteo raster stack
    meteo<-stack(paste("/Users/ameliewendiggensen/sciebo/UHI_Meteo_Raster/", "Meteo__", i, ".grd", sep=""))
    #resample meteo
    meteo<-resample(meteo, pred_stack_06, method="ngb")
    #stack pred_stack and meteo_stack
    pred_stack_all <- stack(pred_stack_06, meteo)
    #extract predictor values for training gdata
    extr <- raster::extract(pred_stack_all,logger_dat,df=T) #problemmooo 
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    #rename
    total_stack_temp<-extr

    total_stack<-rbind(total_stack, total_stack_temp)
  }
} 



test <- str(total_stack$time)
total_stack$time<-as.POSIXct(total_stack$time)
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
write.csv(total_stack, file="total_stack_vielleicht.csv")

        