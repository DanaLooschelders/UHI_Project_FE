#stack all static predictors 

library(raster)
library(rgdal)
library(mapview)
library(RStoolbox)
library(sf)
library(dplyr)
library(sp)
library(stars)
library(CAST)
library(caret)
library(latticeExtra)
library(terra)

#load all raster stacks 
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/")
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
albedo_ndvi_06 <- stack("Albedo/albedo_ndvi_final_06.tif")
albedo_ndvi_07 <- stack("Albedo/albedo_ndvi_final_07.tif")

imperv <- ("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness/")
list.files(imperv)
imperv<- stack("Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_3x3_MS_10m.tif",
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_5x5_MS_10m.tif" ,
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_crop_MS_10m.tif")

treecov<- "C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Treecover"

list.files(treecov)
setwd(treecov)
treecov <- stack( "copernicus_tree_cover_3x3_MS_10m.tif",     
                  "copernicus_tree_cover_5x5_MS_10m.tif",    
                 "copernicus_tree_cover_crop_MS_10m.tif")


water_wetness <- ("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Water_Wetness")
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
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar")
lidar <- "C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/lidar"
list.files(lidar)
lidar <- stack("Lidar_building_height.grd",
               "Lidar_building_sd_3x3.grd",
               "Lidar_building_sd_5x5.grd")

names(lidar) <- c("building_height", "building_height_sd_3x3", "building_height_sd_5x5")
lidar_crs <- projectRaster(lidar,crs = "+proj=longlat +datum=WGS84 +no_defs",
                           method = "ngb" ,r)

pred_stack_06 <- stack(all_static_pred_06, lidar_crs)
pred_stack_07 <- stack(all_static_pred_07, lidar_crs)

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
writeRaster(pred_stack_06, "pred_stack_06.tif", bylayer=F, format="raster", overwrite=T)
writeRaster(pred_stack_07, "pred_stack_07.tif", bylayer=F, format="raster", overwrite=T)
#load stacks
#setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
#pred_stack_06<-stack("all_static_pred_06.grd")
#pred_stack_07<-stack("all_static_pred_07.grd")

load("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/SpatialPoints_Temp_Data")
#load("/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/SpatialPoints_Temp_Data")

times_06 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Time_of_day/times_06.csv")
times_07 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Time_of_day/times_07.csv")

times_06$date <- as.POSIXct(times_06$date,"%Y-%m-%d %H:%M:%S")
times_06$date_time <- as.POSIXct(paste(times_06$date, times_06$time), format="%Y-%m-%d %H:%M:%S")

times_07$date <- as.POSIXct(times_07$date,"%Y-%m-%d %H:%M:%S")
times_07$date_time <- as.POSIXct(paste(times_07$date, times_07$time), format="%Y-%m-%d %H:%M:%S")
  
remove(total_stack)
remove(total_stack_temp)

meteo$date_time<-all_temp$datetime

#load dynamic predictors 
for(i in 1:222){
  if(!exists("total_stack")){
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #load meteo raster stack
    meteo<-stack(paste("E:/meteo_raster/", "Meteo__", i, ".grd", sep=""))
    #meteo<-stack(paste("/Users/ameliewendiggensen/sciebo/UHI_Meteo_Raster/", "Meteo__", i, ".grd", sep=""))
    #resample meteo
    #meteo<-resample(meteo, pred_stack_06, method="ngb")
    #stack pred_stack and meteo_stack
    pred_stack_all <- stack(pred_stack_06, meteo)
    #extract predictor values for training data
    extr <- raster::extract(pred_stack_all,logger_dat,df=TRUE) 
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- extr$time
    extr_sun <-  left_join(extr, times_06[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
    #rename
    total_stack<-extr_sun
  }
  else{
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #load meteo raster stack
    meteo<-stack(paste("E:/meteo_raster/", "Meteo__", i, ".grd", sep=""))
     #meteo<-stack(paste("/Users/ameliewendiggensen/sciebo/UHI_Meteo_Raster/", "Meteo__", i, ".grd", sep=""))
    #stack pred_stack and meteo_stack
    pred_stack_all <- stack(pred_stack_06, meteo)
    #extract predictor values for training gdata
    extr <- raster::extract(pred_stack_all,logger_dat,df=T)  
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- extr$time
    extr_sun <-  left_join(extr, times_06[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
     #rename
    total_stack_temp<-extr_sun

    total_stack<-rbind(total_stack, total_stack_temp)
  }
} 

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/")
write.csv(total_stack, file ="total_stack_06_new.csv")

remove(total_stack)

#same for july
for(i in 223:length((spatial_list))){  
  if(!exists("total_stack")){
    logger_dat<-spatial_list[[i]]
    meteo<-stack(paste("E:/meteo_raster/", "Meteo__", i, ".grd", sep=""))
    pred_stack_all <- stack(pred_stack_07, meteo)
    extr <- raster::extract(pred_stack_all,logger_dat,df=TRUE) 
    logger_dat$ID<-row.names(logger_dat)
    extr <- merge(extr,logger_dat@data,by.x="ID")
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- extr$time
    extr_sun <-  left_join(extr, times_07[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
    total_stack <-extr_sun
  }
  else{
    logger_dat<-spatial_list[[i]]
    meteo<-stack(paste("E:/meteo_raster/", "Meteo__", i, ".grd", sep=""))
    pred_stack_all<- stack(pred_stack_07, meteo)
    extr <- raster::extract(pred_stack_all,logger_dat,df=T)  
    logger_dat$ID<-row.names(logger_dat)
    extr <- merge(extr,logger_dat@data,by.x="ID")
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- extr$time
    extr_sun <-  left_join(extr, times_07[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
    total_stack_temp <-extr_sun
    
    total_stack<-rbind(total_stack, total_stack_temp)
  }
}  

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/")
write.csv(total_stack, file ="total_stack_07_new.csv")

total_stack_06 <- read_csv("total_stack_06_new.csv")
total_stack_07 <- read_csv("total_stack_07_new.csv") 

names(total_stack_07)[names(total_stack_07) == 'albedo_07'] <- 'albedo'
names(total_stack_07)[names(total_stack_07) == 'ndvi_07'] <- 'ndvi'

names(total_stack_06)[names(total_stack_06) == 'albedo_06'] <- 'albedo'
names(total_stack_06)[names(total_stack_06) == 'ndvi_06'] <- 'ndvi'
  
total <- rbind(total_stack_06, total_stack_07)
total <- total[,2:28]

write.csv(total, file ="total_stack_new.csv")

total_stack_new<-read.csv("total_stack_new.csv")  

#cheat for stability
for(i in 1:length(total_stack_new$meteo_stability)){
  total_stack_new$meteo_stability[i]<-meteo$meteo_stability[meteo$date_time==total_stack_new$date_time[i]]
}

write.csv(total_stack_new, file ="total_stack_new.csv")
