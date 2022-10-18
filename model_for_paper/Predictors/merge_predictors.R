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

#create raster with right parameters
r <- raster(xmn = 7.473961, xmx = 7.774256, ymn =51.8402, ymx = 52.06021, 
            ncols= 2071, nrows = 2450)

#load all raster stacks 
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/")
#setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
albedo_ndvi_06 <- stack("Albedo/albedo_ndvi_final_06.tif")
albedo_ndvi_07 <- stack("Albedo/albedo_ndvi_final_07.tif")

imperv <- ("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Copernicus_grün_blau_grau/Imperviousness/")
list.files(imperv)
imperv<- stack("Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_3x3_MS_10m.tif",
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_5x5_MS_10m.tif" ,
               "Copernicus_grün_blau_grau/Imperviousness/copernicus_imperviousness_crop_MS_10m.tif")

treecov<- "C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Copernicus_grün_blau_grau/Treecover"

list.files(treecov)
setwd(treecov)
treecov <- stack( "copernicus_tree_cover_3x3_MS_10m.tif",     
                  "copernicus_tree_cover_5x5_MS_10m.tif",    
                 "copernicus_tree_cover_crop_MS_10m.tif")


water_wetness <- ("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Copernicus_grün_blau_grau/Water_Wetness")
list.files(water_wetness)
setwd(water_wetness)
water_wetness <- stack("copernicus_water_wetness_3x3_MS_10m.tif",
                      "copernicus_water_wetness_5x5_MS_10m.tif" ,
                       "copernicus_water_wetness_crop_MS_10m.tif")

#sky view factor
setwd("C:/Users/Dana/sciebo/ndom")
svf<-raster("svf_cor_10m.grd")
crs(svf)
svf <- projectRaster(svf, crs = "+proj=longlat +datum=WGS84 +no_defs",
method = "ngb" ,r)

#surface roughness length
rl_mean<-raster("ndom_mean_10m")
#reproject
rl_mean<-projectRaster(rl_mean, crs = "+proj=longlat +datum=WGS84 +no_defs",
              method = "ngb" ,r)
writeRaster(rl_mean,"ndom_mean_10m_reproj", overwrite=T)
#standard deviation
rl_sd<-raster("ndom_sd_10m")
#reproject
rl_sd<-projectRaster(rl_sd, crs = "+proj=longlat +datum=WGS84 +no_defs",
                       method = "ngb" ,r)

writeRaster(rl_sd,"ndom_sd_10m_reproj", overwrite=T)

#load shape of münster
gadm <- getData('GADM',country='DEU', level =2) 
extent <- gadm[gadm$NAME_2 == "Münster",]
extent <- as(extent, "sf")

all <- stack(water_wetness, treecov, imperv, svf, rl_mean, rl_sd)

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
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/lidar")
lidar <- "C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/lidar"
list.files(lidar)
lidar <- stack("Lidar_building_height.grd",
               "Lidar_building_sd_3x3.grd",
               "Lidar_building_sd_5x5.grd")

names(lidar) <- c("building_height", "building_height_sd_3x3", "building_height_sd_5x5")
lidar_crs <- projectRaster(lidar,crs = "+proj=longlat +datum=WGS84 +no_defs",
                           method = "ngb" ,r)

pred_stack_06 <- stack(all_static_pred_06, lidar_crs)
pred_stack_07 <- stack(all_static_pred_07, lidar_crs)
#rename some layer
names(pred_stack_06)
names(pred_stack_06)[names(pred_stack_06)==c("layer")]<-"SVF"
names(pred_stack_06)[names(pred_stack_06)==c("ndom_crop_muenster_int_1m.1")]<-"element_height_mean"
names(pred_stack_06)[names(pred_stack_06)==c("ndom_crop_muenster_int_1m.2")]<-"element_height_sd"

names(pred_stack_07)
names(pred_stack_07)[names(pred_stack_07)==c("layer")]<-"SVF"
names(pred_stack_07)[names(pred_stack_07)==c("ndom_crop_muenster_int_1m.1")]<-"element_height_mean"
names(pred_stack_07)[names(pred_stack_07)==c("ndom_crop_muenster_int_1m.2")]<-"element_height_sd"

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren")
writeRaster(pred_stack_06, "pred_stack_06_20221018.tif", bylayer=F, format="raster", overwrite=T)
writeRaster(pred_stack_07, "pred_stack_07_20221018.tif", bylayer=F, format="raster", overwrite=T)
#load stacks
#setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren")
#pred_stack_06<-stack("all_static_pred_06.grd")
#pred_stack_07<-stack("all_static_pred_07.grd")

load("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/SpatialPoints_Temp_Data")
#load("/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/SpatialPoints_Temp_Data")

times_06 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Time_of_day/times_06.csv")
times_07 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Time_of_day/times_07.csv")

times_06$date_time <- as.POSIXct(paste(times_06$day, times_06$time), format="%Y-%m-%d %H:%M:%S")

times_07$date_time <- as.POSIXct(paste(times_07$day, times_07$time), format="%Y-%m-%d %H:%M:%S")
  
remove(total_stack)
remove(total_stack_temp)


#load training data points
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/")
load("SpatialPoints_Temp_Data")
#str(spatial_list)
setwd( "C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger")
all_temp<-read.csv("all_temp.csv")

#load meteo predictors
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie")
meteo<-read.csv("meteo_all.csv")
str(meteo)
#rename
colnames(meteo)[1]<-"date_time"
meteo$date_time<-as.POSIXct(meteo$date_time)


#load dynamic predictors 
for(i in 1:222){
  if(!exists("total_stack")){
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #extract predictor values for training data
    extr <- raster::extract(pred_stack_06,logger_dat,df=TRUE) 
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- as.POSIXct(extr$time)
    extr_sun <-  left_join(extr, times_06[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
    #add meteodata
    extr_sun_meteo<-left_join(extr_sun, meteo, by= "date_time" )
    #rename
    total_stack<-extr_sun_meteo
  }
  else{
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #extract predictor values for training gdata
    extr <- raster::extract(pred_stack_06,logger_dat,df=T)  
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- extr$time
    extr$date_time <- as.POSIXct(extr$time)
    extr_sun <-  left_join(extr, times_06[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
    #add meteodata
    extr_sun_meteo<-left_join(extr_sun, meteo, by= "date_time" )
    #rename
    total_stack_temp<-extr_sun_meteo

    total_stack<-rbind(total_stack, total_stack_temp)
  }
} 
beep()
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/")
write.csv(total_stack, file ="total_stack_06_20221018.csv")

remove(total_stack)
remove(total_stack_temp)
#same for july
for(i in 223:length((spatial_list))){  
  if(!exists("total_stack")){
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #extract predictor values for training data
    extr <- raster::extract(pred_stack_07,logger_dat,df=TRUE) 
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- as.POSIXct(extr$time)
    extr_sun <-  left_join(extr, times_07[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
    #add meteodata
    extr_sun_meteo<-left_join(extr_sun, meteo, by= "date_time" )
    #rename
    total_stack<-extr_sun_meteo
  }
  else{
    #index logger list to get one element
    logger_dat<-spatial_list[[i]]
    #extract predictor values for training gdata
    extr <- raster::extract(pred_stack_07,logger_dat,df=T)  
    #create ID by row names
    logger_dat$ID<-row.names(logger_dat)
    #merge
    extr <- merge(extr,logger_dat@data,by.x="ID")
    #create column with time span value
    extr$time<-all_temp$datetime[as.numeric(i)]
    extr$date_time <- extr$time
    extr$date_time <- as.POSIXct(extr$time)
    extr_sun <-  left_join(extr, times_07[, c("hours_sss", "hours_ssr","date_time")], by = "date_time")
    #add meteodata
    extr_sun_meteo<-left_join(extr_sun, meteo, by= "date_time" )
    #rename
    total_stack_temp<-extr_sun_meteo
    
    total_stack<-rbind(total_stack, total_stack_temp)
  }
}  

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/")
write.csv(total_stack, file ="total_stack_07_20221018.csv")
remove(total_stack, total_stack_temp)
total_stack_06 <- read.csv("total_stack_06_20221018.csv")
total_stack_07 <- read.csv("total_stack_07_20221018.csv") 

names(total_stack_07)[names(total_stack_07) == 'albedo_07'] <- 'albedo'
names(total_stack_07)[names(total_stack_07) == 'ndvi_07'] <- 'ndvi'

names(total_stack_06)[names(total_stack_06) == 'albedo_06'] <- 'albedo'
names(total_stack_06)[names(total_stack_06) == 'ndvi_06'] <- 'ndvi'
  
total <- rbind(total_stack_06, total_stack_07)
total <- total[,2:35] #remove double first column
plot(total$hours_ssr, type="l")
lines(total$hours_sss, type="l", col="red")

#cheat for stability
for(i in 1:length(total$meteo_stability)){
  total$meteo_stability[i]<-meteo$meteo_stability[meteo$date_time==total$date_time[i]]
}

#set height of non-existing buildings to 0 
total$building_height[is.na(total$building_height)]<-0
total$building_height_sd_3x3[is.na(total$building_height_sd_3x3)]<-0
total$building_height_sd_5x5[is.na(total$building_height_sd_5x5)]<-0

#set to 0
total_$hours_ssr[is.na(total$hours_ssr)]<-0
total$hours_sss[is.na(total$hours_sss)]<-0

write.csv(total, file ="total_stack_20221018.csv", row.names = F)

test<-read.csv(file="total_stack_06_20221012.csv")

