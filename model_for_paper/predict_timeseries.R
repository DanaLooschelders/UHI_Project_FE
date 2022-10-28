#model metrics
library(RStoolbox)
library(dplyr)
library(terra)

library(sp)
library(sf) 
library(mapview)
library(raster)
library(rgdal)
library(tmap)
library(tmaptools)
library(stars)
library(CAST)
library(caret)
library(randomForest)
library(latticeExtra)
library(beepr)
#install.packages("animation")
library(animation)
#parallel
library(parallel)
#install.packages("doParallel")
library(doParallel)
#install.packages("pushBullet")
#library(push)
#predict for whole time period
#load model
setwd("C:/Users/Dana/sciebo/ndom/klaus/")
model<-readRDS(file = "ffs_Model_2022-10-13.RDS")
writeRaster(total_stack[[149]], filename = "test_raster")
varImp(model)
#load meteo data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie")
meteo<-read.csv("meteo_all.csv")
meteo$datetime<-as.POSIXct(meteo$datetime)

#load static pred stack
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren")
pred_stack_06<-stack("pred_stack_06_20221012.grd")
names(pred_stack_06)[1:2]<-c("albedo","ndvi") #rename to match model
pred_stack_07<-stack("pred_stack_07_20221012.grd")
names(pred_stack_07)[1:2]<-c("albedo","ndvi") #rename to match model
#correct buildings
values(pred_stack_06$building_height)[is.na(values(pred_stack_06$building_height))]<-0
values(pred_stack_06$building_height_sd_3x3)[is.na(values(pred_stack_06$building_height_sd_3x3))]<-0
values(pred_stack_06$building_height_sd_5x5)[is.na(values(pred_stack_06$building_height_sd_5x5))]<-0
#for pred_stack_07
values(pred_stack_07$building_height)[is.na(values(pred_stack_07$building_height))]<-0
values(pred_stack_07$building_height_sd_3x3)[is.na(values(pred_stack_07$building_height_sd_3x3))]<-0
values(pred_stack_07$building_height_sd_5x5)[is.na(values(pred_stack_07$building_height_sd_5x5))]<-0

#load empty raster for meteos
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction/Stacks_for_prediction/")
raster_Steinf<-stack("empty_Raster_Steinf.grd")

setwd("D:/Meteo/")
#####meteo####
for(i in 700:800){
  #Temperature
  raster_Steinf_temp<-raster_Steinf
  values(raster_Steinf_temp)<-meteo$meteo_Temp[i]
  #relative humidity
  raster_Steinf_RH<-raster_Steinf
  values(raster_Steinf_RH)<-meteo$meteo_rH[i]
  #stability
  raster_Steinf_stability<-raster_Steinf
  values(raster_Steinf_stability)<-meteo$meteo_stability[i]
  #cloudcover
  raster_Steinf_cloudcover<-raster_Steinf
  values(raster_Steinf_cloudcover)<-meteo$meteo_cloudcover[i]
  #radiation
  raster_Steinf_radiation<-raster_Steinf
  values(raster_Steinf_radiation)<-meteo$meteo_radiation[i]
  #cum_radiation
  raster_Steinf_cum_radiation<-raster_Steinf
  values(raster_Steinf_cum_radiation)<-meteo$meteo_cum_radiation[i]
  #precipitation current
  raster_Steinf_precip<-raster_Steinf
  values(raster_Steinf_precip)<-meteo$meteo_precip[i]
  #precipitation last 3h
  raster_Steinf_precip3h<-raster_Steinf
  values(raster_Steinf_precip3h)<-meteo$meteo_precip3hour[i]
  #precipitation last day
  raster_Steinf_precip1day<-raster_Steinf
  values(raster_Steinf_precip1day)<-meteo$meteo_precip1day[i]
  #wind speed
  raster_Steinf_windspeed<-raster_Steinf
  values(raster_Steinf_windspeed)<-meteo$meteo_windspeed[i]
  #wind direction
  raster_Steinf_winddir<-raster_Steinf
  values(raster_Steinf_winddir)<-meteo$meteo_winddirection[i]
  #stack values
  meteo_stack<-stack(raster_Steinf_RH, raster_Steinf_temp, raster_Steinf_stability,
                     raster_Steinf_cloudcover, raster_Steinf_radiation, 
                     raster_Steinf_cum_radiation, raster_Steinf_precip, raster_Steinf_precip3h,
                     raster_Steinf_precip1day,raster_Steinf_windspeed, raster_Steinf_winddir)
  #set layer names
  names(meteo_stack)<-c("meteo_RH", "meteo_Temp", "meteo_stability", 
                        "meteo_cloudcover", "meteo_radiation", "meteo_cum_radiation",
                        "meteo_precip", "meteo_precip3hour", "meteo_precip1day",
                        "meteo_windspeed","meteo_winddirection")
  #write raster into file
  writeRaster(meteo_stack, filename = paste("Meteo_", i,
                                            sep="_"), overwrite=T)
}
beep()
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Time_of_day/")
times<-read.csv("times_tidy_20221018.csv")
times$hours_ssr[is.na(times$hours_ssr)]<-0
times$hours_sss[is.na(times$hours_sss)]<-0
####times of day####
setwd("D:/Times/")

for(i in 139:139){
  tryCatch({
  #hours_sss
  raster_Steinf_sss<-raster_Steinf
  values(raster_Steinf_sss)<-times$hours_sss[i]
  #hours_ssr
  raster_Steinf_ssr<-raster_Steinf
  values(raster_Steinf_ssr)<-times$hours_ssr[i]
  #stack values
  time_stack<-stack(raster_Steinf_sss, raster_Steinf_ssr)
  #set layer names
  names(time_stack)<-c("hours_sss", "hours_ssr")
  #write raster into file
  writeRaster(time_stack, filename = paste("time_", i,
                                           sep="_"), overwrite=T)
  }, error=function(e){})
}

beep()

#create output list
pred_list<-vector(mode='list', length=50)
x=0 #initialise x
for(i in 130:170){
  print(i)
  x=x+1
  if(i<=222){ #take june pred stack
    #load meteo data
    meteo_name<-paste("meteo__", i, ".grd", sep = "")
    meteo_stack<-stack(paste("D:/Meteo/", meteo_name, sep = ""))
    #load times data
    times_name<-paste("time__", i, ".grd", sep = "")
    times_stack<-stack(paste("D:/Times/", times_name, sep = ""))
    #stack predictors
    pred_stack_temp<-stack(meteo_stack, times_stack, pred_stack_06)
    #predict
    pred_list[[x]]<-predict(pred_stack_temp, model, savePrediction=TRUE)
  }else{ #take july pred stack
    #load meteo data
    meteo_name<-paste("meteo__", i, ".grd", sep = "")
    meteo_stack<-stack(paste("D:/Meteo/", meteo_name, sep = ""))
    #load times data
    times_name<-paste("time__", i, ".grd", sep = "")
    times_stack<-stack(paste("D:/Times/", times_name, sep = ""))
    #stack predictors
    pred_stack_temp<-stack(meteo_stack, times_stack, pred_stack_07)
    #predict
    pred_list[[x]]<-predict(pred_stack_temp, model, savePrediction=TRUE)
  }
}

beep()
setwd("C:/Users/Dana/Desktop")
saveRDS(pred_list, file="pred_list_130_170.RDS")

pred_list<-readRDS(file="pred_list_130_170.RDS")

#stack all predictions
pred_plot_stack <- stack(pred_list[1:41])
rm(pred_list, pred_stack_06, pred_stack_07)
spplot(pred_plot_stack[[40]])
writeRaster(pred_plot_stack[[50]], filename="test_raster")
#rm(pred_stack_06, pred_stack_07, meteo_stack, times_stack)
writeRaster(pred_plot_stack[[191:208]], "pred_plot_stack_10", overwrite=T)
pred_plot_stack_test<-stack("pred_plot_stack_3.grd")
names(pred_plot_stack_test)

spplot(pred_plot_stack[[80]])
names(pred_plot_stack)<-meteo$datetime[130:170]
#animate all layers of stack
saveGIF(animate(pred_plot_stack[[1:41]], pause=0.2, col=heat.colors(n=30,rev = T)),
        movie.name = "pred_plot_130_170.gif")
getwd()
setwd("C:/Users/Dana/Desktop/Pred_plot_stacks/")
#create animated gif for all stacks

for(i in 2:10){
  print(i)
  pred_plot_stack_test<-stack(paste("pred_plot_stack_",i, ".grd", sep=""))
  #get rows for meteodata
  rows<-as.numeric(substr(names(pred_plot_stack_test), start=7, stop=9))
  #rename to time
  names(pred_plot_stack_test)<-meteo$datetime[rows]
  #save as gif
  saveGIF(animate(pred_plot_stack_test, pause=0.2, col=heat.colors(n=30,rev = T)),
          movie.name = paste("pred_", i, ".gif", sep=""))
  
}
#get rows for meteodata
rows<-as.numeric(substr(names(pred_plot_stack), start=7, stop=9))
#rename to time
names(pred_plot_stack)<-meteo$datetime[rows]

#save gif for al frames
saveGIF(animate(pred_plot_stack[[100:208]], pause=0.2, col=heat.colors(n=30,rev = T)),
        movie.name = "pred_plot_whole_part_2.gif")
beep()
plot(pred_plot_stack[[1]])
#save names in vector
name_vec<-names(pred_plot_stack)
#use pred_list because all items are called "layer" and can be used to call ggplot
#DO NOT CHANGE NAME OF LIST ITEMS
setwd("C:/Users/Dana/Desktop/Predictions_ggplot")
for(i in 1:length(pred_list)){
  tryCatch({
  ggplot(pred_list[[i]]) +  
  geom_tile(aes(x=x, y=y, fill=layer)) +
  coord_equal()+
  ggtitle(paste(substr(name_vec[i], start = 2, stop=11), " ", 
                substr(name_vec[i], start=13, stop=14), "o'clock"))+
  scale_fill_gradient("Temp. [°C]", na.value = "white", low = "yellow", high = "red")+
  theme_bw()
  ggsave(filename=paste("prediction_20221028_", 
                        name_vec[i],
                        ".jpg", sep=""), width = 300, 
         height=200, units="mm")
  }, error=function(e){})
}

names(pred_plot_stack[[1]])
pred_plot_stack[[1]]
