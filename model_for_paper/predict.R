#model metrics
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
#parallel
library(parallel)
#install.packages("doParallel")
library(doParallel)

no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores) 
registerDoParallel(cl)

#RMSE
plot(model)

#Variable importance
varImp(model)
#as plot
plot(varImp(model))

#predict
sample<-sample(1:nrow(meteo),5)
#random sample is 170 341  547 547 547

#load static pred stack
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
pred_stack_06<-stack("all_static_pred_06.grd")
names(pred_stack_06)[1:2]<-c("albedo","ndvi") #rename to match model
pred_stack_07<-stack("all_static_pred_07.grd")
names(pred_stack_07)[1:2]<-c("albedo","ndvi") #rename to match model

#load dynamic meteo preds (5 randomly sampled from all meteo files)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prediction/Meteo_for_prediction")
meteo_547<-stack("Meteo__547.grd")
meteo_547$meteo_stability
#load time_of_day data and write as raster
times_06 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Time_of_day/times_06.csv")
times_07 <- read.csv("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Time_of_day/times_07.csv")
#combine
times<-rbind(times_06, times_07)
#change NA values to 0
times$hours_sss[is.na(times$hours_sss)]<-0
times$hours_ssr[is.na(times$hours_ssr)]<-0
#create Raster
#get shape of polygon
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)
#load refrence raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/Copernicus_grün_blau_grau/Imperviousness")
ref_raster<-raster("copernicus_imperviousness_crop_MS_10m.tif")
#transform polygon into Raster
r <- raster(ncol=ncol(ref_raster), nrow=nrow(ref_raster), crs = "+proj=longlat +datum=WGS84 +no_defs")
extent(r) <- extent(ref_raster)
raster_Steinf<-rasterize(gadm, r)
#write to raster (only samples)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prediction/sunshinehours_for_prediction")

for(i in sample){
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
}

#load time raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prediction/sunshinehours_for_prediction")
time_547<-stack("time__547.grd")


#load lidar data
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

#set height of non-existing buildings to 0 
values(lidar_crs$building_height)[is.na(values(lidar_crs$building_height))]<-0
values(lidar_crs$building_height_sd_3x3)[is.na(values(lidar_crs$building_height_sd_3x3))]<-0
values(lidar_crs$building_height_sd_5x5)[is.na(values(lidar_crs$building_height_sd_5x5))]<-0

#stack all
pred_stack_all_547 <- stack(pred_stack_07, lidar_crs, meteo_547, time_547)

#mapview(pred_stack_all_547, maxpixels =  5073950)
#plot(pred_stack_all_547)

names(pred_stack_all_547)

#save as raster
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modell")
writeRaster(pred_stack_all_547,filename= "pred_stack_all_547.tif", bylayer=F,format="raster",overwrite=T)

#check
names(pred_stack_07)
plot(pred_stack_07)

names(lidar_crs)
plot(lidar_crs)

names(meteo_547)
plot(meteo_547)

names(time_547)
plot(time_547)

#predict
model_547_predict<-predict(pred_stack_all_547, model, savePrediction=TRUE)

mapview(model_547_predict, maxpixels =  5073950)

any(!is.na(values(model_547_predict)))
#check which time was predicted
all_temp[547,] #2020-07-20 14:00:00
all_temp[547,] #2020-07-16 01:00:00 

#calculate AOA
pred_stack_547_selected_vars<-dropLayer(pred_stack_all_547, setdiff(names(pred_stack_all_547),model$selectedvars))#drop layers with vars not used
model_547_aoa<-aoa(pred_stack_547_selected_vars, model, cl=cl)

train_new<-model$trainingData

table(total_stack$meteo_stability)

for(i in 1:ncol(train_new)){
  var<-var(train_new[,i])
  print(var)
}
#stability has no variance
table(meteo$meteo_stability)
as.factor(meteo$meteo_stability)

#stackApply(pred_stack_all_547,indices=c(seq(1:nlayers(pred_stack_all_547))), fun=var)
vars<-rep(NA, nlayers(pred_stack_all_547)) #create output dataframe
nas<-rep(NA, nlayers(pred_stack_all_547))
for(i in 1:nlayers(pred_stack_all_547)){
  #check variance
  variance<-var(values(pred_stack_all_547[[i]]), na.rm=T)
  vars[i]<-variance
  #check nas
  navalues<-any(is.na(values(pred_stack_all_547[[i]])))
  nas[i]<-navalues
}
nas
vars

any(is.na(values(pred_stack_all_547$albedo)))

####check old stuff#### 
#load model
setwd("C:/Users/Dana/sciebo/Archive_UHI_Projekt_FE/Modelle")
old_model<-readRDS("ffs_Model_2021-08-10.RDS")
#old pred stack
setwd("C:/Users/Dana/sciebo/Archive_UHI_Projekt_FE/Daten_bearbeitet")
pred_stack<-stack("pred_stack.grd")
meteo_day<-stack("C:/Users/Dana/sciebo/Archive_UHI_Projekt_FE/Daten_bearbeitet/Meteo_data_steinf/Meteo_MOD11A1_A2020157_22_23.grd")
#resample meteo
meteo<-raster::resample(meteo_day, pred_stack, method="ngb")
mapview(pred_stack)

vars<-rep(NA, nlayers(pred_stack)) #create output dataframe
any(is.na(values(pred_stack$copernicus_tree_cover_MS_100m)))
any(is.na(values(pred_stack$dlm_raster)))
any(is.na(values(pred_stack$ucz_ms_100m)))

for(i in 1:nlayers(pred_stack)){
  variance<-var(values(pred_stack[[i]]))
  vars[i]<-variance
}
vars

pred_stack_all_old<-stack(pred_stack, meteo)
names(pred_stack_all_old)
test<-aoa(pred_stack_all_old, old_model, cl=cl)

train_old<-old_model$trainingData
