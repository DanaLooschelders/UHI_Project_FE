library(mapview)
library(sf)
library(raster)
library(sp)
#calculate temperature for different city regions
gadm_3 <- getData('GADM', country='DEU', level=4)
gadm_3 <- gadm_3[gadm_3$NAME_2=="Münster",]
plot(gadm_3)
mapview(gadm_3)
gadm_3
#load region data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Stadtbezirke")
regions<-read_sf("plz_5-stellig_muenster.kml")
#check
mapview(regions)
#check crs
crs(regions)
crs(model_1_day_predict)
#transform to SpatialPolygonsDataframe
regions_sp<-as(regions, "Spatial")
#extract values from model
extract_temp_model_1<-extract(model_1_day_predict, regions_sp)
#rename with post code
names(extract_temp_model_1)<-unique(regions$Name)
#calculate mean and median temp
mean_temp_model_1<-lapply(extract_temp_model_1, function(x) mean(x, na.rm=T))
median_temp_model_1<-lapply(extract_temp_model_1, function(x) median(x, na.rm=T))
#write in dataframe
temps_model_1<-data.frame("name"<-names(mean_temp_model_1), "mean"=unlist(mean_temp_model_1),
                  "median"<-unlist(median_temp_model_1))

#model 2
#extract values from model
extract_temp_model_2<-extract(model_2_day_predict, regions_sp)
#rename with post code
names(extract_temp_model_2)<-unique(regions$Name)
#calculate mean and median temp
mean_temp_model_2<-lapply(extract_temp_model_2, function(x) mean(x, na.rm=T))
median_temp_model_2<-lapply(extract_temp_model_2, function(x) median(x, na.rm=T))
#write in dataframe
temps_model_2<-data.frame("name"<-names(mean_temp_model_2), "mean"=unlist(mean_temp_model_2),
                          "median"<-unlist(median_temp_model_2))


                   
