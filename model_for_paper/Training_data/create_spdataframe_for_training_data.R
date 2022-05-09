library(readxl)
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
library(tidyverse)
library(lubridate)
library(openair)
####Netatmo####
#create spatial points dataframe with Netatmo Temp for a certain time
#load Netatmo data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Netatmo/")
netatmo_1<-read.csv("Netatmo_2020_Juni.csv")
netatmo_2<-read.csv("Netatmo_2020.csv")
#rowbind matching columns, create empty colums in both dataframes 
#for those that do not match
netatmo_1[setdiff(names(netatmo_2), names(netatmo_1))] <- NA
netatmo_2[setdiff(names(netatmo_1), names(netatmo_2))] <- NA
#rbind dataframes
netatmo<-rbind(netatmo_1, netatmo_2)
netatmo$datetime<-as.POSIXct(netatmo$datetime)
#move datetime to last column
netatmo<-netatmo %>% relocate(datetime, .after = last_col())
#remove x in front of colnames
colnames(netatmo)[2:43]<-substr(colnames(netatmo)[2:43], start=2, 
                                stop=nchar(colnames(netatmo)[2:43]))
#replace point with doublepoint so that names match
colnames(netatmo)<-gsub(colnames(netatmo), pattern = "[.]", replacement=":")
#load metadata with spatial information
juni<-read.csv2("Netatmo_metadata_Juni.csv")
juni<-juni[,2:9] #remove double column
july_1<-read.csv("stations_07_07.csv")
july_1<-july_1[,1:8]
july_2<-read.csv("stations_07_14.csv")
july_2<-july_2[,1:8]
netatmo_metadata<-rbind(juni, july_1, july_2) #combine metadata
#keep only unique values
netatmo_metadata<-netatmo_metadata[!duplicated(netatmo_metadata$device_id),]
#get metadata from netatmo devices  actually used (by names of devices in list)
names=data.frame("device_id"=colnames(netatmo)[2:43])
#merge metadata by device id
merge_metadata=merge(netatmo_metadata, names,by = "device_id")
#create metadata subset for merge
netatmo_metadata_subset <- merge_metadata[ , names(merge_metadata) %in% c("lat", "lon")]
rownames(netatmo_metadata_subset)<-merge_metadata$device_id
netatmo_metadata_subset$index<-rep("Netatmo")
#change order of columns to match logger metadata
netatmo_metadata_subset<-netatmo_metadata_subset[,c(2,1,3)]
names(netatmo_metadata_subset)<-c("Lat", "Lon", "index")
####Logger####
#create spatial points dataframe with Logger Temp for a certain time
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger/")
#load logger data
logger=read.csv(file="Logger_2020.csv", header=T)

logger=logger[,-1] #remove first column that contained rownames
colnames(logger)[1:32]=substr(colnames(logger)[1:32], start=2, stop=10)

#average logger data by hour
#load coordinates of logger
#read in metadata
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Temp_Logger/")
des=read_excel(path = "Sensortabelle_Kartierung_Stand_22.07.2020_DL_ohne_meta.xlsx", 
               col_names = T, na = "NA")
#exclude water temp logger
waterlogger=des$Logger_ID[des$Loggertyp=="WL"]
logger <- logger[ , ! names(logger) %in% c(waterlogger, "69")] 

logger$date<-as.POSIXct(logger$datetime)
logger<-logger[,-27]
logger=timeAverage(logger,avg.time = "hour")
names(logger)[1]<-"datetime"
#load modis times and dates to match with logger temp
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/")
aqua<-read.csv(file="aqua_processed/aqua_times.csv")
aqua$datetime<- as.POSIXct(aqua$datetime)
terra<-read.csv(file="terra_processed/terra_times.csv")
terra$datetime<- as.POSIXct(terra$datetime)
#round time to nearest 10 mins
aqua$datetime_round<- round_date(aqua$datetime,unit="hours")
terra$datetime_round<-round_date(terra$datetime,unit="hours")
#bind together
modis<-rbind(aqua, terra)
#create a metadata table for logger
#set ID as first column
metadata=data.frame("Logger_ID"=as.integer(colnames(logger)[2:27]))
#get metadata from loggers actually used (by names of loggers in list)
metadata=merge(metadata, des,by = "Logger_ID")
#correct lat/lon values
metadata$Lat=metadata$Lat/1000000
metadata$Lon=metadata$Lon/1000000
#create metadata subset for merge
metadata_subset <- metadata[ , names(metadata) %in% c("Lat", "Lon")]
rownames(metadata_subset)<-metadata$Logger_ID
metadata_subset$index<-rep("Logger")
####combine logger and Netatmo data####
all_metadata<-rbind(metadata_subset, netatmo_metadata_subset)
all_temp=inner_join(logger, netatmo, by="datetime")
str(all_metadata)
str(all_temp)

#create dataframe per time
which(colnames(all_temp)=="datetime")
for(i in 1:length(modis$filename)){
  if(any(all_temp$datetime==modis$datetime_round[i], na.rm = T))
  {if(i==1){
    all_temp_match<-list()
    temp_dat<-data.frame(ID<-as.character(colnames(all_temp)[-1]), 
                         temperature<-t(all_temp[all_temp$datetime==modis$datetime_round[i],-1]))
    all_temp_match[[i]]<-merge(all_metadata, temp_dat, by="row.names" )
    names(all_temp_match[[i]])<-c("rownames", "Lat", "Lon","index", "Logger_ID", "Temperature")
    names(all_temp_match)[[i]]<-modis$filename[i]
  }else{
    temp_dat<-data.frame(ID<-as.character(colnames(all_temp)[-1]), 
                         temperature<-t(all_temp[all_temp$datetime==modis$datetime_round[i],-1]))
    all_temp_match[[i]]<-merge(all_metadata,temp_dat, by="row.names" )
    names(all_temp_match[[i]])<-c("rownames", "Lat", "Lon","index", "Logger_ID", "Temperature")
    names(all_temp_match)[[i]]<-modis$filename[i]
  }
  }else{}
}

####create spatialdataframe####
#add Temperature to metadata
#metadata$Temperature=NA
#for(i in as.integer(colnames(logger)[1:26])){
#  index=which(colnames(logger)==as.character(i))
#  metadata$Temperature[metadata$Logger_ID==i]<-logger[index,1]
#}

#remove empty list entries
all_temp_match = all_temp_match[-which(sapply(all_temp_match, is.null))]
#create spatialpointsdataframe with logger coordinates

for(i in 1:length(all_temp_match)){
  if(i ==1){
    spatial_list=all_temp_match
    spatial_list[[i]]<-SpatialPointsDataFrame(coords=spatial_list[[i]][,c(3,2)], 
                                              data=data.frame(Temp=spatial_list[[i]][,6]),
                                              proj4string=CRS(as.character(crs(gadm))))
    
  }else{
    spatial_list[[i]]<-SpatialPointsDataFrame(coords=spatial_list[[i]][,c(3,2)],
                                              data=data.frame(Temp=spatial_list[[i]][,6]),
                                              proj4string=CRS(as.character(crs(gadm))))
  }
}

spTransform(spatial_list[[1]], CRSobj = crs(gadm))
#save workspace as list
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet")
save.image(file="SpatialPoints_Temp_Data")

#test to plot modis with training data
#get modis sample
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed_resampled/")
modisfiles=list.files(pattern="*.tif")
modis_r=raster(modisfiles[8])
ncell(modis_r)

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/aqua_processed/")
modisfiles=list.files(pattern="*.tif")
modis_nr=raster(modisfiles[8])
ncell(modis_nr)


modisfiles=list.files(pattern="*.tif")
modis=raster(modisfiles[1])
#plot
mapview(modis)+mapview(spatial_list[[1]])

#count points in cell
#plot
test_count <- rasterize(spatial_list[[1]], modis, fun="count")
plot(test_count)
#table
test=na.omit(cbind(1:ncell(test_count), values(test_count)))
#table version 2
x <- rasterToPoints(test_count)
z <- cbind(cell=cellFromXY(test_count, x[,1:2]), value=x[,3])
