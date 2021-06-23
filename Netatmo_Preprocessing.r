#process Netatmo data
library(lubridate)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Netatmo/")
MS_shape=readOGR("stadtgebiet.shp")

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Netatmo/Temperature")

#crs(MS_shape) #get crs
#transform coordinates to lat lon
MS_shape=spTransform(x = MS_shape, CRSobj = "+proj=longlat +datum=WGS84")
#crs(MS_shape) #check
#plot(MS_shape)
#plot points of netatmo stations
metadata_1=read.csv("data_August/stations.csv")
metadata_2=read.csv("data_August_September/stations.csv")
metadata_3=read.csv("data_September/stations.csv")
metadata_4=read.csv("data_September_2/stations.csv")
#for 2020
metadata_6=read.csv("data_2020/stations.csv")
metadata_7=read.csv("data_2020/stations_2.csv")
prep_plot=function(datapath="data_August/net_2019-08-01_to_2019-08-20.csv",
                   metadata=metadata_1,
                   startdate="01.08"){
  setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Netatmo/Temperature/")
  data=read.csv(datapath)
  
  stations=unique(data$device_id)
  
  #transform coordiantes to lat lon and create spatial points
  points=SpatialPointsDataFrame(coords = metadata_7[2:3], 
                                proj4string=CRS("+proj=longlat +datum=WGS84"),
                                data=metadata_7)
  #test: plotting points in shapefile
  #leaflet(MS_shape) %>%
  #  addPolygons() %>%
  #  addTiles() %>%
  #  addCircles(data=points)
  
  #subset points by shapefile -> get only points within Muenster
  station_subset <- points[MS_shape, ]
  
  ID_in_MS=station_subset@data$device_id #get vector with netatmo IDs in MS
  
  #test: plotting points in shapefile
  #leaflet(MS_shape) %>%
  #  addPolygons(fillOpacity = 0) %>%
  #  addTiles() %>%
  #  addCircles(data = station_subset, col="black")
  
  #create list for netatmo data
  #subset data by IDs and write data for each ID in a list element
  list_netatmo=list()
  for (i in ID_in_MS){
    list_netatmo[[i]]=data[data$device_id==i,]
  }
  
  list_netatmo[[1]]$isoTime[1] #ISO-8601 date format
  
  #choose column with iso time
  list_netatmo_datetime <- lapply(list_netatmo, `[`, 5)
  #cconvert from isotime to UTC
  list_netatmo_datetime=lapply(list_netatmo_datetime, function(x) ymd_hms(x$isoTime))
  #bind lists
  list_netatmo <- mapply(cbind, list_netatmo, "Datetime"=list_netatmo_datetime, SIMPLIFY=F)
  
  
  #ggplot(bind_rows(list_netatmo, .id="df"), aes(Datetime, temperature, colour=df)) +
  #  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS') 
  #ggsave(filename = paste("overview_netatmo", startdate,".pdf"), width=14, height=7)
  
  return(list_netatmo)
}
#merge Netatmo
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Netatmo/Temperature/")
#rowbind Netatmo data to form consecutive timeline
#create new list
#individually name lists to puzzle them together later
merge_netatmo=function(){
  list_netatmo_1=prep_plot(datapath="data_2020/net_2020-07-07_to_2020-07-14.csv",
                           metadata=metadata_6,
                           startdate="07.07")
  list_netatmo_2=prep_plot(datapath="data_2020/net_2020-07-14_to_2020-07-28.csv",
                           metadata=metadata_7,
                           startdate="14.07")
  #list_netatmo_3=prep_plot(datapath="data_September/net_2019-09-05_to_2019-09-25.csv",
  #metadata=metadata_3,
  #startdate="05.09")
  #list_netatmo_4=prep_plot(datapath="data_September_2/net_2019-09-25_to_2019-09-30.csv",
  #metadata=metadata_4,
  #startdate="25.09")
  list_netatmo_merge=list_netatmo_1
  #Map list 1 and list 2 together
  list_netatmo_merge[names(list_netatmo_2)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_2)], list_netatmo_2)
  #Map 1 (merged with 2) and 3 together
  #list_netatmo_merge[names(list_netatmo_3)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_3)], list_netatmo_3)
  #Map 1 (merged with 2,3) and 4 together
  #list_netatmo_merge[names(list_netatmo_4)] <- Map(rbind, list_netatmo_merge[names(list_netatmo_4)], list_netatmo_4)
  return(list_netatmo_merge)
}

#execute function
list_netatmo_merge=merge_netatmo()

length(unique(names(list_netatmo_merge)))

#convert to UTC+2 
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  data$Datetime=as.POSIXct(format(data$Datetime, tz="Europe/Berlin"))
  list_netatmo_merge[[i]]=data
}

#drop all dataframes with 0 rows
list_netatmo_merge=Filter(function(x) dim(x)[1] > 0, list_netatmo_merge)
length(list_netatmo_merge)

#remove all double values (overlapping measurements)
for (i in 1:length(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  data=data[!duplicated(data$timestamp),]
  list_netatmo_merge[[i]]=data
}

#add column with date
list_netatmo_merge_date <- lapply(list_netatmo_merge, `[`, 6)
list_netatmo_merge_date=lapply(list_netatmo_merge_date, function(x) as.Date(x$Datetime, tz="Europe/Berlin"))
list_netatmo_merge <- mapply(cbind, list_netatmo_merge, "Date"=list_netatmo_merge_date, SIMPLIFY=F)

#check and remove stations that didn't record every day
for (i in names(list_netatmo_merge)){
  if(length(unique(list_netatmo_merge[[i]]$Date))==
     length(seq(from=min(list_netatmo_merge[[i]]$Date), 
                to=max(list_netatmo_merge[[i]]$Date), by="day"))){
  }else{ list_netatmo_merge[[i]]=NULL}
}

#use only stations that recorded over whole period (check for start and end date)
#until one day later than specified, because of time conversion
for (i in names(list_netatmo_merge)){
  data=list_netatmo_merge[[i]]
  if(data$Date[1]=="2020-07-07"&data$Date[length(data$date)]=="2020-07-29"){}
  else{list_netatmo_merge[[i]]=NULL}
}
#as there are missing values inbetween create consecutive time sequence
#and create NAs for missing rows in order to get all dataframes to the same length
list_netatmo_whole=list_netatmo_merge
for (i in 1:length(list_netatmo_whole)){
  time=data.frame("Datetime"=seq(from=as.POSIXct("2020-07-07 02:15:00", tz="Europe/Berlin"), 
                                 to=as.POSIXct("2020-07-29 01:45:00", tz="Europe/Berlin"), by="30 min"))
  data=list_netatmo_merge[[i]]
  time=merge(x=time,y=data, all.x=TRUE, by="Datetime")
  list_netatmo_whole[[i]]=time
}

list_netatmo_merge=list_netatmo_whole
#replace NAs in Date column by date
for (i in 1:length(list_netatmo_merge)){
  dat=list_netatmo_merge[[i]]
  dat$Date=as.Date(dat$Datetime, tz="Europe/Berlin") #fill column with date
  dat$device_id=names(list_netatmo_merge)[i] #add device id
  list_netatmo_merge[[i]]=dat
}

#subset to the 30th of September
for (i in 1:length(list_netatmo_merge)){
  dat=list_netatmo_merge[[i]]
  dat=dat[dat$Datetime<=as.POSIXct("2020-07-28 23:45:00"),]
  list_netatmo_merge[[i]]=dat
}

#plot with no legend (as legend takes most of the screen)
ggplot(bind_rows(list_netatmo_merge, .id="df"), aes(Datetime, temperature, colour=df)) +
  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
  theme(legend.position="none")
ggsave(filename = "overview_netatmo_merge_2020.pdf", width=14, height=7)


metadata_merge=rbind(metadata_6, metadata_7)
metadata_merge=metadata_merge[!duplicated(metadata_merge$device_id),]
length(unique(metadata_merge$device_id))

#shorten metadatalist by excluding the IDs that had no data
rownames(metadata_merge)=metadata_merge$device_id #set ID as rownames
ids_to_keep=names(list_netatmo_merge) #get character vector of ids to keep
metadata_merge=metadata_merge[ids_to_keep,] #subset metadata with ids from data

#add column with month index for August and September
#add month index to dataframe
list_netatmo_month <- lapply(list_netatmo_merge, `[`, 7)
list_netatmo_month=lapply(list_netatmo_month, function(x) strftime(x$Date, "%B", tz="Europe/Berlin"))
list_netatmo_merge <- mapply(cbind, list_netatmo_merge, "Month"=list_netatmo_month, SIMPLIFY=F)

#tidy up environnment
rm(metadata_1, metadata_2, metadata_3, metadata_4, 
   list_netatmo_merge_date, list_netatmo_month, list_netatmo_whole,
   ids_to_keep) 