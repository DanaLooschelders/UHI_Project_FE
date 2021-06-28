library(sp)
library(raster)
library(MODIS)
library(MODISTools)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
library(tiff)

getwd()
#load lists
#aqua
load("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/UHI_Project_FE/aqua_lists.rData")
#terra
load("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/UHI_Project_FE/terra_lists.rData")

#load LST data for day
data_day <- lapply(filelist_data_day, raster)
#set names
names(data_day)<-substr(filelist_data_day,78,93)
#load LST data for night
data_night <- lapply(filelist_data_night, raster)
#set names
names(data_night)<-substr(filelist_data_night,78,93)
#convert values to celsius
data_day_celsius <- lapply(data_day, function(x) x-273.15)
data_night_celsius <- lapply(data_night, function(x) x-273.15)

#load time data for day
time_day <- lapply(filelist_time_day, raster)
#set names
names(time_day)<-substr(filelist_time_day,78,93)
#load time data for night
time_night <- lapply(filelist_time_night, raster)
#set names
names(time_night)<-substr(filelist_time_night,78,93)

#load MS shape
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
#mapview(gadm_sf)

#coordinate transformation
data_day_celsius=lapply(data_day_celsius,  function(x) projectRaster(from=x, crs=crs(gadm)))
data_night_celsius=lapply(data_night_celsius,  function(x) projectRaster(from=x, crs=crs(gadm)))
time_day=lapply(time_day,  function(x) projectRaster(from=x, crs=crs(gadm)))
time_night=lapply(time_night,  function(x) projectRaster(from=x, crs=crs(gadm)))

#crop data
data_day_celsius_crop=lapply(data_day_celsius,  function(x) crop(x=x, y=gadm_sf))
data_night_celsius_crop=lapply(data_night_celsius,  function(x) crop(x=x, y=gadm_sf))
time_day_crop=lapply(time_day,  function(x) crop(x=x, y=gadm_sf))
time_night_crop=lapply(time_night,  function(x) crop(x=x, y=gadm_sf))

#check 
#mapview(data_day_celsius_crop[[5]])


####Info for solar time
#https://lpdaac.usgs.gov/documents/118/MOD11_User_Guide_V6.pdf
#Note that the Day_view_time and Night_view_time are in local solar time, 
#which is the UTC time plus grid’s longitude  in  degrees  /  15  degrees  
#(in hours,  +24  if  local  solar  time<  0  or -24  
#if local  solar  time  >=  24). The  data  day  in  the  name  of  all  
#the  daily  MOD11A1  files  is  in UTC so the data day in local solar
#time at each grid may be different from the data day in UTC by one day

#convert values from local solar time to UTC
#longitude of Muenster is 7.6261
range(values(time_day_crop[[5]]), na.rm=T)
range(values(time_day_crop[[7]]), na.rm=T)
#as the range in values is smaller than the temporal resolution of the training data
#a single value is assigned to every dataset
#LST = UTC + longitude/15
#UTC = LST - longitude/15
#use min value (beginn of swath) 

#create results dataframe (just to check)
UTC_time_day=rep(NA, length(time_day_crop))

#convert times in loop for day
for (i in 1:length(time_day_crop)){
  #check if time values are available
  if(sum(!is.na(values(time_day_crop[[i]])))){
  #get minimum local solar time value
  LST=min(values(time_day_crop[[i]]), na.rm=T)
  #calculate UTC 
  UTC=LST-7.6261/15
  #convert to UTC+2 (European summer time)
  MESZ=UTC+2
  #convert from decimal format into POSixct
  MESZ_format <- paste(floor(MESZ), 
                       round((MESZ-floor(MESZ))*60), sep=":")
  #write into results data frame
  #VERY UGLY way to get a leading zero for the hour
  UTC_time_day[i]<-substr(strptime(MESZ_format, 
                                     format="%H:%M"), 
                            start=12, stop=16)
  names(data_day_celsius_crop)[i]<-paste(names(data_day_celsius_crop)[i], 
                                         UTC_time_day[i], sep="_")
  }else{}
}
#check if it worked
UTC_time_day

#create results dataframe (just to check)
UTC_time_night=rep(NA, length(time_night_crop))
#convert times in loop for night
for (i in 1:length(time_night_crop)){
  #check if time values are available
  if(sum(!is.na(values(time_night_crop[[i]])))){
    #get minimum local solar time value
    LST=min(values(time_night_crop[[i]]), na.rm=T)
    #calculate UTC 
    UTC=LST-7.6261/15
    #convert to UTC+2 (European summer time)
    MESZ=UTC+2
    #convert from decimal format into POSixct
    MESZ_format <- paste(floor(MESZ), 
                         round((MESZ-floor(MESZ))*60), sep=":")
    #write into results data frame
    #VERY UGLY way to get a leading zero for the hour
    UTC_time_night[i]<-substr(strptime(MESZ_format, 
                                       format="%H:%M"), 
                              start=12, stop=16)
    names(data_night_celsius_crop)[i]<-paste(names(data_night_celsius_crop)[i], 
                                           UTC_time_night[i], sep="_")
  }else{}
}
#check if it worked
UTC_time_night
#stack time and data for day
#day_stack<-data_day_celsius_crop
#for(i in 1:length(data_day_celsius_crop)){
 # day_stack[[i]]<-stack(data_day_celsius_crop[[i]], time_day_crop[[i]])
#}
#stack time and data for night
#night_stack<-data_night_celsius_crop
#for(i in 1:length(data_night_celsius_crop)){
#  night_stack[[i]]<-stack(data_night_celsius_crop[[i]], time_night_crop[[i]])
#}

#exclude if all values are NA
day_cc=Filter(function(a) sum(!is.na(values(a))), data_day_celsius_crop)
night_cc=Filter(function(a) sum(!is.na(values(a))), data_night_celsius_crop)
#bind lists
terra=c(day_cc, night_cc)
#create dataframe to match POSIXct time to filename
terra_times=data.frame("filename"=names(terra), "datetime"=NA)
terra_times$datetime<-strptime(substr(terra_times$filename, 
                start=10, stop=nchar(terra_times$filename[1])),
         format="%Y%j_%H:%M", tz="UTC")
