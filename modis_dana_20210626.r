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

#load LST data
data_day <- lapply(filelist_data_day, raster)
data_night <- lapply(filelist_data_night, raster)
#convert values to celsius
data_day_celsius <- lapply(data_day, function(x) x-273.15)
data_night_celsius <- lapply(data_night, function(x) x-273.15)

#load time data
time_day <- lapply(filelist_time_day, raster)
time_night <- lapply(filelist_time_night, raster)
#convert values from local solar time to UTC
#longitude of Muenster is 7.6261

#load MS shape
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)

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

#stack time and data for day
day_stack<-data_day_celsius_crop
for(i in 1:length(data_day_celsius_crop)){
  day_stack[[i]]<-stack(data_day_celsius_crop[[i]], time_day_crop[[i]])
}
#stack time and data for night
night_stack<-data_night_celsius_crop
for(i in 1:length(data_night_celsius_crop)){
  night_stack[[i]]<-stack(data_night_celsius_crop[[i]], time_night_crop[[i]])
}

#exclude if all values are NA
day_stack_cc=Filter(function(a) sum(!is.na(values(a@layers[[1]]))), day_stack)
night_stack_cc=Filter(function(a) sum(!is.na(values(a@layers[[1]]))), night_stack)

mapview(day_stack_cc[[1]]@layers[[2]])
