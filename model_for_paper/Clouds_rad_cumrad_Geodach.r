#####GeoDach####
library(openair)
library(lubridate)
library(dplyr)
library(readxl)
#read in 2019 data
#geo_2019<-read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach_2019.csv",
#                     sep=",", dec=".", skip=2000, header=F, nrow=20000,fill = T, na.strings = c("-"),
#                     col.names = read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach_2019.csv",
#                                            sep=",", dec=".", skip=0, header=F, nrow=1))
#subset to parameters needed
#geo_2019<-geo_2019[,c(1,16,23)]

#change colnames
#colnames(geo_2019)[1] <- "date"
#format timestamp 1: 2019-08-14 19:10:00
#geo_2019$date<-strptime(geo_2019$date, format="%Y-%m-%d %H:%M:%S", tz="Europe/London") #tz is russiun solution =eternal wintertime
#geo_2019$date<-as.POSIXct(geo_2019$date, tz="Europe/London")

#read in 2020 data
geo_2020<-read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach2020_withcloudcover.csv",
                     sep=";", dec=",", skip=20000, header=F, nrow=20000,fill = T, na.strings = c("-"),
                     col.names = read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach2020_withcloudcover.csv",
                                            sep=";", dec=",", skip=2, header=F, nrow=1))

#subset to parameters needed
geo_2020<-geo_2020[,c(1,16,23)]
#change colnames
colnames(geo_2020)[1] <- "date"

#format timestamp
geo_2020$date<-strptime(geo_2020$date, format="%d.%m.%Y %H:%M", tz="Europe/London") #tz is russiun solution =eternal wintertime
geo_2020$date<-as.POSIXct(geo_2020$date, tz="Europe/London")

#put together
meteo_geo<-rbind(geo_2019, geo_2020)

str(meteo_geo)
#meteo_geo$tcc<-as.factor(meteo_geo$tcc)

#round to hour
meteo_geo=timeAverage(meteo_geo,avg.time = "hour")
meteo_geo$tcc<-round(meteo_geo$tcc, digits = 0)
meteo_geo$date<-as.POSIXct(meteo_geo$date, tz="Europe/London")
#convert to summertime to match Steinf and Logger 
meteo_geo <- with_tz(time = meteo_geo, tzone = "CET")
#subset to timespans needed
#2019:  
#08/20 - 09/30   2019-08-20 00:00:00 bis 2019-09-30 00:00:00
#09/24 - 11/09   2019-09-24 00:00:00 bis 2019-11-09 00:00:00
#2020
#06/05 - 06/19          2020-06-05 00:00:00 bis 2020-06-19 00:00:00
#07/03 - 07/31          2020-07-03 00:00:00 bis 2020-07-31 00:00:00

#choose timespans seperately
#Summer2019<-meteo_geo[meteo_geo$date>="2019-08-20 00:00:00"&
#                        meteo_geo$date<="2019-09-30 00:00:00",]
#Autumn2019<-meteo_geo[meteo_geo$date>="2019-09-24 00:00:00"&
#                        meteo_geo$date<="2019-11-09 00:00:00",]

Spring2020<-meteo_geo[meteo_geo$date>="2020-06-05 00:00:00"&
                        meteo_geo$date<="2020-06-19 00:00:00",]
Summer2020<-meteo_geo[meteo_geo$date>="2020-07-03 00:00:00"&
                        meteo_geo$date<="2020-07-31 00:00:00",]
#rbind together
meteo_geo<-rbind(Spring2020,Summer2020) #exclude Summer2019,Autumn2019,
#remove temporaray dat
rm(Spring2020,Summer2020) #exclude Summer2019,Autumn2019,
####calculate cumulative radiation per day####
#read in sunrise/sunset data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Pr??diktoren/Time of day")
sunrise<-read.table(file = "Sunrise_dates.csv", sep=";", dec=".", header=T)      
sunrise$day<-seq.Date(from=as.Date("2020-01-01"), to=as.Date("2020-12-31"), by="day")
sunrise<-sunrise[,c(3,7)]
sunrise$Sunrise<-as.factor(sunrise$Sunrise)
#save with proper date
write.csv(sunrise, "sunrise_with_date.csv", row.names = F)
#prepare meteo geo
meteo_geo$day<-lubridate::date(meteo_geo$date) #create column with day
#join with meteo_geo
meteo_geo<-left_join(meteo_geo, sunrise, by="day")
meteo_geo<-meteo_geo %>% 
  group_by(Sunrise) %>% #group by sunrise --> spans from one sunrise to next
  mutate(cum_radiation = cumsum(Shortwave.Radiation)) #calculate cumsum per day
#meteo_geo<-meteo_geo[meteo_geo$date>=starttime&meteo_geo$date<=endtime,]
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Pr??diktoren/Meteorologie/GeoDach/")
write.csv(meteo_geo, "meteo_geo.csv", row.names = F)
