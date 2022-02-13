#third model with 10m resolution
library(openair)
library(lubridate)
#set timespan needed
#starttime<-range(meteo$datetime)[1]
#endtime<-range(meteo$datetime)[2]

#"2020-07-07 02:00:00 CEST" "2020-07-28 23:00:00 CEST"
#prep prediction
#steinfurter: Temp, rH, windspeed, winddir, stability
#GeoDach: cloud cover, SWincoming

#####GeoDach####
#read in 2019 data
geo_2019<-read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach_2019.csv",
                     sep=",", dec=".", skip=2000, header=F, nrow=20000,fill = T, na.strings = c("-"),
                     col.names = read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach_2019.csv",
                                            sep=",", dec=".", skip=0, header=F, nrow=1))
#subset to parameters needed
geo_2019<-geo_2019[,c(1,16,23)]

#change colnames
colnames(geo_2019)[1] <- "date"
#format timestamp 1: 2019-08-14 19:10:00
geo_2019$date<-strptime(geo_2019$date, format="%Y-%m-%d %H:%M:%S", tz="Europe/London") #tz is russiun solution =eternal wintertime
geo_2019$date<-as.POSIXct(geo_2019$date, tz="Europe/London")

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
Summer2019<-meteo_geo[meteo_geo$date>="2019-08-20 00:00:00"&
                        meteo_geo$date<="2019-09-30 00:00:00",]
Autumn2019<-meteo_geo[meteo_geo$date>="2019-09-24 00:00:00"&
                        meteo_geo$date<="2019-11-09 00:00:00",]

Spring2020<-meteo_geo[meteo_geo$date>="2020-06-05 00:00:00"&
                        meteo_geo$date<="2020-06-19 00:00:00",]
Summer2020<-meteo_geo[meteo_geo$date>="2020-07-03 00:00:00"&
                        meteo_geo$date<="2020-07-31 00:00:00",]
#rbind together
meteo_geo<-rbind(Summer2019,Autumn2019,Spring2020,Summer2020)
#remove temporaray dat
rm(Summer2019,Autumn2019,Spring2020,Summer2020)
#meteo_geo<-meteo_geo[meteo_geo$date>=starttime&meteo_geo$date<=endtime,]
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Meteorologie/GeoDach/")
write.csv(meteo_geo, "meteo_geo.csv", row.names = F)

####Steinfurther Str####
#Steinf: Temp, rH,  stability (calculated from temp) #ACHTUNG ewige Sommerzeit
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Meteorologie/Steinfurter/preprocessed/")
#meteo_steinf_2019<-read.table(file = "Steinfurter_meteo_2019.csv", sep=",", dec=".",
#                        header=T)
meteo_steinf<-read.table(file = "Steinfurter_meteo_2020.csv", sep=";", dec=",",
                              header=T)
#meteo_steinf<-rbind(meteo_steinf_2019, meteo_steinf_2020)

str(meteo_steinf)
#QAQC
#plot(meteo_steinf$RH2m_Avg, type="l")

#subset to perameters needed
meteo_steinf<-meteo_steinf[,c(1,5,8,11)]
#QAQC
plot(meteo_steinf$timestamp,meteo_steinf$AirTC2m_Avg, type="l")
plot(meteo_steinf$timestamp,meteo_steinf$RH2m_Avg, type="l")
plot(meteo_steinf$timestamp,meteo_steinf$AirTC10m_Avg, type="l")

#convert to POSIXct
meteo_steinf$timestamp<-strptime(meteo_steinf$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin")
str(meteo_steinf)
meteo_steinf$timestamp<-as.POSIXct(meteo_steinf$timestamp)
colnames(meteo_steinf)[1]<-"date"
#round to hour
meteo_steinf=timeAverage(meteo_steinf,avg.time = "hour")
#calculate atmospheric stability
#only stable/unstable
meteo_steinf$stability<-NA
for( i in 1:length(meteo_steinf$stability)){
  if(meteo_steinf$AirTC2m_Avg[i]<meteo_steinf$AirTC10m_Avg[i]){
    meteo_steinf$stability[i]="stable"
  }else if(meteo_steinf$AirTC2m_Avg[i]>meteo_steinf$AirTC10m_Avg[i]){
    meteo_steinf$stability[i]="unstable"
  }else{}
}
#remove AirTC in 10m
meteo_steinf<-meteo_steinf[,-4]
#subset to timespan needed
Spring2020<-meteo_steinf[meteo_steinf$date>="2020-06-05 00:00:00"&
                        meteo_steinf$date<="2020-06-19 00:00:00",]
Summer2020<-meteo_steinf[meteo_steinf$date>="2020-07-03 00:00:00"&
                        meteo_steinf$date<="2020-07-31 00:00:00",]
#rbind together
meteo_steinf<-rbind(Spring2020,Summer2020)
#remove temporaray dat
rm(Spring2020,Summer2020)

#meteo_steinf<-meteo_steinf[meteo_steinf$date>=starttime&meteo_steinf$date<=endtime,]
#Steinf: Wind 
wind_steinf<-read.csv(file="C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Meteorologie/Steinfurter/preprocessed/Steinfurter_wind_2020.csv")
wind_steinf_1<-read.csv(file="C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Meteorologie/Steinfurter/preprocessed/Steinfurter_wind_spring2020.csv")
wind_steinf<-rbind(wind_steinf_1, wind_steinf)
wind_steinf$date<-strptime(wind_steinf$timestamp_start, format="%Y-%m-%d %H:%M:%S")
#subset to parameters needed
wind_steinf<-wind_steinf[,c(4,8,14)]
wind_steinf$date<-as.POSIXct(wind_steinf$date)
#aggregate by hour
wind_steinf=timeAverage(wind_steinf,avg.time = "hour")
#subset to timespan needed
#subset to timespan needed
Spring2020<-wind_steinf[wind_steinf$date>="2020-06-05 00:00:00"&
                           wind_steinf$date<="2020-06-19 00:00:00",]
Summer2020<-wind_steinf[wind_steinf$date>="2020-07-03 00:00:00"&
                           wind_steinf$date<="2020-07-31 00:00:00",]
#rbind together
wind_steinf<-rbind(Spring2020,Summer2020)
#remove temporaray dat
rm(Spring2020,Summer2020)
#wind_steinf<-wind_steinf[wind_steinf$date>=starttime&wind_steinf$date<=endtime,]

#combine all meteo data
str(meteo_steinf)
str(meteo_geo)
str(wind_steinf)
meteo <- data.frame("datetime"=meteo_steinf$date, 
                    "meteo_Temp"=meteo_steinf$AirTC2m_Avg,
                    "meteo_rH"=meteo_steinf$RH2m_Avg,
                    "meteo_stability" =meteo_steinf$stability,
                    "meteo_cloudcover"=meteo_geo$tcc,
                    "meteo_radiation"=meteo_geo$Shortwave.Radiation,
                    "meteo_windspeed"=wind_steinf$mean_windspeed)
meteo$meteo_stability<-as.factor(meteo$meteo_stability)

