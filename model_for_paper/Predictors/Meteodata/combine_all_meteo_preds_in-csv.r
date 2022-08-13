#third model with 10m resolution
library(openair)
library(lubridate)
#set timespan needed
#starttime<-range(meteo$datetime)[1]
#endtime<-range(meteo$datetime)[2]

#"2020-07-07 02:00:00 CEST" "2020-07-28 23:00:00 CEST"
#prep prediction
#steinfurter: Temp, rH, windspeed, winddir, stability
#GeoDach: cloud cover, SWincoming, cum SW incoming

####Geodach####
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie/GeoDach/")
meteo_geo<-read.table(file="meteo_geo.csv", dec=".", sep=",", header=T)
####Steinfurther Str####
#Steinf: Temp, rH,  stability (calculated from temp) #ACHTUNG ewige Sommerzeit
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie/Steinfurter/preprocessed/")
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

#convert to POSIXct
meteo_steinf$timestamp<-strptime(meteo_steinf$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin")
str(meteo_steinf)
meteo_steinf$timestamp<-as.POSIXct(meteo_steinf$timestamp)
##QAQC
plot(meteo_steinf$timestamp,meteo_steinf$AirTC2m_Avg, type="l")
plot(meteo_steinf$timestamp,meteo_steinf$RH2m_Avg, type="l")
plot(meteo_steinf$timestamp,meteo_steinf$AirTC10m_Avg, type="l")
#rename date column
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
wind_steinf<-read.csv(file="C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie/Steinfurter/preprocessed/Steinfurter_wind_2020.csv")
wind_steinf_1<-read.csv(file="C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie/Steinfurter/preprocessed/Steinfurter_wind_spring2020.csv")
wind_steinf<-rbind(wind_steinf_1, wind_steinf)
wind_steinf$date<-strptime(wind_steinf$timestamp_start, format="%Y-%m-%d %H:%M:%S")
#subset to parameters needed
wind_steinf<-wind_steinf[,c(4,8,14)]
wind_steinf$date<-as.POSIXct(wind_steinf$date)
names(wind_steinf)[1:2]<-c("ws", "wd") #rename
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
                    "meteo_cum_radiation"=meteo_geo$cum_radiation,
                    "meteo_windspeed"=wind_steinf$ws,
                    "meteo_winddirection"=wind_steinf$wd)
meteo$meteo_stability<-as.factor(meteo$meteo_stability)

#write in file
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Meteorologie/")
write.csv(meteo, file = "meteo_all.csv", row.names = F )
