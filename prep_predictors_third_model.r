#third model with 10m resolution
library(openair)
library(lubridate)
#set timespan needed
starttime<-range(meteo$datetime)[1]
endtime<-range(meteo$datetime)[2]

#"2020-07-07 02:00:00 CEST" "2020-07-28 23:00:00 CEST"
#prep predictors
  #meteo: Temp, rH, cloud cover, wind, stability (30min res)
      #GeoDach: cloud cover
meteo_geo<-read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach2020_withcloudcover.csv",
           sep=";", dec=",", skip=20000, header=F, nrow=20000,fill = T, na.strings = c("-"),
           col.names = read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach2020_withcloudcover.csv",
                                                                      sep=";", dec=",", skip=2, header=F, nrow=1))
#change colnames
colnames(meteo_geo)[1] <- "date"
#subset to parameters needed
meteo_geo<-meteo_geo[,c(1,23)]
str(meteo_geo)
#meteo_geo$tcc<-as.factor(meteo_geo$tcc)
#format timestamp
meteo_geo$date<-strptime(meteo_geo$date, format="%d.%m.%Y %H:%M", tz="Europe/London") #tz is russiun solution =eternal wintertime
meteo_geo$date<-as.POSIXct(meteo_geo$date, tz="Europe/London")
#round to hour
meteo_geo=timeAverage(meteo_geo,avg.time = "hour")
meteo_geo$tcc<-round(meteo_geo$tcc, digits = 0)
meteo_geo$date<-as.POSIXct(meteo_geo$date, tz="Europe/London")
#convert to summertime to match Steinf and Logger 
meteo_geo <- with_tz(time = meteo_geo, tzone = "CET")
#subet to timespan needed
meteo_geo<-meteo_geo[meteo_geo$date>=starttime&meteo_geo$date<=endtime,]
      #Steinf: Temp, rH,  stability (calculated from temp) #ACHTUNG ewige Sommerzeit
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_SteinfurterStr/2020")
meteo_steinf<-read.table(file = "meteo_data_2020.csv", sep=";", dec=",",
                         header=T)
str(meteo_steinf)
#subset to perameters needed
meteo_steinf<-meteo_steinf[,c(1,5,8,11)]
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
meteo_steinf<-meteo_steinf[meteo_steinf$date>=starttime&meteo_steinf$date<=endtime,]
        #Steinf: Wind 
wind_steinf<-read.csv(file="C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_SteinfurterStr/wind_30min.csv")
wind_steinf$date<-strptime(wind_steinf$timestamp_start, format="%Y-%m-%d %H:%M:%S")
#subset to parameters needed
wind_steinf<-wind_steinf[,c(4,8,14)]
wind_steinf$date<-as.POSIXct(wind_steinf$date)
#aggregate by hour
wind_steinf=timeAverage(wind_steinf,avg.time = "hour")
#subset to timespan needed
wind_steinf<-wind_steinf[wind_steinf$date>=starttime&wind_steinf$date<=endtime,]

#combine all meteo data
str(meteo_steinf)
str(meteo_geo)
str(wind_steinf)
meteo <- data.frame("datetime"=all_temp$datetime, 
                    "meteo_Temp"=meteo_steinf$AirTC2m_Avg,
                    "meteo_rH"=meteo_steinf$RH2m_Avg,
                    "meteo_stability" =meteo_steinf$stability,
                    "meteo_cloudcover"=meteo_geo$tcc,
                    "meteo_windspeed"=wind_steinf$mean_windspeed)
meteo$meteo_stability<-as.factor(meteo$meteo_stability)
