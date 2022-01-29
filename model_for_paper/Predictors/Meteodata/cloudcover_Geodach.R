#GeoDach: cloud cover
#read in 2019 data
geo_2019<-read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach_2019.csv",
                      sep=",", dec=".", skip=2000, header=F, nrow=20000,fill = T, na.strings = c("-"),
                      col.names = read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach_2019.csv",
                                             sep=",", dec=".", skip=0, header=F, nrow=1))
#read in 2020 data
geo_2020<-read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach2020_withcloudcover.csv",
                     sep=";", dec=",", skip=20000, header=F, nrow=20000,fill = T, na.strings = c("-"),
                     col.names = read.table("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_GeoDach/GeoDach2020_withcloudcover.csv",
                                            sep=";", dec=",", skip=2, header=F, nrow=1))
meteo_geo<-rbind(geo_2019, geo_2020)
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
