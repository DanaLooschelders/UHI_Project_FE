#Netatmo
#QAQC Netatmo (Meier, Fenner et al. 2017 - 
#Crowdsourcing air temperature from citizen science 
#weather stations for urban climate research)
library(ggplot2)
library(ggforce)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Netatmo/")
#**************************************************************************
#Data Quality Level A - inconsistent Metadata
#*****************************************************************************
NAs=sapply(list_netatmo_June, function(x) sum(is.na(x$temperature))) #none
any(NAs>0)
NAs

#filter out stations with no lat/lon value
any(is.na(metadata_merge$lon)) #FALSE
any(is.na(metadata_merge$lat)) #FALSE
#Check if stations have identical metadata (remove if >2)
any(duplicated(metadata_merge[2:9])) #FALSE
#skip check for monthly NAs because timespan is too short

#****************************************************************************
#Data Quality Level B - identify real outdoor measurements
#****************************************************************************
#Level B part 1 
#*************************************************************************
#five times the sd in TNref (arithmetic mean ofUCON and DWD stations)
#and in SDref.
#calculate daily min air temp and sd 
list_netatmo_level_B=list_netatmo_June #create output list

#create output dataframe
daily_min_table=data.frame("date"=unique(list_netatmo_June[[1]]$Date), 
                           "daily_min"=rep(NA), "SD"=rep(NA))
for (i in 1:length(list_netatmo_June)){
  data=list_netatmo_June[[i]]
  for (x in data$Date){
    daily_min_table$daily_min[daily_min_table$date==x]=min(data$temperature[data$Date==x], na.rm=T)
    daily_min_table$SD[daily_min_table$date==x]=sd(data$temperature[data$Date==x], na.rm=T)
  }
  list_netatmo_level_B[[i]]=daily_min_table
}
daily_min_table
#temp -> DWD reference data
daily_min_ref=data.frame("date"=seq.Date(from=as.Date("2020-06-09", 
                                                      tz="Europe/Berlin"), 
                                         to=as.Date("2020-06-18", tz="Europe/Berlin"), by=1), "daily_min"=rep(NA), "SD"=rep(NA))

for (x in daily_min_ref$date){
  daily_min_ref$daily_min[daily_min_ref$date==x]=min(temp$TT_TU[as.Date(temp$MESS_DATUM, tz="Europe/Berlin")==x], na.rm=T)
  daily_min_ref$SD[daily_min_ref$date==x]=sd(temp$TT_TU[as.Date(temp$MESS_DATUM, tz="Europe/Berlin")==x], na.rm=T)
}


#scatterplot mean temp vs SD
month="Juni"
list_netatmo_level_B_june=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B", tz="Europe/Berlin")==month))
#caluculate monthly means for reference data

mean_june_temp_ref=mean(daily_min_ref$daily_min[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)
mean_june_sd_ref=mean(daily_min_ref$SD[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)

sd_june_temp_ref=sd(daily_min_ref$daily_min[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)
sd_june_sd_ref=sd(daily_min_ref$SD[strftime(daily_min_ref$date, "%B", tz="Europe/Berlin")==month], na.rm=T)

#calculate monthly means for netatmo data
mean.june=data.frame("ID"=names(list_netatmo_level_B_june), 
                    "mean_min_temp"=sapply(list_netatmo_level_B_june, function(x) mean(x$daily_min, na.rm=T)),
                    "mean_sd"=sapply(list_netatmo_level_B_june, function(x) mean(x$SD, na.rm=T)))

test=
  ggplot(data=mean.june, aes(mean_min_temp, mean_sd))+
  geom_point()+ #netatmo mean monthly daily min values
  geom_point(aes(x=mean(mean.june$mean_min_temp, na.rm=T), y=mean(mean.june$mean_sd, na.rm=T)), color="green", shape=15)+ #one point for netatmo mean and sd
  #one point for reference data mean and sd point
  geom_point(aes(x=mean_june_temp_ref, y=mean_june_sd_ref), color="red", shape=15)+
  #ellipse for 5 times the sd for mean and sd of ref
  geom_ellipse(aes(a=sd_june_sd_ref*5, x0=mean_june_temp_ref, b=sd_june_temp_ref*5, y0=mean_june_sd_ref, angle=0))
#ggsave(filename = paste("Level_B_1_netatmo",month,".pdf"), width=14, height=7)

#how to: geom_ellipse
#x0 -> center coordinate on x axis
#y0 -> center coordinate on y axis
#a -> length of ellipse on y axis
#b -> length of ellipse on x axis
#angle 

#ggplot_built: builds a ggplot for rendering 
#(outputs a list of dataframe s-> one for each layer)
#and a panel object with axis limits/breaks etc
built_whole=ggplot_build(test)
built <- ggplot_build(test)$data
points <- built[[1]] #first list element are the black points

#ell <- built[[4]] #forth list element is the ellipse
ell <- built[[4]][built[[4]]$group == built[[4]]$group[1],] 

dat <- data.frame(
  "ID"=built_whole[["plot"]][["data"]][["ID"]],
  points[1:2], #first two columns are the coordinates
  in.ell = as.logical(point.in.polygon(point.x=points$x, point.y=points$y, pol.x=ell$x, pol.y=ell$y)))


#use for loop to exclude station that were flagged as false
for (i in dat$ID){
  if (dat$in.ell[dat$ID==i]==FALSE){
    #remove station from both lists
    list_netatmo_June[[i]]=NULL
    list_netatmo_level_B[[i]]=NULL
    metadata[metadata_merge$device_id==i,] #delete from metadata
  }else{}
}


#*************************************************************************
#level B part 2
#*************************************************************************

#Instructions from paper
#1. compute histograms of TNref and SDref 
#bin sizes: max and min of TNref and SDref in ellipse
#bin numbers: 10
#subset table to August values
daily_min_ref_june=daily_min_ref[strftime(daily_min_ref$date, "%B")=="Juni",]


#2.compute relative frequency for every bin combination of histograms of TN/SD (2D)
#create a subset list that includes only August values
list_netatmo_level_B_june=lapply(list_netatmo_level_B, function(x) subset(x, strftime(x$date, "%B", tz="Europe/Berlin")=="Juni"))
#calculate mean minimal temperature and standard deviation for every netatmo station
mean.june=data.frame("ID"=names(list_netatmo_level_B_june), 
                    "mean_min_temp"=sapply(list_netatmo_level_B_june, function(x) mean(x$daily_min)),
                    "mean_sd"=sapply(list_netatmo_level_B_june, function(x) mean(x$SD)))

#use ggplot to plot 2D histogramm
n <- ggplot(mean.june, aes(mean_min_temp, mean_sd)) 
n <- n + stat_bin2d(bins = 10)
n
#get density values from plot
hist=ggplot_build(n)

#3. flag Netatmo stations in 2D bin with frequency >0.001 as TRUE
#briefly check if any station is below 0.01
any(hist$data[[1]]$density<0.001) #FALSE

#ggplot(bind_rows(list_netatmo_merge, .id="df"), aes(Datetime, temperature, colour=df)) +
# geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
#  theme(legend.position="none")
#ggsave(filename = "overview_netatmo_levelA_B.pdf", width=14, height=7)
library(dplyr)
library(zoo)
#*************************************************************************
#Data Quality Level C - filter systematic/single radiative errors
#*************************************************************************
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Netatmo/")
rad=read.table("GeoDach_2020.csv", sep=",", dec=".", header=T, na.strings = "-" )
#str(rad)
names(rad)[1]="Datetime" #rename first column
rad=data.frame("Datetime"=rad$Datetime, "SWrad"=rad$Shortwave.Radiation, "Temperature"=rad$Temperature) #create column with just two variables
rad$Datetime=strptime(rad$Datetime, format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin") #convert to Posixlt
rad$Datetime=as.POSIXct(rad$Datetime, tz="Europe/Berlin") #convert to Posixct
rad=rad[complete.cases(rad),] #remove rows with all NAs
range(list_netatmo_level_B_june[[1]]$date)
#subset data to timeframe
rad2=rad[rad$Datetime>="2020-06-09 00:00:00"&rad$Datetime<="2020-06-18 23:59:59",]
#str(rad2) #check
#aggregate data to hourly means
rad2$Hour <- cut(as.POSIXct(rad2$Datetime, 
                            format="%Y-%m-%Y %H:%M:%S"), breaks="hour",
                 tz="Europe/Berlin") #create new column with hour
hourly_rad <- aggregate(cbind(Temperature,SWrad) ~ Hour, rad2, mean) #aggregate values to hourly means
hourly_rad$Hour=as.POSIXct(hourly_rad$Hour)
rm(rad, rad2) #tidy script by removing dataframes

#aggregate netatmo data to hourly means
list_netatmo_hourly=list_netatmo_June #create new list
#for loop to aggregate data by hour for each station
for (i in 1:length(list_netatmo_hourly)){
  data=list_netatmo_June[[i]]
  data$Hour = cut(as.POSIXct(data$Datetime, 
                             format="%Y-%m-%d %H:%M:%S",
                             tz="Europe/Berlin"), breaks="hour")
  hourly= aggregate(temperature ~ Hour, data, mean, na.action=na.pass)
  hourly$Hour=as.POSIXct(hourly$Hour, format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin")
  list_netatmo_hourly[[i]]=hourly
}

#add month indices back to list
#add column with month index for August and September
#add month index to dataframe
#create output list
list_netatmo_month <- lapply(list_netatmo_hourly, `[`, 1)
list_netatmo_month=lapply(list_netatmo_month, function(x) strftime(x$Hour, "%B", tz="Europe/Berlin"))
list_netatmo_level_C <- mapply(cbind, list_netatmo_hourly, "month"=list_netatmo_month, SIMPLIFY=F)
rm(list_netatmo_month)
#filter systematic errors
for (i in 1:length(list_netatmo_level_C)){
  #create one dataframe
  data=cbind(hourly_rad, list_netatmo_level_C[[i]]$temperature)
  #name dataframe
  names(data)=c("Hour", "ref_Temperature","SWrad","netatmo_Temperature")
  #calculate Temperature difference between Netatmo and reference station
  data$Temp_diff=data$netatmo_Temperature-data$ref_Temperature
  #compute pearson correlation between radiation and 
  #temperature difference for all rad values >10 Wm2
  cor_rad=cor.test(data$SWrad[data$SWrad>10],data$Temp_diff[data$SWrad>10], method = "pearson")
  #test if correlation has p-value <0.01 and correlation >0.5
  #then station has systematic radiative error and is removed
  if(cor_rad$p.value<0.01&cor_rad$estimate>0.5){
    list_netatmo_hourly[[i]]=NULL #remove station
  }else{}
}

#single radiative errors
#test if single values need to be filtered out
#filter temp diff values that are >3 times SD of ref temp
#for August and September

for (i in 1:length(list_netatmo_level_C)){
  #create one dataframe
  data=cbind(hourly_rad, list_netatmo_level_C[[i]]$temperature, list_netatmo_level_C[[i]]$month)
  #name dataframe
  names(data)=c("Hour", "ref_Temperature","SWrad","netatmo_Temperature", "month")
  #calculate Temperature difference between Netatmo and reference station
  data$Temp_diff=data$netatmo_Temperature-data$ref_Temperature
  SD_june=sd(data$ref_Temperature[data$month=="Juli"], na.rm=T)
  #SD_sep=sd(data$ref_Temperature[data$month=="September"], na.rm=T)
  for (x in 1:length(data$Temp_diff[data$month=="Juli"])){
    value=data$Temp_diff[data$month=="Juli"][x]
    if( any(value>SD_june*3,value<SD_june*-3, is.na(value))){
      #if single value differs more then SD*3 in any direction set NA
      data$Temp_diff[data$month=="Juli"][x]=NA 
    }else{}
    #replace corrected values
  }#for September
  #for (y in 1:length(data$Temp_diff[data$month=="September"])){
  #value=data$Temp_diff[data$month=="September"][y]
  #if( any(value>SD_sep*3,value<SD_sep*-3, is.na(value))){
  #if single value differs more then SD*3 in any direction set NA
  # data$netatmo_Temperature[data$month=="September"][y]=NA 
  #}else{}
  #}
  list_netatmo_level_C[[i]]$temperature=data$netatmo_Temperature 
}

#ggplot(bind_rows(list_netatmo_level_C, .id="df"), aes(Hour, temperature, colour=df)) +
#  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')+
#  theme(legend.position="none")
#ggsave(filename = "overview_netatmo_level_C.pdf", width=14, height=7)


#and create new output list
list_netatmo_level_D = list_netatmo_level_C
#******************************************************************************
#Data Quality Level D -  outliers
#**********************************************************************************
level_D=function(month="Juni"){
  for (i in 1:length(list_netatmo_level_D)){
    data=list_netatmo_level_D[[i]]
    data_month=data[data$month==month,]
    sd_month=sd(data_month$temperature, na.rm=T)
    for (x in 1:length(data_month$temperature)){
      value=data_month$temperature[1]
      if (any(value > sd_month*3, value< sd_month*-3,is.na(value))) {
        data_month$temperature[x]=NA #set value NA
      }else{} #keep value
    }
    data[data$month==month,]=data_month #put controlled data back in dataframe
  } 
  list_netatmo_level_D[[i]]=data #put controlled data back in list
  return(list_netatmo_level_D)
}

list_netatmo_level_D=level_D(month="Juni")
#list_netatmo_level_D_2=level_D(month="September")

#ggplot(bind_rows(list_netatmo_level_D, .id="df"), aes(Hour, temperature, colour=df)) +
#  geom_line()+theme_bw()+ylab("Temperature [°C]")+xlab("Date")+ labs(color='Netatmo devices in MS')
#ggsave(filename = "overview_netatmo_level_D.pdf", width=14, height=7)

#update metadata table
#shorten metadatalist by excluding the IDs that had no data
rownames(metadata_merge)=metadata_merge$device_id #set ID as rownames
ids_to_keep=names(list_netatmo_level_D) #get character vector of ids to keep
metadata_Juni=metadata_merge[ids_to_keep,] #subset metadata with ids from data

#save metadata_merge to csv file
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Netatmo/")
write.csv2(file="Netatmo_metadata_Juni.csv", metadata_Juni)

rm(ids_to_keep, list_netatmo_hourly)

#transform coordiantes to lat lon and create spatial points
#points=SpatialPointsDataFrame(coords = metadata_merge[2:3], 
#                            proj4string=CRS("+proj=longlat +datum=WGS84"),
#                           data=metadata_merge)

#final test: plotting points in shapefile
#leaflet(MS_shape) %>%
#  addPolygons() %>%
#  addTiles() %>%
#  addCircles(data=points)
list_netatmo_only_temp <- lapply(list_netatmo_level_D, `[`, 2)
dataframe_netatmo<-as.data.frame(do.call(cbind, list_netatmo_only_temp))   
colnames(dataframe_netatmo)<-names(list_netatmo_level_D)
dataframe_netatmo$datetime<-list_netatmo_level_D[[1]]$Hour
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Netatmo/")
write.csv(dataframe_netatmo, file = "Netatmo_2020_Juni.csv")
