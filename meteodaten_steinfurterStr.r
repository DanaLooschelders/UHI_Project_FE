#Meteodaten auslesen
#von Dana Looschelders
#08.03.2021

#Manuell:
#Meteodaten mit LoggerNet auslesen
#in Ordner abspeichern

library(dplyr)
library(ggplot2)
library(tidyr)

#####Meteodaten####
setwd("Z:/junk/Dana/UHI_Steinfurter/2020_convert/")
#list files to read in 
files_list=list.files(pattern ="\\.dat")
#read in Metadata
metadata_meteo=read.table(files_list[1], sep = ",", dec = ".", header = F,skip=1,
                          na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)

metadata_meteo=metadata_meteo[1:3,]
#create header
header=metadata_meteo[1,]

for(i in files_list) {
  if(!exists("meteo_data")) {
    j=1
    meteo_data=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                          na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
  } else {
    j=j+1
    temp_meteo_data=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                               na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
    meteo_data=rbind(meteo_data, temp_meteo_data)
    remove(temp_meteo_data)
  }
}
#set header
colnames(meteo_data)=header

#QAQC
str(meteo_data)
#convert to POSIXct
#Achtung die Daten liegen in ewiger Sommerzeit vor!
meteo_data$TIMESTAMP=strptime(meteo_data$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
str(meteo_data$TIMESTAMP)
#sort by date
meteo_data=arrange(meteo_data, TIMESTAMP)
#meteo_data=meteo_data[1:65224,]#the values afterwards are somehow wrong
#change colname of TIMESTAMP
colnames(meteo_data)[1]="timestamp"
#coerce to POSIXct
str(meteo_data$timestamp)
meteo_data$timestamp=as.POSIXct(meteo_data$timestamp,tz="")
#check that timestamp is continous (due to missing files)
if(any(which(c(FALSE, diff(meteo_data$timestamp) > 10)))){
  #create timestamp vector
  timestamp=seq.POSIXt(from=meteo_data[1,1], to=meteo_data[length(meteo_data$timestamp),1], by="10 min")
  #coerce to dataframe
  timestamp=as.data.frame(timestamp)
  #Daten zuordnen (so dass leere Zeilen als NA dargestellt werden)
  meteo_data_2=merge(x=timestamp, y=meteo_data, by="timestamp", all.x = T)
}else{}
range(meteo_data$timestamp, na.rm=T)

#meteo_data_subset=meteo_data[meteo_data$timestamp>="2021-03-12 00:00:00"&meteo_data$timestamp<="2021-03-22 00:00:00",]

#get data in tidy format
meteo_long=meteo_data%>%
  pivot_longer(c(-timestamp), 
               names_to="variable", 
               values_to = "value")
#Variablen plotten
meteo_long%>%
  ggplot(aes(timestamp,value))+
  geom_line()+
  facet_wrap(~variable, scales="free")+
  theme_minimal()

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Meteo_Daten/Meteo_SteinfurterStr/2020")

ggsave(filename = "meteo_quickview_2020.pdf", device = "pdf",
       width = 40, height = 20, units = "cm")

#save as .csv
write.csv2(meteo_data, file="meteo_data_2020.csv", row.names=F)