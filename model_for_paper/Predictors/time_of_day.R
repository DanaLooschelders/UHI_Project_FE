library(raster) 
library(readr)
library(dplyr)
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/")
setwd("/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/")

#times
#06/05 - 06/19          2020-06-05 00:00:00 bis 2020-06-19 00:00:00
#07/03 - 07/31          2020-07-03 00:00:00 bis 2020-07-31 00:00:00 

#load data 
sunrise_dates <- read_csv2("Time_of_day/sunrise_dates.csv")
sunrise_w_date <- read_csv("Time_of_day/sunrise_with_date.csv")

#merge and organize date 
sunrise <- merge(sunrise_dates, sunrise_w_date, by.x = "Sunrise", by.y = "Sunrise")
sunrise <- sunrise[-c(2,3,160,161,258,259,372,373), ] #manually remove doubled dates 

sorted <- sunrise[order(sunrise$day),]
sorted$ID <- seq.int(nrow(sorted))

times_06 <- sorted[sorted$ID %in% c(157:171), ]
times_07 <- sorted[sorted$ID %in% c(185:213), ]

#add hourly information
hourly <- data.frame(seq(ISOdate(2020,06,05, hour = 0),ISOdate(2020,06,19, hour = 23),by = "hour"))
names(hourly) <- c("date")
hourly <- hourly %>% mutate(day = substr(date,1L,10L))
hourly$day <- as.Date(hourly$day)

hourly_times_06 <- merge(times_06, hourly, by.x = "day", by.y = "day")

hourly_times_06$time <- format(as.POSIXct(hourly_times_06$date), format = "%H:%M:%S")

#as posixct 
library(hms) 
hourly_times_06$time<-as_hms(hourly_times_06$time) 
class(hourly_times_06$time) 

as.POSIXct(hourly_times_06$date,"%Y-%m-%d %H:%M:%S")
as.POSIXct(hourly_times_06$Sunset,"%H:%M:%S")
as.POSIXct(hourly_times_06$Sunrise,"%H:%M:%S")

#time since sun rise 
hours_ssr <-  ifelse(as.numeric(hourly_times_06$time) >= as.numeric(hourly_times_06$Sunrise) & (as.numeric(hourly_times_06$time) < as.numeric(hourly_times_06$Sunset)),
                difftime(hourly_times_06$time,hourly_times_06$Sunrise,units="hours"),
                NA) 

hourly_times_06$hours_ssr <- hours_ssr

#time since sunset
hourly_times_06$hours_sss<-NA 

for(i in 24:nrow(hourly_times_06)){ #start after first day
  if(hourly_times_06$time[i]>hourly_times_06$Sunset[i]){ #if it is after sunset the same day
    hourly_times_06$hours_sss[i]<-difftime(hourly_times_06$time[i],hourly_times_06$Sunset[i],units="hours")
  } else{#if it is before sunset, calculate time since last days sunset
    sunsettime<-hourly_times_06$Sunset[hourly_times_06$day==as.Date(hourly_times_06$day[i])-1][1]#get last days sunset time
    hourly_times_06$hours_sss[i]<-difftime(hourly_times_06$time[i],sunsettime,units="hours")+24#calculate hours
  }
} 
