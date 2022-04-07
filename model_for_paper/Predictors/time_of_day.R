library(raster) 
library(readr)
library(dplyr)
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/")

sunrise_dates <- read_csv2("Time_of_day/sunrise_dates.csv")
sunrise_w_date <- read_csv("Time_of_day/sunrise_with_date.csv")

sunrise <- merge(sunrise_dates, sunrise_w_date, by.x = "Sunrise", by.y = "Sunrise")
sunrise <- sunrise[-c(2,3,160,161,258,259,372,373), ] #manually remove doubled dates 

sorted <- sunrise[order(sunrise$day),]
sorted$ID <- seq.int(nrow(sorted))

times_06 <- sorted[sorted$ID %in% c(157:171), ]
times_07 <- sorted[sorted$ID %in% c(185:213), ]

hourly <- data.frame(seq(ISOdate(2020,06,05, hour = 0),ISOdate(2020,06,19, hour = 23),by = "hour"))
names(hourly) <- c("date")
hourly <- hourly %>% mutate(day = substr(date,1L,10L))
hourly$day <- as.Date(hourly$day)

hourly_times_06 <- merge(times_06, hourly, by.x = "day", by.y = "day")

hourly_times_06$time <- format(as.POSIXct(hourly_times_06$date), format = "%H:%M:%S")

library(hms) 
hourly_times_06$time<-as_hms(hourly_times_06$time) 
class(hourly_times_06$time) 

as.POSIXct(hourly_times_06$date,"%Y-%m-%d %H:%M:%S")
as.POSIXct(hourly_times_06$Sunset,"%H:%M:%S")
as.POSIXct(hourly_times_06$Sunrise,"%H:%M:%S")

time_s_sunrise <-  ifelse(as.numeric(hourly_times_06$time) >= as.numeric(hourly_times_06$Sunrise) & (as.numeric(hourly_times_06$time) < as.numeric(hourly_times_06$Sunset)),
                difftime(hourly_times_06$time,hourly_times_06$Sunrise,units="hours"),
                NA) 

time_s_sunset <-  ifelse(as.numeric(hourly_times_06$time) <= as.numeric(hourly_times_06$Sunrise) | (as.numeric(hourly_times_06$time) > as.numeric(hourly_times_06$Sunset)),
                         difftime(hourly_times_06$time,hourly_times_06$Sunset,units="hours"),
                         NA)

hourly_times_06$time_s_sunrise <- time_s_sunrise
hourly_times_06$time_s_sunset <- time_s_sunset

#06/05 - 06/19          2020-06-05 00:00:00 bis 2020-06-19 00:00:00
#07/03 - 07/31          2020-07-03 00:00:00 bis 2020-07-31 00:00:00