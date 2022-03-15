library(raster) 
library(readr)

setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/")

sunrise_dates <- read_csv2("Time_of_day/sunrise_dates.csv")
sunrise_w_date <- read_csv("Time_of_day/sunrise_with_date.csv")

sunrise <- merge(sunrise_dates, sunrise_w_date, by.x = "Sunrise", by.y = "Sunrise")
sunrise <- sunrise[-c(2,3,160,161,258,259,372,373), ] #manurally remove doubled dates 

sorted <- sunrise[order(sunrise$day),]
sorted$ID <- seq.int(nrow(sorted))

times_06 <- sorted[sorted$ID %in% c(157:171), ]
times_07 <- sorted[sorted$ID %in% c(185:213), ]


meteo_geo<-meteo_geo %>% 
  group_by(Sunrise) %>% #group by sunrise --> spans from one sunrise to next
  mutate(cum_radiation = cumsum(Shortwave.Radiation)) #calculate cumsum per day


#damit kann man nach Gruppen kumuliert aufsummieren, falls du das für die Stunden seit Sonnenaufgang brauchst
#06/05 - 06/19          2020-06-05 00:00:00 bis 2020-06-19 00:00:00
#07/03 - 07/31          2020-07-03 00:00:00 bis 2020-07-31 00:00:00