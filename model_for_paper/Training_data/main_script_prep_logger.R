####read in first logger data####
# R-Script for QAQC check, calculation of statistics and plotting of Thermochron iButton temperature loggers
# Date: 2019/07/05
# Author: Laura Ehrnsperger

# Sensor list:
# - DS1921-F5 and MF1921G Thermochron iButton 1-wire digital thermometer, Maxim Integrated (ID: 1-110)
# - HC2-S3 Thermometers, Campbell Scientific

# Changelog:
# 2019/07/05: Creation of script: Reading in csv-files, calculation of mean and standard deviation, plotting of single file
# 2019/07/07: Convertion of timestamp to POSIXct format for both single and list of files
# 2019/07/08: Splitting list for calculation, create statistics data table
# 2019/07/10: Extracting the individual ID of each iButton and assign them to the data file names
# 2019/07/16: Creating index to subset by timestamp
# 2019/07/18: Reading in data of reference thermometers, plotting time series, calculating means of reference temperature
# 2019/07/24: Calculation of offset for each iButton, adding extra column to statistics table, export of statistics table
# 2019/07/25: Plotting of offsets, 


# --------------------------------------------------------------------------------------------------------------------------------
#### Global options ####

# Empty console
cat("\014")

# Empty workspace
# Uncomment if necessary
rm(list = ls())

# Adapt R-settings to display of more digits
options(digits.secs=3)
options(digits=2)

# --------------------------------------------------------------------------------------------------------------------------------
### Predefine functions for later use ###
###
# Function to check if a package is already installed
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# --------------------------------------------------------------------------------------------------------------------------------
# Load required packages
# Packages for graphics in R
usePackage("ggplot2")
# Packages to order data
usePackage("magrittr")
usePackage("plyr")
usePackage("dplyr")
usePackage("reshape2")   
usePackage("stringr")
usePackage("tidyverse")
usePackage("zoo")
usePackage("xts")

####Source other scripts####
source(file="C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/UHI_Project_FE/model_for_paper/Training_data/prep_june2020_logger.R")
source(file="C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/UHI_Project_FE/model_for_paper/Training_data/prep_first_July2020_logger.R")
source(file="C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/UHI_Project_FE/model_for_paper/Training_data/prep_second_July2020_logger.R")

#clear wd() again
rm(list = ls())

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger")
#read in lists
juneiButton<-readRDS("JuneiButton.RData")
julyfirstiButton<-readRDS("JulyfirstiButton.RData")
julysecondiButton<-readRDS("JulysecondiButton.RData")

#create empty time series for June logger
#empty_list <- vector(mode = "list", length = 32)
#names(empty_list)<-names(julyfirstiButton)
juneiButton_long<-julyfirstiButton #create list with all Logger IDs
dummydate<-juneiButton[[1]]$Datetime.1 #create dummy data frame

for(i in names(juneiButton_long)){
  if(any(names(juneiButton)==i)){ #if ID matches
    juneiButton_long[[i]]<-juneiButton[[i]] #write data in list
  } else{ #empty list entry
    juneiButton_long[[i]]<-data.frame("Temperature_C"=NA, 
                                      "Datetime.1"=dummydate)
  }
}

#rowbind dataframes two one list
complete_list<-juneiButton_long
for(i in names(juneiButton_long)){
  dat=juneiButton_long[[i]]
  dat_2=julyfirstiButton[[i]]
  dat_3=julysecondiButton[[i]]
  dat_bind=rbind(dat, dat_2, dat_3)
  complete_list[[i]]=dat_bind
}
#plot
ggplot(bind_rows(complete_list, .id="df"), aes(x=Datetime.1, y=Temperature_C, colour=df)) +
  geom_line()
#round date to nearest 10 mins
library(lubridate)

for(i in 1:length(complete_list)){
  complete_list[[i]]$Datetime.1<-round_date(complete_list[[i]]$Datetime.1,
                    unit="10 minutes")
}
#add consecutive and NA in order to dispaly it correctly in the plot 
daterange <- data.frame("Datetime.1"=seq.POSIXt(from=as.POSIXct(complete_list[["35"]][1,2]),
                        to=as.POSIXct(complete_list[["35"]][dim(complete_list[[1]])[1],2]),
                        by="10 min")) #create complete timeframe

library(dplyr)
for(i in 1:length(complete_list)){
  #merge
  dataframe<-left_join(daterange, complete_list[[i]])
  #reorder columns
  dataframe<-dataframe %>% select(Temperature_C, everything())
  complete_list[[i]]<-dataframe
  }
  


complete_list_date <- lapply(complete_list, `[`, 2)
complete_list_date <- lapply(complete_list_date, function(x) as.POSIXct(x$Datetime.1,format="%Y-%m-%d %H:%M:%S"))
complete_list = map2(complete_list, complete_list_date, ~ mutate(., Datetime.1 = .y)) 

ggplot(bind_rows(complete_list, .id="df"), aes(x=Datetime.1, y=Temperature_C, colour=df)) +
  geom_line()

setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/UHI_Project_FE/model_for_paper/Training_data/")
save(complete_list, file="listiButtons.rData")

list_iButton_only_temp <- lapply(complete_list, `[`, 1)
dataframe_logger<-data.frame(do.call(cbind, list_iButton_only_temp))   
colnames(dataframe_logger)<-names(complete_list)
dataframe_logger$datetime<-complete_list[[1]]$Datetime.1
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger")
write.csv(dataframe_logger, file = "Logger_2020.csv")

