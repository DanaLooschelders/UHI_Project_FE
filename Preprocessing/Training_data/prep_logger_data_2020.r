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


# --------------------------------------------------------------------------------------------------------------------------------
### Read in data ###

# Set Working directory (wd)
# setwd(choose.dir()) # Uncomment if necessary, but only works for Windows operating system
#setwd("V:/klima/Projekte/2019_Urban_Heat_Island/Data/Data_raw/Calibration_test_20190704-20190708")
#setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/logger_data/UHI_20200703-20200717/")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Temp_Logger/UHI_20200703-20200717")
#the logger IDs 56 and 102 were added manually to the .csv files as they were missing in the original file
#later, ID 102 was corrected to 93 because 102 was the old ID from Stiftherrenstrasse 
#and 93 was the true missing ID from Spiekerhof vegetation
#################
# Data iButtons #
#################
# Two different ways of reading in data:
# 1. Read in a single file: If you only need to control a single iButton etc. use this code
# 2. Read in several files that are in the working directory: 
#    If you want to look at multiple iButtons, apply statistics to all data etc. use this code

###
# Column names for iButton data
# 1. Select header from one of the iButton-Files 
# UNCOMMENT IF NEEDED
#iButton_header=read.table("3F000000517B3D21_190708.csv", sep = ",", dec = ".", header = F, skip = 7,
#                          nrows = 1, as.is = T) 

# Customized header
# Create new header as the default one from the files isn't really pretty
iButton_header = c("Datetime", "Temperature_C")

# Create vector containing ID #
# We only need the ID # without the first (empty) column, so we set the first column to NULL in colClasses
#--> changed name (to second file from raw data - UHI timeframe)
iButton_ID=read.table("0800000051790E21_200717.csv", sep = ",", dec = ".", header = F, skip = 4,
                      nrows = 1, as.is = T, colClasses=c("NULL", NA)) 


# 1. Read in a single file 
# Adjust file name to the exact file you want to read in!
# read.csv automatically assumes a comma as separator, which is fine for us.
# The last row contains an end character. This causes trouble while reading in, therefore we skip the last line of each file.
# We also skip the first 8 lines, which only contain meta data on the specific iButton. The customized header is added 
# afterwards with colnames().

#d_iButton_single = read.csv(text=paste0(head(readLines("0D000000519CE121_190814.csv"), -1)), dec = ".", skip = 8, header = F, 
#                           na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE, encoding = 'UTF-8')

# Add correct column names to dataframe
#colnames(d_iButton_single) <- iButton_header

#---> check data
#str(d_iButton_single)
# Transform date time column from text to POSIXct format
#outcommented ---> d_iButton_single$Datetime = as.POSIXct(d_iButton_single$Datetime, format = "%d-%m-%Y %H:%M")
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
#d_iButton_single$Datetime = as.POSIXct(d_iButton_single$Datetime, format = "%Y-%m-%d %H:%M")

###
# 2. Read in several files into separate dataframes
# List all files you want to read in by choosing them by name/file type, etc. with "pattern".
# Here every file, which ends with ".csv" is chosen from the current wd.
files_iButtons=list.files(pattern =".csv")  


# Loop to read in all files in the list into separate data.frames
for (i in 1:length(files_iButtons)) assign(files_iButtons[i], read.csv(text=paste0(head(readLines(files_iButtons[i]), -1)), 
                                                                       sep = ",", dec = ".", header = F, skip = 8, 
                                                                       na.strings = c("<NA>", "NA", "NAN"), 
                                                                       stringsAsFactors = FALSE))


### 
# Read in ID from all files from the list and bind them together
# CAUTION: The files have to have the SAME number of columns!
for(i in files_iButtons) {
  if(!exists("iButton_ID_multi")) {
    iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                nrows = 1, as.is = T, colClasses=c("NULL", NA))
  } else {
    temp_iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                     nrows = 1, as.is = T, colClasses=c("NULL", NA))
    iButton_ID_multi=rbind(iButton_ID_multi, temp_iButton_ID_multi)
    remove(temp_iButton_ID_multi)
  }
}

# To check if all iButtons we want are read in, we sort ID # ascending and then display all ID # that occur
iButton_ID_sort = sort(iButton_ID_multi$V2)
iButton_ID_sort

# Create new names for the data files out of ID, serial number (SR) and date of data collection
# First get rid of the file ending .csv in the file name
iButton_SR_date = sapply(strsplit(files_iButtons, "\\."), "[", 1)

# Paste the ID vector with the names derived from the files_iButton list
list_header <- paste(iButton_ID_multi$V2)
list_header
# Select all csv-files and put them in one list
# By having all dataframes in the same list, you can apply changes to all files simultanuously, e.g. renaming.
list_iButton <- mget(ls(pattern =  ".csv"))

# Assign new file names to every iButton-file in list "list_iButton"
names(list_iButton) <- list_header

# Assign the same column names to every iButton-file in list "list_iButton"
list_iButton = lapply(list_iButton, setNames, nm = iButton_header)


# --------------------------------------------------------------------------------------------------------------------------------
# Add timestamp to all iButton-files
# First select only the first column (=timestamp of our data) of each file in the list
list_iButton_datetime <- lapply(list_iButton, `[`, 1)

# Transform timestamp from character to POSIXct format for each file in the sublist list_iButton_datetime
# We get a sublist only containing POSIXct datetime format for each iButton
# outcommented ---> list_iButton_datetime <- lapply(list_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%d-%m-%Y %H:%M"))
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
list_iButton_datetime <- lapply(list_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%Y-%m-%d %H:%M"))


# Add the new transformed timestamp to the original list
# There are two options, uncomment the one you need.
# 1. Add POSIXct timestamp as additional column
list_iButton <- mapply(cbind, list_iButton, "Datetime"=list_iButton_datetime, SIMPLIFY=F)

# 2. Replace the old timestamp with POSIXct timestamp
#list_iButton = map2(list_iButton, list_iButton_datetime, ~ mutate(., Datetime = .y)) 


# --------------------------------------------------------------------------------------------------------------------------------
# Subset data by choosing start and end date of the desired time period
# Unnecessary data can be removed, e.g. data collected before the labtest started.
# For example the data collected during the setting up of iButtons in my office. It contains rather high values of
# about 27 ??C, which should not be included in the statistics.
# If no subset should be supplied, just work with the original data table or enter the dates of the entire period.

# Time period for Labtest
#--> outcommented start_Labtest <- strptime("2019-07-04 16:00:00", "%Y-%m-%d %H:%M:%S")
#---> outcommented end_Labtest <- strptime("2019-07-08 13:55:00", "%Y-%m-%d %H:%M:%S")

#---> chose own time period
range(list_iButton_datetime[[1]])
start_time=strptime("2020-07-07 00:00:00", "%Y-%m-%d %H:%M:%S")
end_time=strptime("2020-07-17 00:00:00", "%Y-%m-%d %H:%M:%S")

# Apply the time index on the single data table
# ---> outcommented d_iButton_single_corr <- subset(d_iButton_single, Datetime >= start_Labtest & Datetime <= end_Labtest)
#---> changed for own time span
#d_iButton_single_corr <- subset(d_iButton_single, Datetime >= start_time & Datetime <= end_time)

# Apply the time index on each data table in the list "list_iButton"
# So for each data table the same time period is selected
#---> outcommented list_iButton_corr = lapply(list_iButton, function(x) {subset(x, x[,1] >= start_Labtest & x[,1] <= end_Labtest)})
#---> changed for own time span
list_iButton_corr = lapply(list_iButton, function(x) {subset(x, x[,1] >= start_time & x[,1] <= end_time)})

rm(list = as.character(files_iButtons)) #remove csv.files from environment
####start time correction####
require(zoo)
require(xts)
require(splines)

#interpolate the data to minute intervalls to set all loggers to the same starting point
#create empty vector of minute data for the same timeframe as temp data
#assume linearity and approximate values 
head(list_iButton_corr)

#Create time sequence by minute
#loop through list
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") #create minute timeframe
list_iButton_corr_set=list_iButton_corr
list_iButton_corr_set=lapply(list_iButton_corr_set, `[`, 2:3) #use only 2nd and 3rd column
#create new list to use as output
for(i in 1:length(list_iButton_corr)){
  #create time series with datetime and temperature
  test=xts(list_iButton_corr[[i]][,2],list_iButton_corr[[i]][,3]) 
  #merge logger time series with emtpy one minute time series
  test2=merge(test,date_time_complete)
  #replace NA values (created by merging with higher res) with spline interpolated values
  test2=na.spline(test2)
  test2=data.frame("Temperature_corr"=test2) #name the new column
  test2$Datetime.1=rownames(test2) #use the newly set times to replace previous time data
  rownames(test2)=NULL #delete rownames
  test3=test2[c(TRUE,rep(FALSE,9)),] #keep only every 10th value to get 10min res
  test3$test=round(test3$test/.5)*.5 #round to .5 decimal place 
  list_iButton_corr_set[[i]][1:length(test3[,1]),1:2]=test3[,1:2] #replace the time and temp column with the new values
  list_iButton_corr_set[[i]]=list_iButton_corr_set[[i]][-length(test3[,1]+1),1:2] #delete last row in every dataframe (sometimes NA)
}

rm(test, test2, test3)
#test spline interpolation
#test=xts(list_iButton_corr[[5]][,2],list_iButton_corr[[5]][,3])
#test2 = merge(test,date_time_complete)
#test_spline=na.spline(test2)
#str(test_spline)
#test_linear=na.approx(test2)
#str(test_linear)

#test_spline=as.data.frame(test_spline)
#test_spline$Datetime.1=rownames(test_spline)
#rownames(test_spline)=NULL
#test_spline$Datetime.1=strptime(x = test_spline$Datetime.1, format="%Y-%m-%d %H:%M:%S")
#test_spline=test_spline[1:length(test_linear),]

#test_linear=as.data.frame(test_linear)
#test_linear$Datetime.1=rownames(test_linear)
#rownames(test_linear)=NULL
#test_linear$Datetime.1=strptime(x = test_linear$Datetime.1, format="%Y-%m-%d %H:%M:%S")

#plot(test_linear$Datetime.1, test_linear$test, type="l", col="green")
#lines(test_spline$Datetime.1, test_spline$test, col="red")
#diff=test_linear$test-test_spline$test
#str(test_linear)
#str(test_spline)
#plot(diff, type="l")

#qqnorm(test_linear$test)
#qqline(test_linear$test)

#wilcox.test(test_linear$test, test_spline$test)
#use only for data from 03.07 to subset all ts to same intervall
range(list_iButton_corr_set[[1]]$Datetime.1)
range(list_iButton_corr_set[[2]]$Datetime.1)
start_time=strptime("2020-07-07 00:00:00", "%Y-%m-%d %H:%M:%S")
end_time=strptime("2020-07-17 00:00:00", "%Y-%m-%d %H:%M:%S")
list_iButton_corr_set = lapply(list_iButton_corr_set, function(x) {subset(x, x[,2] >= start_time & x[,2] < end_time)})

# R-Script for QAQC check, calculation of statistics and plotting of Thermochron second_iButton temperature loggers
# Date: 2019/07/05
# Author: Laura Ehrnsperger

# Sensor list:
# - DS1921-F5 and MF1921G Thermochron second_iButton 1-wire digital thermometer, Maxim Integrated (ID: 1-110)
# - HC2-S3 Thermometers, Campbell Scientific

# Changelog:
# 2019/07/05: Creation of script: Reading in csv-files, calculation of mean and standard deviation, plotting of single file
# 2019/07/07: Convertion of timestamp to POSIXct format for both single and list of files
# 2019/07/08: Splitting list for calculation, create statistics data table
# 2019/07/10: Extracting the individual ID of each second_iButton and assign them to the data file names
# 2019/07/16: Creating index to subset by timestamp
# 2019/07/18: Reading in data of reference thermometers, plotting time series, calculating means of reference temperature
# 2019/07/24: Calculation of offset for each second_iButton, adding extra column to statistics table, export of statistics table
# 2019/07/25: Plotting of offsets, 


# --------------------------------------------------------------------------------------------------------------------------------
#### Global options ####

# Empty console
#cat("\014")

# Empty workspace
# Uncomment if necessary
#rm(list = ls())

# Adapt R-settings to display of more digits
#options(digits.secs=3)
#options(digits=12)

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


# --------------------------------------------------------------------------------------------------------------------------------
### Read in data ###

# Set Working directory (wd)
# setwd(choose.dir()) # Uncomment if necessary, but only works for Windows operating system
#setwd("V:/klima/Projekte/2019_Urban_Heat_Island/Data/Data_raw/Calibration_test_20190704-20190708")
#setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/logger_data/UHI_20200717-20200731/")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/Temp_Logger/UHI_20200718-20200731")
#the logger IDs 56 and 102 were added manually to the .csv files as they were missing in the original file
#later, ID 102 was corrected to 93 because 102 was the old ID from Stiftherrenstrasse 
#and 93 was the true missing ID from Spiekerhof vegetation
#################
# Data second_iButtons #
#################
# Two different ways of reading in data:
# 1. Read in a single file: If you only need to control a single second_iButton etc. use this code
# 2. Read in several files that are in the working directory: 
#    If you want to look at multiple second_iButtons, apply statistics to all data etc. use this code

###
# Column names for second_iButton data
# 1. Select header from one of the second_iButton-Files 
# UNCOMMENT IF NEEDED
#second_iButton_header=read.table("3F000000517B3D21_190708.csv", sep = ",", dec = ".", header = F, skip = 7,
#                          nrows = 1, as.is = T) 

# Customized header
# Create new header as the default one from the files isn't really pretty
second_iButton_header = c("Datetime", "Temperature_C")

# Create vector containing ID #
# We only need the ID # without the first (empty) column, so we set the first column to NULL in colClasses
#--> changed name (to second file from raw data - UHI timeframe)
second_iButton_ID=read.table("0800000051790E21_200817.csv", sep = ",", dec = ".", header = F, skip = 4,
                             nrows = 1, as.is = T, colClasses=c("NULL", NA)) 


# 1. Read in a single file 
# Adjust file name to the exact file you want to read in!
# read.csv automatically assumes a comma as separator, which is fine for us.
# The last row contains an end character. This causes trouble while reading in, therefore we skip the last line of each file.
# We also skip the first 8 lines, which only contain meta data on the specific second_iButton. The customized header is added 
# afterwards with colnames().

#d_second_iButton_single = read.csv(text=paste0(head(readLines("0D000000519CE121_190814.csv"), -1)), dec = ".", skip = 8, header = F, 
#                           na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE, encoding = 'UTF-8')

# Add correct column names to dataframe
#colnames(d_second_iButton_single) <- second_iButton_header

#---> check data
#str(d_second_iButton_single)
# Transform date time column from text to POSIXct format
#outcommented ---> d_second_iButton_single$Datetime = as.POSIXct(d_second_iButton_single$Datetime, format = "%d-%m-%Y %H:%M")
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
#d_second_iButton_single$Datetime = as.POSIXct(d_second_iButton_single$Datetime, format = "%Y-%m-%d %H:%M")

###
# 2. Read in several files into separate dataframes
# List all files you want to read in by choosing them by name/file type, etc. with "pattern".
# Here every file, which ends with ".csv" is chosen from the current wd.
files_second_iButtons=list.files(pattern =".csv")  


# Loop to read in all files in the list into separate data.frames
for (i in 1:length(files_second_iButtons)) assign(files_second_iButtons[i], read.csv(text=paste0(head(readLines(files_second_iButtons[i]), -1)), 
                                                                                     sep = ",", dec = ".", header = F, skip = 8, 
                                                                                     na.strings = c("<NA>", "NA", "NAN"), 
                                                                                     stringsAsFactors = FALSE))


### 
# Read in ID from all files from the list and bind them together
# CAUTION: The files have to have the SAME number of columns!
for(i in files_second_iButtons) {
  if(!exists("second_iButton_ID_multi")) {
    second_iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                       nrows = 1, as.is = T, colClasses=c("NULL", NA))
  } else {
    temp_second_iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                            nrows = 1, as.is = T, colClasses=c("NULL",  NA))
    second_iButton_ID_multi=rbind(second_iButton_ID_multi, temp_second_iButton_ID_multi)
    remove(temp_second_iButton_ID_multi)
  }
}

# To check if all second_iButtons we want are read in, we sort ID # ascending and then display all ID # that occur
second_iButton_ID_sort = sort(second_iButton_ID_multi$V2)
second_iButton_ID_sort

# Create new names for the data files out of ID, serial number (SR) and date of data collection
# First get rid of the file ending .csv in the file name
second_iButton_SR_date = sapply(strsplit(files_second_iButtons, "\\."), "[", 1)

# Paste the ID vector with the names derived from the files_second_iButton list
list_header <- paste(second_iButton_ID_multi$V2)
list_header
# Select all csv-files and put them in one list
# By having all dataframes in the same list, you can apply changes to all files simultanuously, e.g. renaming.
list_second_iButton <- mget(ls(pattern =  ".csv"))

# Assign new file names to every second_iButton-file in list "list_second_iButton"
names(list_second_iButton) <- list_header

# Assign the same column names to every second_iButton-file in list "list_second_iButton"
list_second_iButton = lapply(list_second_iButton, setNames, nm = second_iButton_header)


# --------------------------------------------------------------------------------------------------------------------------------
# Add timestamp to all second_iButton-files
# First select only the first column (=timestamp of our data) of each file in the list
list_second_iButton_datetime <- lapply(list_second_iButton, `[`, 1)
# Transform timestamp from character to POSIXct format for each file in the sublist list_second_iButton_datetime
# We get a sublist only containing POSIXct datetime format for each second_iButton
# outcommented ---> list_second_iButton_datetime <- lapply(list_second_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%d-%m-%Y %H:%M"))
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
list_second_iButton_datetime <- lapply(list_second_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%Y-%m-%d %H:%M"))

list_second_iButton_temp <- lapply(list_second_iButton, `[`, 2)
# Add the new transformed timestamp to the original list
# There are two options, uncomment the one you need.
# 1. Add POSIXct timestamp as additional column
list_second_iButton <- mapply(cbind, list_second_iButton_temp, "Datetime.1"=list_second_iButton_datetime, SIMPLIFY=F)

# 2. Replace the old timestamp with POSIXct timestamp
#list_second_iButton = map2(list_second_iButton, list_second_iButton_datetime, ~ mutate(., Datetime = .y)) 


# --------------------------------------------------------------------------------------------------------------------------------
# Subset data by choosing start and end date of the desired time period
# Unnecessary data can be removed, e.g. data collected before the labtest started.
# For example the data collected during the setting up of second_iButtons in my office. It contains rather high values of
# about 27 ??C, which should not be included in the statistics.
# If no subset should be supplied, just work with the original data table or enter the dates of the entire period.

# Time period for Labtest
#--> outcommented start_Labtest <- strptime("2019-07-04 16:00:00", "%Y-%m-%d %H:%M:%S")
#---> outcommented end_Labtest <- strptime("2019-07-08 13:55:00", "%Y-%m-%d %H:%M:%S")

#---> chose own time period
range(list_second_iButton_datetime[[1]])
start_time=strptime("2020-07-18 00:00:00", "%Y-%m-%d %H:%M:%S")
end_time=strptime("2020-07-30 00:00:00", "%Y-%m-%d %H:%M:%S")

# Apply the time index on the single data table
# ---> outcommented d_second_iButton_single_corr <- subset(d_second_iButton_single, Datetime >= start_Labtest & Datetime <= end_Labtest)
#---> changed for own time span
#d_second_iButton_single_corr <- subset(d_second_iButton_single, Datetime >= start_time & Datetime <= end_time)

# Apply the time index on each data table in the list "list_second_iButton"
# So for each data table the same time period is selected
#---> outcommented list_second_iButton_corr = lapply(list_second_iButton, function(x) {subset(x, x[,1] >= start_Labtest & x[,1] <= end_Labtest)})
#---> changed for own time span
list_second_iButton_corr = lapply(list_second_iButton, function(x) {subset(x, x[,2] >= start_time & x[,2] <= end_time)})

rm(list = as.character(files_second_iButtons)) #remove csv.files from environment
list_second_iButton_corr_set=list_second_iButton_corr

require(zoo)
require(xts)
require(splines)

#interpolate the data to minute intervalls to set all loggers to the same starting point
#create empty vector of minute data for the same timeframe as temp data
#assume linearity and approximate values 
head(list_second_iButton_corr)

#Create time sequence by minute
#loop through list
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") #create minute timeframe
list_second_iButton_corr_set=list_second_iButton_corr
list_second_iButton_corr_set=lapply(list_second_iButton_corr_set, `[`, 1:2) #use only 2nd and 3rd column
#create new list to use as output
for(i in 1:length(list_second_iButton_corr)){
  #create time series with datetime and temperature
  test=xts(list_second_iButton_corr[[i]][,1],list_second_iButton_corr[[i]][,2]) 
  #merge logger time series with emtpy one minute time series
  test2=merge(test,date_time_complete)
  #replace NA values (created by merging with higher res) with spline interpolated values
  test2=na.spline(test2)
  test2=data.frame("Temperature_corr"=test2) #name the new column
  test2$Datetime.1=rownames(test2) #use the newly set times to replace previous time data
  rownames(test2)=NULL #delete rownames
  test3=test2[c(TRUE,rep(FALSE,9)),] #keep only every 10th value to get 10min res
  test3$test=round(test3$test/.5)*.5 #round to .5 decimal place 
  list_second_iButton_corr_set[[i]][1:length(test3[,1]),1:2]=test3[,1:2] #replace the time and temp column with the new values
  list_second_iButton_corr_set[[i]]=list_second_iButton_corr_set[[i]][-length(test3[,1]+1),1:2] #delete last row in every dataframe (sometimes NA)
}

rm(test, test2, test3)


#rowbind dataframes two one list
for(i in names(list_iButton_corr_set)){
  dat=list_iButton_corr_set[[i]]
  dat_2=list_second_iButton_corr_set[[i]]
  dat_bind=rbind(dat, dat_2)
  list_iButton_corr_set[[i]]=dat_bind
}

#add consecutive and NA in order to dispaly it correctly in the plot 
date_time <- seq.POSIXt(from=list_iButton_corr_set[[1]][1,2],
                        to=list_iButton_corr_set[[1]][dim(list_iButton_corr_set[[1]])[1],2],
                        by="10 min") #create complete timeframe

for(i in 1:length(list_iButton_corr_set)){
  test=xts(list_iButton_corr_set[[i]][,1],list_iButton_corr_set[[i]][,2]) 
  #merge logger time series with emtpy one minute time series
  test2=merge(test,date_time)
  test2=data.frame("Temperature_C"=test2) #name the new column
  test2$Datetime.1=rownames(test2) #use the newly set times to replace previous time data
  rownames(test2)=NULL #delete rownames
  colnames(test2)=c("Temperature_C", "Datetime.1")
  list_iButton_corr_set[[i]]=test2
}

list_iButton_corr_set_date <- lapply(list_iButton_corr_set, `[`, 2)
list_iButton_corr_set_date <- lapply(list_iButton_corr_set_date, function(x) as.POSIXct(x$Datetime.1,format="%Y-%m-%d %H:%M:%S"))
list_iButton_corr_set = map2(list_iButton_corr_set, list_iButton_corr_set_date, ~ mutate(., Datetime.1 = .y)) 

setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/UHI_Project_FE")
save(list_iButton_corr_set, file="listiButtons.rData")

list_iButton_only_temp <- lapply(list_iButton_corr_set, `[`, 1)
dataframe_logger<-as.data.frame(do.call(cbind, list_iButton_only_temp))   
colnames(dataframe_logger)<-names(list_iButton_corr_set)
dataframe_logger$datetime<-list_iButton_corr_set[[1]]$Datetime.1
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Temp_Logger/")
write.csv(dataframe_logger, file = "Logger_2020.csv")
