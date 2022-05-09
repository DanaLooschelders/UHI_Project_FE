
# Empty console
cat("\014")

# Empty workspace
# Uncomment if necessary
rm(list = ls())

# --------------------------------------------------------------------------------------------------------------------------------
### Read in data ###

# Set Working directory (wd)
# setwd(choose.dir()) # Uncomment if necessary, but only works for Windows operating system
#setwd("V:/klima/Projekte/2019_Urban_Heat_Island/Data/Data_raw/Calibration_test_20190704-20190708")
#setwd("C:/00_Dana/Uni/6. Semester/Bachelorarbeit/logger_data/UHI_20200703-20200717/")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger/Rohdaten/UHI_20200605-20200619/")
#the logger IDs 56 and 102 were added manually to the .csv files as they were missing in the original file
#later, ID 102 was corrected to 93 because 102 was the old ID from Stiftherrenstrasse 
#and 93 was the true missing ID from Spiekerhof vegetation
#################
# Data june_iButtons #
#################
# Two different ways of reading in data:
# 1. Read in a single file: If you only need to control a single june_iButton etc. use this code
# 2. Read in several files that are in the working directory: 
#    If you want to look at multiple june_iButtons, apply statistics to all data etc. use this code

###
# Column names for june_iButton data
# 1. Select header from one of the june_iButton-Files 
# UNCOMMENT IF NEEDED
#june_iButton_header=read.table("3F000000517B3D21_190708.csv", sep = ",", dec = ".", header = F, skip = 7,
#                          nrows = 1, as.is = T) 

# Customized header
# Create new header as the default one from the files isn't really pretty
june_iButton_header = c("Datetime", "Temperature_C")

# Create vector containing ID #
# We only need the ID # without the first (empty) column, so we set the first column to NULL in colClasses
#--> changed name (to second file from raw data - UHI timeframe)
june_iButton_ID=read.table("100_1400000051A3B521_200619.csv", sep = ",", dec = ".", header = F, skip = 4,
                      nrows = 1, as.is = T, colClasses=c("NULL", NA)) 


###
# 2. Read in several files into separate dataframes
# List all files you want to read in by choosing them by name/file type, etc. with "pattern".
# Here every file, which ends with ".csv" is chosen from the current wd.
files_june_iButtons=list.files(pattern =".csv")  


# Loop to read in all files in the list into separate data.frames
for (i in 1:length(files_june_iButtons)) assign(files_june_iButtons[i], read.csv(text=paste0(head(readLines(files_june_iButtons[i]), -1)), 
                                                                       sep = ",", dec = ".", header = F, skip = 8, 
                                                                       na.strings = c("<NA>", "NA", "NAN"), 
                                                                       stringsAsFactors = FALSE))


### 
# Read in ID from all files from the list and bind them together
# CAUTION: The files have to have the SAME number of columns!
for(i in files_june_iButtons) {
  if(!exists("june_iButton_ID_multi")) {
    june_iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                nrows = 1, as.is = T, colClasses=c("NULL", NA))
  } else {
    temp_june_iButton_ID_multi=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                                     nrows = 1, as.is = T, colClasses=c("NULL", NA))
    june_iButton_ID_multi=rbind(june_iButton_ID_multi, temp_june_iButton_ID_multi)
    remove(temp_june_iButton_ID_multi)
  }
}

# To check if all june_iButtons we want are read in, we sort ID # ascending and then display all ID # that occur
june_iButton_ID_sort = sort(june_iButton_ID_multi$V2)
june_iButton_ID_sort

# Create new names for the data files out of ID, serial number (SR) and date of data collection
# First get rid of the file ending .csv in the file name
june_iButton_SR_date = sapply(strsplit(files_june_iButtons, "\\."), "[", 1)

# Paste the ID vector with the names derived from the files_june_iButton list
list_header <- paste(june_iButton_ID_multi$V2)
list_header
# Select all csv-files and put them in one list
# By having all dataframes in the same list, you can apply changes to all files simultanuously, e.g. renaming.
list_june_iButton <- mget(ls(pattern =  ".csv"))

# Assign new file names to every june_iButton-file in list "list_june_iButton"
names(list_june_iButton) <- list_header

# Assign the same column names to every june_iButton-file in list "list_june_iButton"
list_june_iButton = lapply(list_june_iButton, setNames, nm = june_iButton_header)


# --------------------------------------------------------------------------------------------------------------------------------
# Add timestamp to all june_iButton-files
# First select only the first column (=timestamp of our data) of each file in the list
list_june_iButton_datetime <- lapply(list_june_iButton, `[`, 1)

# Transform timestamp from character to POSIXct format for each file in the sublist list_june_iButton_datetime
# We get a sublist only containing POSIXct datetime format for each june_iButton
# outcommented ---> list_june_iButton_datetime <- lapply(list_june_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%d-%m-%Y %H:%M"))
#---> shouldn't it be: "%Y-%m-%d %H:%M"? changed in next line
list_june_iButton_datetime <- lapply(list_june_iButton_datetime, function(x) as.POSIXct(x$Datetime,format = "%Y-%m-%d %H:%M"))


# Add the new transformed timestamp to the original list
# There are two options, uncomment the one you need.
# 1. Add POSIXct timestamp as additional column
list_june_iButton <- mapply(cbind, list_june_iButton, "Datetime"=list_june_iButton_datetime, SIMPLIFY=F)

# 2. Replace the old timestamp with POSIXct timestamp
#list_june_iButton = map2(list_june_iButton, list_june_iButton_datetime, ~ mutate(., Datetime = .y)) 


# --------------------------------------------------------------------------------------------------------------------------------
# Subset data by choosing start and end date of the desired time period
# Unnecessary data can be removed, e.g. data collected before the labtest started.
# For example the data collected during the setting up of june_iButtons in my office. It contains rather high values of
# about 27 °C, which should not be included in the statistics.
# If no subset should be supplied, just work with the original data table or enter the dates of the entire period.

#---> chose own time period
range(list_june_iButton_datetime[[1]])
start_time=strptime("2020-06-09 18:00:00", "%Y-%m-%d %H:%M:%S")
end_time=strptime("2020-06-18 20:30:00", "%Y-%m-%d %H:%M:%S")


# Apply the time index on each data table in the list "list_june_iButton"
# So for each data table the same time period is selected
#---> outcommented list_june_iButton_corr = lapply(list_june_iButton, function(x) {subset(x, x[,1] >= start_Labtest & x[,1] <= end_Labtest)})
#---> changed for own time span
list_june_iButton_corr = lapply(list_june_iButton, function(x) {subset(x, x[,1] >= start_time & x[,1] <= end_time)})

rm(list = as.character(files_june_iButtons)) #remove csv.files from environment

#plot as QAQC
library(ggplot2)
library(dplyr)
ggplot(bind_rows(list_june_iButton_corr, .id="df"), aes(x=Datetime.1, y=Temperature_C, colour=df)) +
  geom_line()

####start time correction####
require(zoo)
require(xts)
require(splines)

#interpolate the data to minute intervalls to set all loggers to the same starting point
#create empty vector of minute data for the same timeframe as temp data
#assume linearity and approximate values 
head(list_june_iButton_corr)

#Create time sequence by minute
#loop through list
date_time_complete <- seq.POSIXt(from=start_time,
                                 to=end_time,by="min") #create minute timeframe
list_june_iButton_corr_set=list_june_iButton_corr
list_june_iButton_corr_set=lapply(list_june_iButton_corr_set, `[`, 2:3) #use only 2nd and 3rd column

#create new list to use as output
for(i in 1:length(list_june_iButton_corr)){
  #create time series with datetime and temperature
  test=xts(list_june_iButton_corr[[i]][,2],list_june_iButton_corr[[i]][,3]) 
  #merge logger time series with emtpy one minute time series
  test2=merge(test,date_time_complete)
  #replace NA values (created by merging with higher res) with interpolated values
  test2=na.approx(test2)
  test2=data.frame("Temperature_corr"=test2) #name the new column
  test2$Datetime.1=rownames(test2) #use the newly set times to replace previous time data
  rownames(test2)=NULL #delete rownames
  test3=test2[c(TRUE,rep(FALSE,9)),] #keep only every 10th value to get 10min res
  test3$test=round(test3$test/.5)*.5 #round to .5 decimal place 
  list_june_iButton_corr_set[[i]][1:length(test3[,1]),1:2]=test3[,1:2] #replace the time and temp column with the new values
  list_june_iButton_corr_set[[i]]=list_june_iButton_corr_set[[i]][-length(test3[,1]+1),1:2] #delete last row in every dataframe (sometimes NA)
}

rm(test, test2, test3)

#plot again
ggplot(bind_rows(list_june_iButton_corr_set, .id="df"), aes(x=Datetime.1, y=Temperature_C, colour=df)) +
  geom_line()

#remove water logger
#wasserlogger: 10, 5, 17, 25, 28
#remove water logger
list_june_iButton_corr_set[c("10","5","17","25","28")]<-NULL
#plot again
ggplot(bind_rows(list_june_iButton_corr_set, .id="df"), aes(x=Datetime.1, y=Temperature_C, colour=df)) +
  geom_line()
#Logger 66 started logging later
plot(list_june_iButton_corr_set[["66"]]$Temperature_C, type="l")
range(list_june_iButton_corr_set[["66"]]$Datetime.1)
#create fulltime time series
daterange<-data.frame("Datetime.1"=seq(from=start_time+120, to=end_time+1200, by="10 min"))
range(daterange$Datetime.1)
#merge full timeseries with incomplete data
test<-left_join(daterange, list_june_iButton_corr_set[["66"]] )
#reorder columns of rest
test<-test %>% select(Temperature_C, everything())
list_june_iButton_corr_set[["66"]]<-test
#save list to folder
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Trainingsdaten/Logger")
saveRDS(list_june_iButton_corr_set, file="JuneiButton.RData")
