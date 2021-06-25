#download the sciebo desktop client from:
#https://hochschulcloud.nrw/de/download/index.html
#synchronize with local hard disc
#the sciebo symbol should appear in your file structure
#set the working directory to the sciebo file
setwd("C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Orga/")
#test to write into an dread from the file
#whoooooo :D 
rm(list=ls())
#load librarys
library(raster)
library(sp)
library(MODIS)
library(MODISTools)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
library(plyr)

#### Download files ####
#EarthdataLogin() # urs.earthdata.nasa.gov download MODIS data from LP DAAC

lap = "C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST/MODIS/"
MODISoptions(lap, outDirPath = file.path(lap, "PROCESSED")
             , MODISserverOrder = c("LPDAAC", "LAADS"), quiet = TRUE)

### download data##
getHdf("MYD11A1",begin = "2020.06.05", end = "2020.06.19",
       tileH = 18, tileV = 3)

### process data (extract LST only) ####
runGdal(job="LST_Germany","MYD11A1",begin = "2020.06.05", end = "2020.06.19",
        tileH = 18, tileV = 3
        , SDSstring = "101010100000")


###############################################################################
  #    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #  
 #    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #     
#    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #
###########################################################################

mainpath <- "C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST"
#datapath <- paste0(mainpath,"/data/")
#datapath <- paste0(datapath,"/RData/")
#Shppath <- paste0(datapath,"/ShapeLayers/")
MODISpath <- "C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST/MODIS/"
#StationDat <- readOGR(paste0(Shppath,"ClimateStations.shp"))

aquapath <- paste0(MODISpath,"/aqua/")
terrapath <- paste0(MODISpath,"/terra/")
dataTable <- list()
timeTable <- list()

  for (path in c(aquapath,terrapath)){
    if(path==aquapath){
      sensor <- 1
      sensorName <- "Aqua"}
    if(path==terrapath){
      sensor <- 2
      sensorName <- "Terra"
    }
    dataTable[[sensor]] <- list()
    filelist <- list.files(path,full.names = TRUE,pattern=".tif$")
    filelist_time <- filelist[grep("view_time",filelist)]
    filelist_data <- filelist[-grep("view_time",filelist)]

    ###check übereinstimmung
    if (any(substr(filelist_data,nchar(filelist_data)-10,nchar(filelist_data))!=
            substr(filelist_time,nchar(filelist_time)-10,nchar(filelist_time)))){
      # stop("filelist data does not match filelist time")
      filelist_data_night <- filelist_data[lapply(strsplit(filelist_data,"_"),function(x){x[[9]]})=="Night"]
      filelist_data_day <- filelist_data[lapply(strsplit(filelist_data,"_"),function(x){x[[9]]})=="Day"]
      filelist_time_night <- filelist_time[lapply(strsplit(filelist_time,"_"),function(x){x[[8]]})=="Night"]
      filelist_time_day <- filelist_time[lapply(strsplit(filelist_time,"_"),function(x){x[[8]]})=="Day"]
      
      filelist_data_night <- filelist_data_night[lapply(strsplit(filelist_data_night,"_"),function(x){x[[11]]})%in%
                                                   lapply(strsplit(filelist_time_night,"_"),function(x){x[[11]]})]
      filelist_time_night <- filelist_time_night[lapply(strsplit(filelist_time_night,"_"),function(x){x[[11]]})%in%
                                                   lapply(strsplit(filelist_data_night,"_"),function(x){x[[11]]})]
      filelist_data_day <- filelist_data_day[lapply(strsplit(filelist_data_day,"_"),function(x){x[[11]]})%in%
                                               lapply(strsplit(filelist_time_day,"_"),function(x){x[[11]]})]
      filelist_time_day <- filelist_time_day[lapply(strsplit(filelist_time_day,"_"),function(x){x[[11]]})%in%
                                               lapply(strsplit(filelist_data_day,"_"),function(x){x[[11]]})]
      
      filelist_time<-c(filelist_time_day,filelist_time_night)
      filelist_data<-c(filelist_data_day,filelist_data_night)
      if (any(substr(filelist_data,nchar(filelist_data)-10,nchar(filelist_data))!=
              substr(filelist_time,nchar(filelist_time)-10,nchar(filelist_time)))){
        stop("filelist data does not match filelist time")
      }}
  }




