#get landsat data
#load packages
library(devtools)
library(raster)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)
library(sp)
#install packages
#install.packages("landsat")
#install.packages("LST")
#devtools::install_github("16EAGLE/getSpatialData") 

library(getSpatialData)
#explanation for getspatialData:
#https://www.rdocumentation.org/packages/getSpatialData/versions/0.1.0
library(landsat) #process landsat data
library(LST) #claculate LST from landsat bands 10 and 11
#check if server is available
services()
#login to USGS
login_USGS(username = "Dana_L", 
           password="Bachelorarbeit2020",
           n_retry = 10,
           verbose = T)
login_earthdata(username = "Dana_L2020", 
                password = "Bachelorarbeit2020",
                n_retry = 10,
                verbose=T)

#set archive directory
set_archive("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt")
#download 
get_products("Landsat")
getLandsat_records(time_range = c("2020-07-01", "2020-07-31"),
                   products = "LANDSAT_8_C1",
                   check_products = T)
getLandsat_data()
#"LC08_L1TP_197023_20200622_20200707_01_T1"
