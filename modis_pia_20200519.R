#download the sciebo desktop client from:
#https://hochschulcloud.nrw/de/download/index.html
#synchronize with local hard disc
#the sciebo symbol should appear in your file structure
#set the working directory to the sciebo file
setwd("C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Orga/")
#test to write into an dread from the file
write.csv(x = rain,file="raintest.csv")
read.csv(file = "raintest.csv")
#whoooooo :D 

#load librarys
library(raster)
library(sp)
library(MODIS)
library(MODISTools)
library(rgdal)
library(gdalUtils)
library(sf)
library(mapview)

#### Download files ####
#EarthdataLogin() # urs.earthdata.nasa.gov download MODIS data from LP DAAC

lap = "C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST/MODIS/"
MODISoptions(lap, outDirPath = file.path(lap, "PROCESSED")
             , MODISserverOrder = c("LPDAAC", "LAADS"), quiet = TRUE)

### download data##
getHdf("MOD11A1",begin = "2020.07.01", end = "2020.07.31",
       tileH = 18, tileV = 3)

### process data (extract LST only) ####
runGdal(job="LST_Germany","MOD11A1",begin = "2020.07.01", end = "2020.07.31",
        tileH = 18, tileV = 3
        , SDSstring = "100000000000")

#Read in the names of all files that end with .tif
rastlist <- list.files(path = "C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST/MODIS/PROCESSED/LST_Germany", 
                       pattern=".tif", 
                       all.files=TRUE, full.names=FALSE)

#check names
rastlist
#import all raster files in folder using lapply
modis <- lapply(rastlist, raster)
#transform all raster files to celsius
modis_celsius <- lapply(modis, function(x) x*0.02-273)

#test by plotting one raster in Kelvin and one in Celsius
plot(modis[[1]])
plot(modis_celsius[[1]])

#crop to muenster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm <- as(gadm,"sf")

#transform coordinates to MODIS coordinate system
gadm <- st_transform(gadm,st_crs(modis_celsius[[1]]))
mapview(gadm)

#crop the list to shape of muenster
modis_crop=lapply(modis_celsius,  function(x) mask(x, gadm))
#check if it worked by plotting first file
mapview(modis_crop[[1]])