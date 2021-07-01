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
library(sp)
library(raster)
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
#set working directory to location of MODIS files
setwd("C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST/MODIS/PROCESSED/LST_Germany")
#import all raster files in folder using lapply
modis <- lapply(rastlist, raster)
#transform all raster files to celsius
modis_celsius <- lapply(modis, function(x) x-273.15)

#test by plotting one raster in Kelvin and one in Celsius
plot(modis[[1]])
plot(modis_celsius[[1]])
values(modis_celsius[[1]])
values(modis_celsius[[2]])
#crop to muenster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
mapview(gadm_sf)
#check if crs are matching
crs(gadm)
crs(modis_celsius[[1]])

#transform coordinates of MODIS coordinate system to gadm
modis_proj=lapply(modis_celsius,  function(x) projectRaster(from=x, crs=crs(gadm)))

crs(modis_celsius[[2]])


#check crs again
crs(gadm)
crs(modis_proj[[1]])
sum(is.na(values(modis_proj[[1]])))

#crop the list to shape of muenster
extent(modis_proj[[1]])
modis_crop=lapply(modis_proj,  function(x) crop(x=x, y=gadm_sf))
sum(values(modis_crop[[1]]))
plot(modis_crop[[2]])                  
#exclude if all values are NA
modis_cc=Filter(function(a) sum(!is.na(values(a))), modis_crop)
mapview(modis_cc[[2]])
#save Raster
setwd("C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST/MODIS/PROCESSED/LST_Germany/")
#lapply(modis_cc,  function(x) writeRaster(x=x, filename=paste("processed",x,".tif"),format = "GTiff"))
for(i in 1:length(modis_cc)){
  writeRaster(x=modis_cc[[i]],filename=paste("processed",i,".tif"),format = "GTiff", 
              overwrite=T)
}
#check if it worked by plotting file
mapview(modis_cc[[3]])
mapview(modis_cc[[11]])+mapview(gadm_sf)

#Read in the names of all files that end with .tif
rastlist <- list.files(path = "C:/Users/Ready2Go/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LST/MODIS/PROCESSED/LST_Germany/", 
                       pattern=".tif", 
                       all.files=TRUE, full.names=FALSE)
#import all raster files in folder using lapply
modis_cc <- lapply(rastlist, raster)
mapview(modis_cc[[40]])
