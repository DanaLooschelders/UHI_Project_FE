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

lap = "C:/Users/Ready2Go/DOCUME~1/zeugs/loekki/MASTER~2/ferni/projekt/lst_zeug/PROCES~1/LST_Germany"
#MODISoptions(lap, outDirPath = file.path(lap, "PROCESSED")
#             , MODISserverOrder = c("LPDAAC", "LAADS"), quiet = TRUE)

### download data##
#getHdf("MOD11A1",begin = "2020.07.01", end = "2020.07.31",
#       tileH = 18, tileV = 3)

### process data (extract LST only) ####
runGdal(job="LST_Germany","MOD11A1",begin = "2020.07.01", end = "2020.07.31",
        tileH = 18, tileV = 3
        , SDSstring = "100000000000")

#Read in the names of all files that end with .tif
rastlist <- list.files(path = "C:/Users/Ready2Go/DOCUME~1/zeugs/loekki/MASTER~2/ferni/projekt/lst_zeug/PROCES~1/LST_Germany", 
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
