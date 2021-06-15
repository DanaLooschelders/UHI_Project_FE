library(LST)
library(raster)
library(mapview)
library(sf)
library(sp)
library(rgdal)
library(gdalUtils)
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo")
#setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung")

LS10 <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B10.TIF")
LS11 <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B11.TIF")
red <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B4.TIF")
NIR <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B5.TIF")
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
mapview(LS10)
gadm_sf <- as(gadm,"sf")

crs(gadm_sf)
crs(LS10) 

gadm_sf <- st_transform(gadm_sf, crs = st_crs(LS10))

LS10_crop <- crop(y=gadm_sf,x=LS10)
LS11_crop <- crop(y=gadm_sf,x=LS11)

e <- extent(395103.5,415705.1,5744177,5768658)
LS10_crop <- crop(LS10,e)
extent(gadm_sf)
extent(LS10_crop)
mapview(LS10_crop)

BT <- BT(Landsat_10 = LS10_crop , Landsat_11 = LS11_crop)
BT_2<- BT[[2]]
BT_1 <- BT[[1]]
mapview(BT_2)
mapview(BT_1_celsius)
BT_1_celsius <- (BT[[1]]-273.15)
writeRaster(x=BT[[2]],filename="BT_2",format = "GTiff")

NIR_crop <- crop(y=gadm_sf, x=NIR)
red_crop <- crop(y=gadm_sf, x=red)
NDVI <- ((NIR_crop- red_crop)/(NIR_crop + red_crop))
E <- E_VandeGriend(NDVI)

test <- MWA(BT = BT_1, tau = 0.86, E = E,Ta = 26)

######### zweiter versuch - per Hand berechnet 
RADIANCE_MULT_BAND_10 <- 3.3420E-04
RADIANCE_MULT_BAND_11 <- 3.3420E-04

RADIANCE_ADD_BAND_10 <- 0.10000
RADIANCE_ADD_BAND_11 <- 0.10000

#Load raster package and load band 10 & 11 into R (navigate to your image directory first)
library(raster)
band_10 <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B10.TIF") #change image name accordingly
band_11 <- raster("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_B11.TIF") #change image name accordingly

#Calculate TOA from DN:
toa_band10 <- calc(band_10, fun=function(x){RADIANCE_MULT_BAND_10 * x + RADIANCE_ADD_BAND_10})
toa_band11 <- calc(band_11, fun=function(x){RADIANCE_MULT_BAND_11 * x + RADIANCE_ADD_BAND_11})

#Values from Metafile
K1_CONSTANT_BAND_10 <- 774.8853
K1_CONSTANT_BAND_11 <- 480.8883
K2_CONSTANT_BAND_10 <- 1321.0789
K2_CONSTANT_BAND_11 <- 1201.1442

#Calculate LST in Kelvin for Band 10 and Band 11
temp10_kelvin <- calc(toa_band10, fun=function(x){K2_CONSTANT_BAND_10/log(K1_CONSTANT_BAND_10/x + 1)})
temp11_kelvin <- calc(toa_band11, fun=function(x){K2_CONSTANT_BAND_11/log(K1_CONSTANT_BAND_11/x + 1)})

#Convert Kelvin to Celsius for Band 10 and 11
temp10_celsius <- calc(temp10_kelvin, fun=function(x){x - 273.15})
temp11_celsius <- calc(temp11_kelvin, fun=function(x){x - 273.15})
crop_temp10 <- crop(y=gadm_sf,x=temp10_celsius)
crop_temp11 <- crop(y=gadm_sf,x=temp11_celsius)

mapview(crop_temp10)
