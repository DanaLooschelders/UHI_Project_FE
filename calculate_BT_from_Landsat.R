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

BT <- BT(Landsat_10 = LS10_crop , Landsat_11 = LS11_crop)
BT_2<- BT[[2]]
BT_1 <- BT[[1]]
mapview(BT_2)
#BT_1_celsius <- (BT[[1]]-273.15)
#writeRaster(x=BT[[2]],filename="BT_2",format = "GTiff")

NIR_crop <- crop(y=gadm_sf, x=NIR)
red_crop <- crop(y=gadm_sf, x=red)
NDVI <- ((NIR_crop- red_crop)/(NIR_crop + red_crop))
E <- E_VandeGriend(NDVI)
tau <- tau(To = 26, RH = 42)
ta <- c(26)
LST <- MWA(BT = BT, tau = tau, E = E, To = ta)
mapview(LST)

######### zweiter versuch - per Hand berechnet 
#https://rpubs.com/geka/UrbanWarm
library(raster)
install.packages("RStoolbox")
library(RStoolbox)
library(rgdal)
#library(devtools)
#install_github(repo = "bleutner/RStoolbox")
rasterOptions(tmpdir = "/Users/amelie/Desktop/LOEK/MSc/M8/Projekt")

metadata <- readMeta("Daten_roh/Landsat/LC08_L1TP_197024_20200724_20200911_02_T1/LC08_L1TP_197024_20200724_20200911_02_T1_MTL.txt",
                  raw=F)
lsat8 <- stackMeta(metadata, allResolutions = TRUE)^

