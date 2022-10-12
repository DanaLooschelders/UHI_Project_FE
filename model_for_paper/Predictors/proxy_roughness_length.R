library(mapview)
library(raster)
library(sp)
library(rgdal)
library(beepr)

####prepare ndom####
setwd("C:/Users/Dana/sciebo/ndom")
ndom<-raster("ndom_crop_muenster_int_1m.tif")
res(ndom)
mapview(ndom)
mean(values(ndom), na.rm=T)
res(ndom)
#aggregate to 10 m with mean height
ndom_mean_10m<-raster::aggregate(x=ndom, fact=10, fun=mean, na.rm=T)
res(ndom_mean_10m)
beep()
plot(ndom_mean_10m)
mapview(ndom_mean_10m)
writeRaster(ndom_mean_10m, filename="ndom_mean_10m", overwrite=T)
test<-raster("ndom_mean_10m")


#aggregate to 10m with sd of height
setwd("C:/Users/Dana/sciebo/ndom")
ndom_sd_10m<-raster::aggregate(x=ndom, fact=10, fun=sd, na.rm=T)
plot(ndom_sd_10m)
mapview(ndom_sd_10m)
writeRaster(ndom_sd_10m, filename="ndom_sd_10m")
test<-raster("ndom_sd_10m")

