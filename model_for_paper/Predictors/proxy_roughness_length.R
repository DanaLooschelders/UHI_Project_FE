library(mapview)
library(raster)
library(sp)
library(rgdal)
library(beepr)

setwd("C:/Users/Dana/sciebo/ndom")
ndom<-raster("ndom_crop_muenster.tif")
res(ndom)
#aggregate to 10 m with mean height
ndom_mean_10m<-aggregate(x=ndom, fact=20, FUN=mean)
plot(ndom_mean_10m)
writeRaster(nddom_10m, filename="ndom_mean_10m")
test<-raster("ndom_mean_10m")
plot(test)
#aggregate to 10m with sd of height
ndom_sd_10m<-aggregate(x=ndom, fact=20, FUN=sd)
plot(ndom_sd_10m)
writeRaster(nddom_10m, filename="ndom_mean_10m")
test<-raster("ndom_sd_10m")

setwd("C:/Users/Dana/sciebo/ndom")
#load svf
svf<-raster("svf.tif")
crs(svf)
# +proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs 
extent(svf)
#load ndom
ndom_1m<-raster("ndom_crop_muenster_int_1m.tif")
crs(ndom_1m) 
extent(ndom_1m)
res(ndom_1m)
#+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs 
#load dlm
dlm<-raster("dlm_raster_ms_2.tif")
crs(dlm) #+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs 
extent(dlm)
res(dlm)

#check if any sky view factor pixels are NA
any(is.na(values(svf))) #TRUE
#check if extents match
extent(svf)==extent(ndom_1m) #TRUE
#define function to set all pixels with height > 4 m as 9999
myFun <- function(x, y) { ifelse( y > 4, x <- 9999, x <- x) }
#execute function and create new output raster
svf_under4 <- overlay(stack(svf, ndom_1m), fun = Vectorize(myFun))
plot(svf_under4)
writeRaster(svf_under4,"svf_under4")
svf_under4<-raster("svf_under4.grd")
#set svf for trees/forest to certain factor
#object codes for forest: 43002, 43003
#object codes for building: 41002, 41010, 41008, 51002, 51006, 51007, 
#51003, 51006, 51007, 51002, 51006, 51007, 51009, 53009, 53001
builds<-c(41002, 41010, 41008, 51002, 51006, 51007, 
          51003, 51006, 51007, 51002, 51006, 51007, 51009, 53009, 53001)
extent(dlm)==extent(svf)
#get to same extent
dlm_resampled <- resample(dlm, svf, method='bilinear')
writeRaster(dlm_resampled, "dlm_resampled") #write to file
dlm_resampled<-raster("dlm_resampled.grd") #load again
extent(dlm_resampled)==extent(svf) #compare extents
#define function to set all pixels with height > 4 m and building object code to NA
myFun2 <- function(x, y) {ifelse( x == 9999 && any(y==builds),
                                    x <- NA, x <- x)}
#create tiny test subset of raster to test function
plot(svf_under4)
extent(svf_under4)
test_extent<-extent(c(397000, 398500, 5755000, 5755500))
svf_under4_test<-crop(svf_under4, test_extent)
plot(svf_under4_test)
mapview(svf_under4_test)
dlm_resampled_test<-crop(dlm_resampled, test_extent)
plot(dlm_resampled_test)
mapview(dlm_resampled_test)
#--> works 
#execute function and create new output raster
svf_build <- overlay(stack(svf_under4, dlm_resampled), fun = Vectorize(myFun2))
plot(svf_build)
mapview(svf_build)
#how many NAs -> should be more in svf_build as NAs were added
cellStats(svf_build, stat="countNA")>cellStats(svf_under4, stat="countNA")
#write to file
writeRaster(svf_build, "svf_build", overwrite=T)

svf_build<-raster("svf_build.grd")

#define function to set all pixels with height > 4 m and forest object code to NA
forest<-c(43002, 43003)
myFun3 <- function(x, y) {ifelse( x == 9999 && any(y==forest),
                                  x <- 0.26, x <- x)}

svf_build_trees<-overlay(svf_build, dlm_resampled, fun=Vectorize(myFun3))
beep(sound=3)
plot(svf_build_trees)
mapview(svf_build_trees)

writeRaster(svf_build_trees, "svf_build_trees")

svf_build_trees<-("svf_build_trees.grd")