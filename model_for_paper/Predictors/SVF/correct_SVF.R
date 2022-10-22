library(mapview)
library(raster)
library(sp)
library(rgdal)
library(beepr)

####prepare ndom####
setwd("C:/Users/Dana/sciebo/ndom")
#load ndom
ndom_1m<-raster("ndom_crop_muenster_int_1m.tif")
crs(ndom_1m) 
extent(ndom_1m)
res(ndom_1m)
#+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs 
####load svf####
svf<-raster("svf.tif")
crs(svf)
# +proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs 
extent(svf)
#####prepare dlm####
dlm<-raster("dlm_raster_ms_2.tif")
crs(dlm) #+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs 
extent(dlm)
res(dlm)
####prepare planetscope NDVI####
#NDVI with Planetscope
setwd("D:/ms2020new_psscene_analytic_sr_udm2/files")

#load file names
files<-list.files(pattern="harmonized_clip.tif")
files_list<-vector(mode='list', length=length(files))
names(files_list)<-files

#read files into list
for(i in files){
  tryCatch({
    print(i)
    #read in files and set crs
    files_list[[i]]<-stack(i)
  }, error=function(e){message("Caught an error")})
}
#merge
#ugly, but it works
test<-mosaic(files_list[[1]], files_list[[2]], files_list[[3]], files_list[[4]],
             files_list[[5]], files_list[[6]], files_list[[7]], files_list[[8]],  fun="mean")
spplot(test) #plot
mapview(test)
#calculate NDVI
#Formula: (NIR – R) / (NIR + R) 
NDVI<-(test[[4]]-test[[3]])/(test[[4]]+test[[3]])
mapview(NDVI)
#use 0.7 as threshold for NDVI (check source)

#resample to a higher resolution (of 1 m)
res(NDVI)
NDVI_1m<-disaggregate(NDVI, fact=3)
#check
res(NDVI_1m)
#get raster where everything is masked except NDVI>0.7
NDVI_green<-NDVI_1m
NDVI_green[NDVI_green<0.7]<-NA
beep()
spplot(NDVI_green)
mapview(NDVI_onlygreen) #check
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/sky view factor")
writeRaster(NDVI_green,filename = "NDVI_green")
NDVI_green<-raster("NDVI_green.gri")
crs(NDVI_green)
extent(NDVI_green)
#+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs
NDVI_green<-projectRaster(NDVI_green, ndom_1m)
beep()
beep(sound=9)
crs(NDVI_green) 
#+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs
extent(NDVI_green)==extent(ndom_1m) #TRUE
writeRaster(NDVI_green,filename = "NDVI_green", overwrite=T)
mapview(NDVI_green)
####correct Sky View Factor####
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
setwd("C:/Users/Dana/sciebo/ndom")
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
mapview(svf_build)

#define function to set all pixels with height > 4 m and forest object code to NA
myFun3 <- function(x, y) {ifelse( x == 9999 && any(!is.na(y)),
                                  x <- 0.26, x <- x)}

svf_build_trees<-overlay(svf_build, NDVI_green, fun=Vectorize(myFun3))
beep()
plot(svf_build_trees)
mapview(svf_build_trees)

writeRaster(svf_build_trees, "svf_build_trees", overwrite=T)
svf_build_trees<-("svf_build_trees.grd")
#mask all pixels that are still NA
svf_build_trees[svf_build_trees==9999]<-NA
beep()
#plot
mapview(svf_build_trees)
writeRaster(svf_build_trees, "svf_build_trees_noNA", overwrite=T)
#load
svf_build_trees<-raster("svf_build_trees_noNA.grd")
#aggregate to 10m resolution
svf_cor_10m<-raster::aggregate(x = svf_build_trees, fact=10)
mapview(svf_cor_10m)

writeRaster(svf_cor_10m, "svf_cor_10m")
