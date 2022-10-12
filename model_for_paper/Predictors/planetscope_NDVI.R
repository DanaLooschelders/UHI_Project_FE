#NDVI with Planetscope
library(raster)
library(sp)
library(rgdal)
library(mapview)
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

#plot
spplot(files_list[[7]])
#merge
files_list$fun <- "mean"
mos <- do.call(mosaic, files_list) #that does not work, I have no idea why
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

#resample to a higher resolution (of 0.5 m)
res(NDVI)
NDVI_05<-disaggregate(NDVI, fact=6)
#check
res(NDVI_05)

