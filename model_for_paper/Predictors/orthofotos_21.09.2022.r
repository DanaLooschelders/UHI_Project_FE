#read digital orthophotos in R
setwd("C:/Users/Dana/Desktop/dop_kacheln")

library(stars)
library(sf)
library(terra)
setwd("C:/Users/Dana/Desktop/dop_kacheln")

#test with one file
test<-rast("dop10rgbi_32_395_5755_1_nw_2022.jp2")
plot(test)

#test with all files
files<-list.files(pattern=".jp2")
files_list<-vector(mode='list', length=length(files))
names(files_list)<-files

for(i in files){
  tryCatch({
    print(i)
    #read in files and set crs
    files_list[[i]]<-rast(i)
  }, error=function(e){message("Caught an error")})
}

#calculate NDVI for every spatRaster
#Kanäle: RGBI 
#mosaic files
names(files_list) <- NULL

mos <- do.call(merge, files_list[1:10])



