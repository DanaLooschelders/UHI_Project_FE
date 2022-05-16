library(stars)
library(sf)
library(raster)
library(mapview)

#set working directory
setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/gml")

#list files
files<-list.files(pattern=".gml")
#create empty files list
files_list<-vector(mode='list', length=length(files))
#set names of list to match files
names(files_list)<-files

#loop through all the files
for(i in files){
  tryCatch({
    #read in files and set crs
    files_list[[i]]<-read_sf(i, layer="Building",
                             crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    
  })
}
#raster to stars Raster with pixel size of 10x10
raster_list<-lapply(files_list, function(x)  st_rasterize(x["measuredHeight"], dx=10, dy=10 ))
#coerce to sp object
sp_list<-lapply(raster_list, function(x) as(x, "Spatial"))
#test by plotting
plot(sp_list[[1]], axes=T)
mapview(sp_list[[1]])
#plot multiple
mapview(sp_list[[9]])+mapview(sp_list[[8]])
#coerce to raster
raster_list <- lapply(sp_list, raster)
#check resolution
lapply(raster_list, res)
#mosaic together
raster_list.mosaicargs <- raster_list #create new list
names(raster_list.mosaicargs) <- NULL #set names to NULL so that they are recognised by moasic
raster_list.mosaicargs$fun <- mean #overlapping cells should get mean value
lapply(raster_list, origin) #check difference in origin
raster_list.mosaicargs$tolerance <- 1 #tolerance for origin is 1
raster_list.mosaicargs$na.rm <- TRUE #ignore NA values
ras_mosaic <- do.call(mosaic,  raster_list.mosaicargs) #mosaic with do.call

mapview(ras_mosaic) #check

#calculate standard deviation of building height
sd_3x3<-focal(ras_mosaic, w=matrix(1, 3,3), fun=sd, na.rm=T)

mapview(sd_3x3)#check
