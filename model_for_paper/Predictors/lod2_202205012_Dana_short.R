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

