library(stars)
library(sf)
library(raster)
library(mapview)
library(rgdal)
#set working directory
setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/3d-gm_lod1_kacheln/")

#list files
files<-list.files(pattern=".gml")
#create empty files list
files_list<-vector(mode='list', length=length(files))
#set names of list to match files
names(files_list)<-files
#Try other methos of opening
#(dsn=path.expand("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/dummedateien/dummedateien/LoD2_32_402_5745_1_NW.gml"), files[1], layer = "Building")
#terra::vect(files[1], layer="Building") 
#st_read(files[1])
#read_sf(files[1], layer="Building")
#loop through all the files
for(i in files){
  tryCatch({
    #read in files and set crs
    files_list[[i]]<-read_sf(i, layer="Building",
                             crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    
  })
}
#find all list entrys that are NULL
length(Filter(is.null, files_list))
index_NULL_files<-names(Filter(is.null, files_list))

#read those in seperately
for (i in index_NULL_files){
  tryCatch(expr={
  #read in files and set crs
  files_list[[i]]<-read_sf(i,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" )
  }, error=function(e){message("Caught an error")})
}

#test how many files are NULL now
length(Filter(is.null, files_list))

#delete NULL files
files_list<-files_list[vapply(files_list, Negate(is.null), NA)]

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
