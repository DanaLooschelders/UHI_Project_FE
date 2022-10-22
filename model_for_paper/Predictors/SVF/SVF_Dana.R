#Sky view factor
library(sf)
library(sfheaders)
library(sp)
library(tidyverse)
library(shadow)
library(parallel)
library(doParallel)
library(purrr)
library(raster)
library(rgdal)
library(mapview)

#prep data
#setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/test_gml/")
#setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/3d-gm_lod1_kacheln")
#setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/gml/")
setwd("E:/3d-gm_lod2_kacheln")

files<-list.files(pattern=".gml")
files_list<-vector(mode='list', length=length(files))
names(files_list)<-files

for(i in files){
  tryCatch({
    #read in files and set crs
    files_list[[i]]<-read_sf(i, layer="Building",
                             crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
    
  }, error=function(e){message("Caught an error")})
}

length(Filter(is.null, files_list))
index_NULL_files<-names(Filter(is.null, files_list))

for (i in index_NULL_files){
  tryCatch(expr={
    #read in files and set crs
    files_list[[i]]<-read_sf(i,crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" )
  }, error=function(e){message("Caught an error")})
}


#check how many files are NULL
length(Filter(is.null, files_list))
#remove NULL files
files_list<-files_list[vapply(files_list, Negate(is.null), NA)]

#extract just height, geometry and ID
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- layer[,c("measuredHeight", "gml_id")]
  files_list[[i]] <- layer
}

#check
class(files_list[[1]])
plot(files_list[[2]])

#only do once! 
#remove z dimension
for (i in seq(files_list)){
  layer <- files_list[[i]]
  layer <- st_zm(layer) 
  files_list[[i]] <- layer
}

#####transform to spatial polygon dataframe####
shp<- vector(mode='list', length=length(files_list)) #create empty list
names(shp)<-names(files_list)
#create extra dataframe for height of every polygon
shp_height<-vector(mode='list', length=length(files_list)) #create empty list
names(shp_height)<-names(files_list)
#remove empty geometries

#transform to spatialPolygons
for (i in seq(files_list)){
  tryCatch(expr={
  print(i) #see where error occurs
  layer <- files_list[[i]]
  layer <- st_as_sf(layer)
  empty_geometry<-rep(NA, length=nrow(layer))
  for(x in 1:nrow(layer)){ #check every row in layer
    if(class(layer[x,]$geometry)[1]=="sfc_POLYHEDRALSURFACE"){ #check if geometry type is correct
      empty_geometry[x]<-x
    }else{}
    if(is.na(layer[x,]$measuredHeight)){ #check if height is NA
      empty_geometry[x]<-x 
    }else{}
  }
  empty_geometry<-empty_geometry[!is.na(empty_geometry)] #drop NAs
  #length(layer$geometry)==length(layer$measuredHeight)
  if(length(empty_geometry)!=0){ #if there are empty geometries
  layer<-layer[-c(empty_geometry),] #drop rows with wrong geometry type
  #length(layer$geometry)==length(layer$measuredHeight)
    }
  layer <- st_polygonize(layer)
  if(any(st_is_empty(layer))){ #check if there are still empty geometries
    layer<-layer[!st_is_empty(layer),] #remove empty geometries
  }
  sp_layer <- as(st_geometry(layer), "Spatial")
  if(length(sp_layer)!=length(layer)){ #check for length mismatch
    if(any(is.na(getSpPPolygonsIDSlots(sp_layer)))){ #if any ID is NA
      ids<-which(is.na(getSpPPolygonsIDSlots(sp_layer)))
      sp_layer<-sp_layer[-ids] #remove polygons without ID
    }else{}
  }else{}
  if(length(sp_layer)!=length(layer$measuredHeight)){ #QAQC if lengths match
    print("length unequal")
  }else{}
  #preserve height
  shp_height[[i]]<-layer$measuredHeight
  crs(sp_layer) <- NA #delete original crs
  crs(sp_layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" #set new crs
  shp[[i]] <- sp_layer #write into new list
 }, error=function(e){message("WHops! Caught an error")})
}

index_ERROR_files<-which(sapply(shp, is.null))
Error_list<-files_list[c(index_ERROR_files)]

#Error handling loop
for (i in names(index_ERROR_files)){
  tryCatch(expr={
    print(i) #see where error occurs
    layer <- Error_list[[i]]
    layer <- st_as_sf(layer)
empty_geometry<-rep(NA, length=nrow(layer)) #output to get vector of all rows with empty geometry
for(x in 1:nrow(layer)){ #check every row in layer
  if(class(layer[x,]$geometry)[1]!="sfc_MULTILINESTRING"){ #check if geometry type is correct
    empty_geometry[x]<-x
  }else{}
  if(is.na(layer[x,]$measuredHeight)){ #check if height is NA
    empty_geometry[x]<-x 
  }else{}
} 
#QAQC output
print(paste0("there are ", sum(!is.na(empty_geometry)), " rows that were deleted"))
empty_geometry<-empty_geometry[!is.na(empty_geometry)] #drop NAs
if(length(empty_geometry)!=0){ #if there are empty geometries
  layer<-layer[-c(empty_geometry),] #drop rows with wrong geometry type
}else{}#do nothing

if(dim(layer)[1]!=0){ #check if layer has length zero
  layer <- st_polygonize(layer) #if not, polygonize
}
  else{ 
    print("layer is empty") #print message
    Error_list[i]<-NULL #if layer is zero, delete
    Files_list[i]<-NULL #delete also from files fist
    next
    } 
  if(any(st_is_empty(layer))){ #check if there are still empty geometries
    layer<-layer[!st_is_empty(layer),] #remove empty geometries
  }
    else{} #if no empty geometries, do nothing
  }, error=function(e){message("WHops! Caught a fatal error")})
}

#Error handling for conversion to sp polygon
for(i in names(Error_list)){
  tryCatch(expr={
    print(i)
    layer <- Error_list[[i]]
#disaggregate all polygons into list 
layer_list<- split(layer, seq(nrow(layer)))  
error_shp_list<-vector(mode='list', length=length(layer_list)) #new output list
#loop through list and convert every single polygon
for(z in 1:length(layer_list)){
  tryCatch(expr={
    #print(paste0("row ", z))
    layer_list[[z]]<-st_polygonize(layer_list[[z]])
  error_shp_list[[z]]<-as(st_geometry(layer_list[[z]]), "Spatial")
  if(validObject(error_shp_list[[z]])==FALSE){
    error_shp_list[[z]]<-NA
  }else{}
  }, error=function(e){message("WHops! Caught an error in conversion")})
}

#remove empty polygons 
null_index<-which(sapply(error_shp_list, is.null)) 
measuredHeight<-layer$measuredHeight #create vector for height
if(length(null_index)!=0){ #if there are heights to be remove
measuredHeight<-measuredHeight[-null_index] #remove null entries
}else{} #do nothing
error_shp_list[sapply(error_shp_list, is.null)] <- NULL #remove NULL entries

#Assign new Polygon IDs
orig_ID <- sapply(error_shp_list, function(x)
  slot(slot(x, "polygons")[[1]], "ID"))

new_IDs=paste0(orig_ID, 1:length(error_shp_list))
for (y in 1:length(error_shp_list)){
  slot(slot(error_shp_list[[y]], "polygons")[[1]], "ID") = new_IDs[y]
}

#Making new SpatialPolygon from list of polygons
new_layer <- SpatialPolygons(lapply(error_shp_list,
                               function(x) slot(x, "polygons")[[1]]))
#preserve height
shp_height[[i]]<-measuredHeight
#check if height values match polygons
if(length(new_layer)!=length(measuredHeight)){
  print("length unequal")
  #what to do then
}else{}
crs(new_layer) <- NA #delete original crs
crs(new_layer) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" #set new crs
Error_list[[i]] <- new_layer #write into new list
}, error=function(e){message("WHops! Caught a fatal error")})
}

#add Error_list entries back to shp
replacements<-intersect(names(Error_list), names(which(sapply(shp, is.null))))
shp<-modifyList(shp, Error_list[replacements])

length(which(sapply(shp, is.null))) #check how many list entries are NULL
shp[sapply(shp, is.null)] <- NULL #remove NULL entries
#16 entries are NULL

#same for shp_height
length(which(sapply(shp_height, is.null))) #check how many list entries are NULL
shp_height[sapply(shp_height, is.null)] <- NULL #remove NULL entries

#some heights are a list -> coerce to vector
shp_height_error<-which(sapply(shp_height, is.list))

shp[shp_height_error]<-NULL
shp_height[shp_height_error]<-NULL

#create new list
spdf<- vector(mode='list', length=length(shp)) #create empty list
names(spdf)<-names(shp)

#tranform to SpatialPolygonsDataFrame
for(i in 1:length(shp)){
  tryCatch(expr={
    print(i)
    if(any(is.na(names(shp[[i]])))){ #check if any IDs are NA
      na_poly<-which(is.na(names(shp[[i]]))) #which ID is NA
      shp[[i]]@polygons[[na_poly]]<-NULL #remove polygon with NA as ID
      shp_height[[i]]<-shp_height[[i]][-na_poly]
    }else{}
  spdf[[i]]<-as(shp[[i]], "SpatialPolygonsDataFrame")
  if(length(spdf[[i]])==length(shp_height[[i]])){ #length is the same
  spdf[[i]]$height<-shp_height[[i]]
  }else{
    print(paste0(i, " no height"))
  }
  }, error=function(e){message("WHops! Caught a fatal error")})
}

length(which(sapply(spdf, is.null))) #check how many list entries are NULL
#none
length(which(sapply(spdf, function(x) ncol(x@data))==1))

#rowbind list of spatialPolygonsdatafarme
obstacles_df <- do.call("rbind", spdf)

plot(obstacles_df)

ncols<-sapply(spdf, ncol)
any(ncols!=2)
which(ncols!=2)
spdf[[133]]

#test
plot(obstacles_df[1:100,])
#create output vectors
no_height_index=rep(FALSE, length(obstacles_df$height))
test_vec<-rep(NA, length(no_height_index))

#see wich heights are empty
for(i in 1:length(obstacles_df$height)){
  if(!is_empty(obstacles_df$height[[i]])){
    test_vec[i]<-unlist(obstacles_df$height[[i]])
  }else{
    no_height_index[i]<-TRUE
  }
}
#count number of empty heights
length(which(no_height_index==TRUE)) #9303

#remove rows with NA height
obstacles_df_complete<-obstacles_df[!no_height_index,] 
test_vec_complete<-test_vec[!no_height_index]

obstacles_df_complete$height_vec<-test_vec_complete
obstacles_df_complete$height<-NULL

#####create height raster####
sqrt(86042)
293*294
#create raster
r<-raster(nrow=293, ncol=294)
#set extent to polygon dataframe
extent(r) <- extent(obstacles_df_complete)
#rasterize polygons
svf_ras<-rasterize(x = obstacles_df_complete, y = r)#,  CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
crs(svf_ras) #check crs
ncell(svf_ras)
plot(svf_ras)
#####create spatial points####
svf_points<-spsample(obstacles_df_complete, n=length(obstacles_df_complete), type="stratified")
spplot(svf_points)
mapview(svf_points)

#save to file
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren/sky view factor")
write_rds(svf_ras, file="svf_ras.grd")

read_rds("svf_ras.grd") #check 

writeOGR(obj=obstacles_df_complete, driver="ESRI Shapefile", dsn="obstacles", layer="height")

readOGR("obstacles/height.shp") #test


####SVF####
#location: Raster* object, specifying the location(s) for which to calculate logical shadow values. Raster* cells are considered as ground location
#obstacles: SpatialPolygonsDataFrame object specifying the obstacles outline
#Name of attribute in obstacles with extrusion height for each feature

no_cores <- detectCores() - 1  

svf_test<-SVF(
  location=svf_ras,
  obstacles=obstacles_df_complete,
  obstacles_height_field="height_vec",
  res_angle = 5,
  b = 0.01,
  parallel = no_cores
)

#saga gis
#normalisiertes digitales Oberflächenmodell 50
