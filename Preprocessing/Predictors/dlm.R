rm(list=ls() )
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Rohdaten_I/dlm")

library(sp)
library(sf) 
library(mapview)
library(raster)

#download dlm data
download.file("https://www.opengeodata.nrw.de/produkte/geobasis/lm/dlm50/dlm50_EPSG25832_Shape.zip", destfile ="dlm50.zip")
unzip("dlm50.zip",exdir="dlm")

#load polygon shapes into list
dlmlist <- list.files(path = "/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Rohdaten_I/dlm",
                     pattern="_f.shp",
                     all.files=TRUE, full.names=F)

#transfer names to list 
names(dlmlist) <- c("geb01_f.shp","geb02_f.shp","geb03_f.shp","gew01_f.shp","gew02_f.shp","rel01_f.shp",
                    "sie01_f.shp","sie02_f.shp","sie03_f.shp","sie04_f.shp","veg01_f.shp","veg02_f.shp",
                    "veg03_f.shp","veg04_f.shp","ver01_f.shp","ver03_f.shp","ver04_f.shp","ver05_f.shp",
                    "ver06_f.shp")

dlm <- lapply(dlmlist, shapefile)

#load shape of münster
gadm <- getData('GADM',country='DEU', level =2)
ms <- gadm[gadm$NAME_2 == "Münster",]
ms <- as(ms, "sf")
ms <- st_transform(ms, crs ="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#extract important variable
for (i in seq(dlm)){
  layer <- dlm[[i]]
  layer <- layer[,3]
  dlm[[i]] <- layer
}

#remove administrative layers
dlm_wo <- dlm[4:19]

#combine to one spdf 
combinedShp <- do.call(what = rbind.SpatialPolygonsDataFrame, args=dlm_wo)

#crop to shape of münster
e <- extent(395103.5,415705.1,5744177, 5768658)
dlm_crop <- crop(combinedShp, e)

#write Mode function
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = subset(x, !is.na(x))
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}    

#write empty raster 
e <- extent(395103.5,415705.1,5744177, 5768658)
projection <- crs(ms)
r <- raster(e,
            crs = projection)
res(r) <- 100 
    
#rasterize
dlm_raster <- rasterize(dlm_crop, r, field= as.numeric(dlm_crop$OBJART),
                        getCover=F, fun=Mode)

#project to WGS84
dlm_ext <- projectExtent(dlm_raster, crs="+proj=longlat +datum=WGS84 +no_defs")
dlm_proj <- projectRaster(dlm_raster, dlm_ext, method="ngb") 

#save Raster
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/dlm")
writeRaster(dlm_proj,"dlm_raster.tif", overwrite = T)
