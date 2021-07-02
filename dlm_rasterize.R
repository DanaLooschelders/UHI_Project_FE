rm(list=ls() )
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_LUC")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LUC/")
library(sp)
library(sf) 
library(mapview)
library(raster)
library(rgdal)

dlm_ms_all<- read_sf("dlm_ms_all")
mapview(dlm_ms_all)

gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")

crs(dlm_ms_all) 

unique(dlm_ms_all$OBJART)
dlm_ms_all$PolyID <- 1:nrow(dlm_ms_all)

e <- extent(395103.5,415705.1,5744177, 5768658)
projection <- crs(gadm)
r <- raster(e,
            crs = projection)
res(r) <- 100 

#mit der Spalte OBJARt als integer (die Objekte kann man später den Zahlen zuordnen)
# und fun=mode 
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = subset(x, !is.na(x))
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}    

getmode <- function(v,na.rm) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}   


dlm_raster <- rasterize(dlm_ms_all, r, field= as.numeric(dlm_ms_all$OBJART),
                        getCover=F, fun=Mode)
#dlm_raster <- fasterize(dlm_ms_all, r, 
 #                       by = as.integer("dlm_ms_all$OBJART"),background=0)

setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/dlm")
writeRaster(dlm_raster,"dlm_raster.tif", overwrite = T)

mapview(dlm_raster)
unique(dlm_raster)
dlm_raster_proj <- projectRaster(from=dlm_raster, crs ="+proj=longlat +datum=WGS84 +no_defs "  )

names <- unique(dlm_ms_all$OBJART_TXT)
nutzung <- as.data.frame(names)
nutzung$nummer <- unique(dlm_ms_all$OBJART)

setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/dlm")
writeRaster(dlm_raster_proj,"dlm_raster_100m.tif", overwrite = T)
