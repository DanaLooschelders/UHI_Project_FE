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
unique(dlm_ms_all$Art)
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

dlm_raster <- rasterize(dlm_ms_all, r, field= as.integer(dlm_ms_all$OBJART),
                       getCover=F, fun=Mode)

mapview(dlm_raster)

dlm_raster <- projectRaster(from=dlm_raster, crs=crs(gadm))

names <- unique(dlm_ms_all$OBJART_TXT)
nutzung <- as.data.frame(names)
nutzung$nummer <- unique(dlm_ms_all$OBJART)

setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/dlm")
writeRaster(dlm_raster,"dlm_raster_100m", overwrite = T)
