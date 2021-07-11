rm(list=ls() )
setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_roh/FE_LUC")
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_roh/FE_LUC/")
library(sp)
library(sf) 
library(mapview)
library(raster)
library(rgdal)

#load dlm data
dlm_ms_all<- read_sf("dlm_ms_all")
#mapview(dlm_ms_all)

#load shape of münster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")

#build empty raster 
e <- extent(395103.5,415705.1,5744177, 5768658)
projection <- crs(gadm)
r <- raster(e,
            crs = projection)
res(r) <- 100 

#write mode function 
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = subset(x, !is.na(x))
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}    

#rasterize into empty raster with mode 
dlm_raster <- rasterize(dlm_ms_all, r, field= as.numeric(dlm_ms_all$OBJART),
                        getCover=F, fun=Mode)
#dlm_raster <- fasterize(dlm_ms_all, r, 
#                       by = as.integer("dlm_ms_all$OBJART"),background=0)

mapview(dlm_raster)
unique(dlm_raster)

#save as raster 
#setwd("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/dlm")
writeRaster(dlm_raster,"dlm_raster.tif", overwrite = T)

#dataframe of number and names of use 
names <- unique(dlm_ms_all$OBJART_TXT)
nutzung <- as.data.frame(names)
nutzung$nummer <- unique(dlm_ms_all$OBJART)