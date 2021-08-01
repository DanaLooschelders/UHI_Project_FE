#AOA analysis
library(tmap)
library(raster)
library(tmaptools)
library(RColorBrewer)
library(mapview)
library(sp)
library(sf)
library(OpenStreetMap)
#create a map where the spatial information of areas ouside the AOA is visible
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Maps")
#the spatial information
ms_osm<-tmaptools::read_osm(bb(gadm), ext=1)

map_ms<-tm_shape(ms_osm)+
  tm_rgb()+
  tm_shape(gadm)+
  tm_borders()+
  tm_shape(steinf)+
  tm_dots(col = "red",size = 9, title = "Weather Station", legend.show=T)+
  tm_scale_bar(bg.color="white",position = c("right", "bottom"))

map_ms
tmap_save(map_ms, "Muenster_overview.png", width = 10, height=6)

test<-rasterize(ms_osm)

#the AOA
plot(model_2_day_aoa$AOA)

AOA_mask<-model_2_day_aoa$AOA
AOA_mask[values(AOA_mask)==0]<-NA
plot(AOA_mask)

#plot
aoa_ms<-tm_shape(ms_osm)+
  tm_rgb()+
  tm_shape(AOA_mask, 
           raster.downsample = FALSE) +
  tm_raster(palette="white", legend.show = F)+
  tm_shape(gadm)+
  tm_borders(lwd = 3)+
  tm_scale_bar(bg.color="white",position = c("right", "bottom"))+
  tm_compass(position = c("left","bottom"))+
  tm_add_legend(type = "fill",
                col="white",
                labels = "Inside AOA")+
  tm_layout(title.size = 0.8,
            legend.position = c("right", "top"),)
 
aoa_ms

tmap_save(aoa_ms, "Muenster_the_unknown.png", width = 10, height=6)
