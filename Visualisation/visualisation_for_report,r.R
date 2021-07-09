#visualisation for report
library(tmap)
library(tmaptools)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Maps")
#plot logger distribution
map_logger <- tm_shape(shp = gadm)+
  tm_polygons()+
  tm_shape(spatial_list[[1]],
                raster.downsample = FALSE) +
  tm_dots(title = "Logger", size = 0.3, legend.show = T)+
  tm_scale_bar(bg.color="white",position = c("right", "bottom"))+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8,
            legend.outside=T)+
  tm_compass(position = c("left","bottom"))
map_logger

tmap_save(map_logger, "Logger_points.png")

#plot prediction maps
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modelle")
#load models
model_1<-readRDS(file="ffs_Model_2021-07-07.RDS")
model_2<-readRDS(file="ffs_Model_2021-07-08.RDS")

