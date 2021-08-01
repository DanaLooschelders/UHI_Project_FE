library(tmap)
library(raster)
library(tmaptools)
library(RColorBrewer)
library(mapview)
library(sp)
library(sf)
#install.packages("OpenStreetMap")
library(OpenStreetMap)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Maps")
#location Steinfurther Str
steinf<-SpatialPoints(coords = data.frame("lat"=51.980214, "lon"=7.599170),
                      proj4string = crs(gadm))

#create map of Muenster for Orientation
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
#create overall color scale from 8 °C to 35 °C
#cols=brewer.pal(9, "YlOrRd")
#logger distribution
map_logger <- tm_shape(shp = gadm)+
  tm_polygons()+
  tm_shape(spatial_list[[1]],
           raster.downsample = FALSE) +
  tm_dots(title = "Logger", size = 0.3, legend.show = T)+
  tm_scale_bar(bg.color="white",position = c("right", "bottom"))+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8)+
  tm_add_legend(type = "symbol",
                col="black",
                labels = "Logger")+
  tm_compass(position = c("left","bottom"))
map_logger

tmap_save(map_logger, "Logger_points.png")

#overview map with logger
map_ms_log<-tm_shape(ms_osm)+
  tm_rgb()+
  tm_shape(gadm)+
  tm_borders()+
  tm_shape(spatial_list[[1]],
           raster.downsample = FALSE) +
  tm_dots(title = "Logger", size = 0.1, legend.show = T)+
  tm_scale_bar(bg.color="white",position = c("right", "bottom"))+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8)+
  tm_add_legend(type = "symbol",
                col="black",
                labels = "Logger")+
  tm_compass(position = c("left","bottom"))

map_ms_log
tmap_save(map_ms_log, "Muenster_overview_logger.png", width = 10, height=6)

#set colors
cols_day<-mapviewPalette("mapviewSpectralColors")(40)[22:40]
cols_night<-mapviewPalette("mapviewSpectralColors")(40)[12:22]
#cols<-mapviewPalette("mapviewSpectralColors")(55)[25:55]
#visualize model 1 day

map <-   tm_shape(shp = gadm)+
  tm_polygons(col="black")+
  tm_shape(model_1_day_predict, 
           raster.downsample = FALSE) +
  tm_raster(title = "Predicted Air \nTemperature [°C]",
            legend.hist=T,
            palette = cols_day,
            breaks=seq(18,30, 1))+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            bg.color="white",
            legend.bg.alpha = 0.8,
            legend.outside=T,
            legend.title.size = 1,
            legend.outside.size = 0.5)+
  tm_add_legend(type = "fill",
                col="black",
                labels = "Outside AOA")+
  tm_compass(position = c("left","bottom"))
map

tmap_save(map, "map_model_1_day.png", width=10, height=7)
#visualize model 1 night

map <-   tm_shape(shp = gadm)+
  tm_polygons(col="black")+
  tm_shape(model_1_night_predict,
           raster.downsample = FALSE,) +
  tm_raster(title = "Predicted Air \nTemperature [°C]",
            legend.hist=T,
           palette = cols_night,
            breaks=seq(8,17, 1)
            )+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            bg.color="white",
            legend.bg.alpha = 0.8,
            legend.outside=T,
            legend.title.size = 1,
            legend.outside.size = 0.5)+
  tm_add_legend(type = "fill",
                col="black",
                labels = "Outside AOA")+
  tm_compass(position = c("left","bottom"))
map
tmap_save(map, "map_model_1_night.png", width=10, height=7)

#visualize model 2 day

map <-   tm_shape(shp = gadm)+
  tm_polygons(col="black")+
  tm_shape(model_2_day_predict,
           raster.downsample = FALSE) +
  tm_raster(title = "Predicted Air \nTemperature [°C]",
            legend.hist=T,
            palette = cols_day,
            breaks=seq(18,30, 1))+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,
          projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            bg.color="white",
            legend.bg.alpha = 0.8,
            legend.outside=T,
            legend.title.size = 1,
            legend.outside.size = 0.5)+
  tm_add_legend(type = "fill",
                col="black",
                labels = "Outside AOA")+
  tm_compass(position = c("left","bottom"))
map
tmap_save(map, "map_model_2_day.png", width=10, height=7)

##visualize model 2 night

map <-   tm_shape(shp = gadm)+
  tm_polygons(col="black")+
  tm_shape(model_2_night_predict,
           raster.downsample = FALSE) +
  tm_raster(title = "Predicted Air \nTemperature [°C]",
            legend.hist=T,
            palette = cols_night,
            breaks=seq(8,17, 1)
            )+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            bg.color="white",
            legend.bg.alpha = 0.8,
            legend.outside=T,
            legend.title.size = 1,
            legend.outside.size = 0.5)+
  tm_add_legend(type = "fill",
                col="black",
                labels = "Outside AOA")+
  tm_compass(position = c("left","bottom"))
map
tmap_save(map, "map_model_2_night.png", width=10, height=7)
