#visualize
map <- tm_shape(test_predict,
                raster.downsample = FALSE) +
  tm_raster(title = "Air Temperature")+
  tm_scale_bar(bg.color="white",position = c("right", "bottom"))+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8,
            legend.outside=T)+
  tm_compass(position = c("left","bottom"))
map

map <- tm_shape(test_predict,
                raster.downsample = FALSE) +
  tm_raster(title = "LUC")+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            bg.color="black",
            legend.bg.alpha = 0.8)+
  tm_add_legend(type = "fill",
                col="black",
                labels = "Outside AOA")
map
