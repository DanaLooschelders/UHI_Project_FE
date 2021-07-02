#Model validation and visualization
map <- tm_shape(test_predict,
                raster.downsample = FALSE) +
  tm_raster(title = "Air Temperature")+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8)+
  tm_compass()
map

model_ffs

#model performance
plot_ffs(model_ffs)
plot_ffs(model_ffs, type="selected")

#calculate AOA
test_aoa<-aoa(pred_stack, model_ffs)
test_aoa$AOA[test_aoa$AOA==1]
spplot(test_aoa, zcol="AOA")

spplot(ratify(test_aoa$AOA), col.regions=c("black","white"))

ncell(test_aoa$AOA[test_aoa$AOA==1])/ncell(test_aoa$AOA)*100

spplot(test_aoa_factor$AOA)
confusionMatrix(factor(model_ffs$pred),factor(model_ffs$pred))

model_ffs$modelType
model_ffs$results
model_ffs$bestTune
model_ffs$selectedvars
model_ffs$selectedvars_perf_SE
model_ffs$metric
model_ffs$selectedvars_perf

#plot areas inside AOA
test_predict[test_aoa$AOA == 0] <- NA

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
