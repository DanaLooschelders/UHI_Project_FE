#Model validation and visualization
#test model for one modis file
modis <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
modis <- raster("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
#load meteo raster stack
meteo<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_Steinf/", "Meteo_", "MOD11A1_A2020189_12_47", sep=""))
meteo<-stack(paste("/Users/amelie/Desktop/LOEK/MSc/M8/Projekt/Sciebo/Daten_bearbeitet/Meteo_data_Steinf/", "Meteo_", "MOD11A1_A2020189_12_47", sep=""))
#stack modis and pred_stack and meteo_stack
pred_stack <- stack(modis,pred_resample, meteo)

names(pred_stack)<-c("modis", "copernicus", "dlm", "ucz", 
                     "meteo_RH", "meteo_Temp", "meteo_SWup")
#predict
test_predict<-predict(pred_stack, model_ffs, savePrediction=TRUE)

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

model_ffs

#plot variable importance
plot(model_ffs) # see tuning results
plot(varImp(model_ffs)) # variablenwichtigkeit

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

model_1$selectedvars
#plot areas inside AOA
test_predict2<-test_predict[test_aoa$AOA == 0] <- NA

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

