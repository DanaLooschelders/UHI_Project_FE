#Model validation and visualization
#test model for one modis file
modis <- raster("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/FE_LST/terra_processed_resampled/terra_ terra__MOD11A1_A2020189_12_47_ .tif")
#load meteo raster stack
meteo<-stack(paste("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Meteo_data_Steinf/", "Meteo_", "MOD11A1_A2020189_12_47", sep=""))
#stack modis and pred_stack and meteo_stack
pred_stack <- stack(modis,pred_resample, meteo)

names(pred_stack)<-c("modis", "copernicus", "dlm", "ucz", 
                     "meteo_RH", "meteo_Temp", "meteo_SWup")
#predict
test_predict<-predict(pred_stack, model_ffs, savePrediction=TRUE)

#plot variable importance
plot(model_ffs) # see tuning results
plot(varImp(model_ffs)) # variablenwichtigkeit


#model performance
plot_ffs(model_ffs)
plot_ffs(model_ffs, type="selected")


model_ffs$modelType
model_ffs$results
model_ffs$bestTune
model_ffs$selectedvars
model_ffs$selectedvars_perf_SE
model_ffs$metric
model_ffs$selectedvars_perf

model_1$selectedvars


