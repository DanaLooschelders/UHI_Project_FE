library(CAST)
library(caret)
#parallel processing
library(doParallel) 
library(parallel)
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores) 
registerDoParallel(cl)


#create space folds
traintemps <- CreateSpacetimeFolds(total_stack,
                                   spacevar="ID", #use row.name as ID
                                   timevar="time", #use time as ID
                                   k=10)
#
names(total_stack)
#train model
model <- CAST::ffs(total_stack[,c( "albedo","ndvi","copernicus_water_wetness_3x3_MS_10m",
                                   "copernicus_water_wetness_5x5_MS_10m","copernicus_water_wetness_crop_MS_10m",
                                   "copernicus_tree_cover_3x3_MS_10m","copernicus_tree_cover_5x5_MS_10m", 
                                   "copernicus_tree_cover_crop_MS_10m","copernicus_imperviousness_3x3_MS_10m",
                                   "copernicus_imperviousness_5x5_MS_10m","copernicus_imperviousness_crop_MS_10m",
                                   "layer","ndom_crop_muenster_int_1m.1" ,"ndom_crop_muenster_int_1m.2", 
                                   "building_height", "building_height_sd_3x3","building_height_sd_5x5"  , 
                                   "Temp", "hours_sss","hours_ssr","meteo_Temp","meteo_rH","meteo_stability",
                                   "meteo_cloudcover","meteo_radiation", "meteo_cum_radiation", "meteo_precip",
                                   "meteo_precip3hour","meteo_precip1day","meteo_windspeed","meteo_winddirection")], #predictors
                         total_stack$Temp, #response
                         method="rf", #randomForest
                         metric="RMSE", #RMSE bc it is a regression model (see ?train in caret)
                         ntree=50, #number of trees
                         tuneGrid=data.frame("mtry"=2:30),  #tuning
                         trControl=trainControl(method="cv",index=traintemps$index),
                         savePrediction=TRUE, cl=cl)
model

#save model in sciebo

saveRDS(model,file="ffs_Model_2022-10-13.RDS") # modell speichern!


