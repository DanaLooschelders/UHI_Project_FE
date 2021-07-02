#create space folds
traintemps <- CreateSpacetimeFolds(training_dat,
                                   spacevar="ID", #use row.name as ID
                                   k=3)
names(training_dat)
#train model
model_ffs <- CAST::ffs(training_dat[,c("terra__terra__MOD11A1_A2020189_12_47__",
                                       "copernicus_tree_cover_MS_100m","dlm_raster",
                                       "ucz_ms_100m")], #predictors
                       training_dat$Temp, #response
                       method="rf", #randomForest
                       metric="RMSE", #RMSE bc it is a regression model (see ?train in caret)
                       ntree=500, #number of trees
                       tuneGrid=data.frame("mtry"=2:10),  #tuning
                       trControl=trainControl(method="cv",index=traintemps$index),
                       savePrediction=TRUE)
model_ffs

test_predict<-predict(pred_stack, model_ffs,savePrediction=TRUE)
