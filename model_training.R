#create space folds
traintemps <- CreateSpacetimeFolds(total_stack,
                                   spacevar="ID", #use row.name as ID
                                   timevar="time", #use time as ID
                                   k=10)
#choice of k
#complete cases
names(training_dat)
#train model
model_ffs <- CAST::ffs(training_dat[,c("modis",
                                       "copernicus","dlm",
                                       "ucz")], #predictors
                       training_dat$temp, #response
                       method="rf", #randomForest
                       metric="RMSE", #RMSE bc it is a regression model (see ?train in caret)
                       ntree=500, #number of trees
                       tuneGrid=data.frame("mtry"=2:10),  #tuning
                       trControl=trainControl(method="cv",index=traintemps$index),
                       savePrediction=TRUE)
model_ffs

test_predict<-predict(pred_stack, model_ffs,savePrediction=TRUE)
