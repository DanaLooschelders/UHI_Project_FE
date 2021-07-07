#create space folds
traintemps <- CreateSpacetimeFolds(total_stack,
                                   spacevar="ID", #use row.name as ID
                                   timevar="time", #use time as ID
                                   k=10)
#choice of k
#complete cases
#sicherstellen, dass keine NAs in Daten sind
total_stack <- total_stack[complete.cases(total_stack),]
#train model
model_ffs <- CAST::ffs(total_stack[,c("modis",
                                       "copernicus","dlm",
                                       "ucz")], #predictors
                       total_stack$temp, #response
                       method="rf", #randomForest
                       metric="RMSE", #RMSE bc it is a regression model (see ?train in caret)
                       ntree=500, #number of trees
                       tuneGrid=data.frame("mtry"=2:10),  #tuning
                       trControl=trainControl(method="cv",index=traintemps$index),
                       savePrediction=TRUE)
model_ffs

test_predict<-predict(pred_stack, model_ffs,savePrediction=TRUE)
