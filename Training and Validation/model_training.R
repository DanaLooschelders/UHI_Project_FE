#complete cases
#sicherstellen, dass keine NAs in Daten sind
total_stack <- total_stack[complete.cases(total_stack),]

#create space folds
traintemps <- CreateSpacetimeFolds(total_stack,
                                   spacevar="ID", #use row.name as ID
                                   timevar="time", #use time as ID
                                   k=10)
#########choice of k??? 

#train model
model_ffs <- CAST::ffs(total_stack[,c("modis", "copernicus", "dlm", "ucz", 
                                      "meteo_RH", "meteo_Temp", "meteo_SWup")], #predictors
                       total_stack$temp, #response
                       method="rf", #randomForest
                       metric="RMSE", #RMSE bc it is a regression model (see ?train in caret)
                       ntree=500, #number of trees
                       tuneGrid=data.frame("mtry"=2:7),  #tuning
                       trControl=trainControl(method="cv",index=traintemps$index),
                       savePrediction=TRUE)
model_ffs

#save model in sciebo
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modelle")
saveRDS(model_ffs,file="ffs_Model_2021-07-08.RDS") # modell speichern!

model_1<-readRDS(file="ffs_Model_2021-07-07.RDS")
