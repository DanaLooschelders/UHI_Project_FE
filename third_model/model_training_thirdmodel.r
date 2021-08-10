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
model_three <- CAST::ffs(total_stack[,c( "copernicus", "ucz", 
                                      "meteo_RH", "meteo_Temp", "meteo_stability",
                                      "meteo_cloudcover", "meteo_wind")], #predictors
                       total_stack$temp, #response
                       method="rf", #randomForest
                       metric="RMSE", #RMSE bc it is a regression model (see ?train in caret)
                       ntree=500, #number of trees
                       tuneGrid=data.frame("mtry"=2:7),  #tuning
                       trControl=trainControl(method="cv",index=traintemps$index),
                       savePrediction=TRUE)
model_three

#save model in sciebo
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modelle")
saveRDS(model_three,file="ffs_Model_2021-08-10.RDS") # modell speichern!

model_1<-readRDS(file="ffs_Model_2021-07-07.RDS")
