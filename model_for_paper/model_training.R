library(CAST)
library(caret)
#parallel processing
library(doParallel) 
library(parallel)
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores) 
registerDoParallel(cl)

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
#load total stack
total_stack<-read.csv("total_stack.csv")

#set height of non-existing buildings to 0 
total_stack$building_height[is.na(total_stack$building_height)]<-0
total_stack$building_height_sd_3x3[is.na(total_stack$building_height_sd_3x3)]<-0
total_stack$building_height_sd_5x5[is.na(total_stack$building_height_sd_5x5)]<-0

#set to 0
total_stack$hours_ssr[is.na(total_stack$hours_ssr)]<-0
total_stack$hours_sss[is.na(total_stack$hours_sss)]<-0

#check how many rows will be removed
length(total_stack$Temp[is.na(total_stack$Temp)])

#complete cases
#sicherstellen, dass keine NAs in Daten sind
total_stack <- total_stack[complete.cases(total_stack),]

#delete columns
total_stack<-total_stack[,-c(1,26)]

#create space folds
traintemps <- CreateSpacetimeFolds(total_stack,
                                   spacevar="ID", #use row.name as ID
                                   timevar="time", #use time as ID
                                   k=10)
#########choice of k??? 
names(total_stack)
#train model
model <- CAST::ffs(total_stack[,c( "albedo","ndvi", 
                                   "copernicus_water_wetness_3x3_MS_10m", 
                                   "copernicus_water_wetness_5x5_MS_10m",
                                   "copernicus_water_wetness_crop_MS_10m",
                                   "copernicus_tree_cover_3x3_MS_10m", 
                                   "copernicus_tree_cover_5x5_MS_10m",    
                                   "copernicus_tree_cover_crop_MS_10m",
                                   "copernicus_imperviousness_3x3_MS_10m", 
                                   "copernicus_imperviousness_5x5_MS_10m",
                                   "copernicus_imperviousness_crop_MS_10m",
                                   "building_height", "building_height_sd_3x3", "building_height_sd_5x5",
                                   "meteo_RH", "meteo_Temp", "meteo_stability", "meteo_cloudcover", 
                                   "meteo_radiation",  "meteo_cum_radiation" , "meteo_wind",
                                   "hours_sss", "hours_ssr")], #predictors
                         total_stack$Temp, #response
                         method="rf", #randomForest
                         metric="RMSE", #RMSE bc it is a regression model (see ?train in caret)
                         ntree=500, #number of trees
                         tuneGrid=data.frame("mtry"=2:22),  #tuning
                         trControl=trainControl(method="cv",index=traintemps$index),
                         savePrediction=TRUE, cl=cl)
model

#save model in sciebo
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Modell")
saveRDS(model,file="ffs_Model_2022-05-23.RDS") # modell speichern!

model<-readRDS(file="ffs_Model_2022-05-23.RDS")
