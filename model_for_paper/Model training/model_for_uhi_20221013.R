#install.packages("CAST") #does not work
#install dependencies
#install.packages("reshape")
#install older version from source
#install.packages("CAST_0.5.1.tar.gz", repos=NULL, type="source")
#install.packages("caret")
library(caret)
#Sys.which("make") #check that make is there
#install.packages("randomForest_4.6-14.tar.gz", repos=NULL, type="source")
library(randomForest)
library(foreach)
library(parallel)
library(doParallel)
library(CAST, lib.loc = "/home/p/p_loet03/uhi/")

#no_cores <- detectCores() - 1  
no_cores <- 20
cl <- makeCluster(no_cores) 
registerDoParallel(cl)

total_stack<-read.csv("total_stack_20220710.csv")
any(is.na(total_stack)) #no NAs
#create space folds
traintemps <- CreateSpacetimeFolds(total_stack,
                                   spacevar="ID", #use row.name as ID
                                   timevar="date_time", #use time as ID
                                   k=8)
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
                   ntree=50, #number of trees
                   tuneGrid=data.frame("mtry"=2:22),  #tuning
                   trControl=trainControl(method="cv",index=traintemps$index),
                   savePrediction=TRUE, cl=cl)

save(model,file="result.RData")
saveRDS(model,file="result_2.RDS") # modell speichern!
