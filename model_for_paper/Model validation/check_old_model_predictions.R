#test old model for weird predictions
setwd("C:/Users/Dana/sciebo/Archive_UHI_Projekt_FE/Modelle")
model_old<-readRDS("ffs_Model_2021-08-10.RDS")
model_old$selectedvars
#prep meteo data 
setwd("C:/Users/Dana/sciebo/ndom/klaus/Prediction/Stacks_for_prediction/")
meteo_149<-stack("meteo__149.grd")
names(meteo_149)
meteo_149_old<-meteo_149[[c("meteo_Temp", "meteo_RH", "meteo_windspeed","meteo_cloudcover", "meteo_stability")]]
names(meteo_149_old)[3]<-"meteo_wind"
names(meteo_149_old)

#load copernicus
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Copernicus_grün_blau_grau/Treecover")
copernicus<-stack("copernicus_tree_cover_crop_MS_10m.tif")

pred_stack_old<-stack(meteo_149_old, copernicus)

#load ucz
setwd("C:/Users/Dana/sciebo/Archive_UHI_Projekt_FE/Daten_bearbeitet/FE_UCZ")
lcz<-raster("ucz_ms_100m.tif")
res(lcz)
#disaggregate to 10 m resolution
lcz_10m<-disaggregate(lcz, fact=10)
#resample to same resolution
lcz_10m_res <- resample(lcz_10m,copernicus, method="ngb")
pred_stack_old<-stack(copernicus, lcz_10m_res, meteo_149_old)
#check names
str(model_old$trainingData)
#rename some
names(pred_stack_old)[1]<-"copernicus"
names(pred_stack_old)[2]<-"ucz"
names(pred_stack_old)
#predict
test_pred<-predict(pred_stack_old, model_old)

setwd("C:/Users/Dana/sciebo/ndom/klaus/")

png(filename="old_test_pred.png", 
    width = 300, height=200, units="mm", res = 200)
spplot(test_pred)
dev.off()

model$selectedvars
