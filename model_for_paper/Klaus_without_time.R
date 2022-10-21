#predict Klaus without time predictor
setwd("C:/Users/Dana/sciebo/ndom/klaus_ohne_stunden")
#load new Klaus without time
model<-readRDS("ffs_Model_2022-10-19.RDS")
rm(pred_list, pred_plot_stack, pred_stack_temp)
##load static pred stack
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren")
pred_stack_06<-stack("pred_stack_06_20221012.grd")
names(pred_stack_06)[1:2]<-c("albedo","ndvi") #rename to match model
pred_stack_07<-stack("pred_stack_07_20221012.grd")
names(pred_stack_07)[1:2]<-c("albedo","ndvi") #rename to match model
#correct buildings
values(pred_stack_06$building_height)[is.na(values(pred_stack_06$building_height))]<-0
values(pred_stack_06$building_height_sd_3x3)[is.na(values(pred_stack_06$building_height_sd_3x3))]<-0
values(pred_stack_06$building_height_sd_5x5)[is.na(values(pred_stack_06$building_height_sd_5x5))]<-0
#for pred_stack_07
values(pred_stack_07$building_height)[is.na(values(pred_stack_07$building_height))]<-0
values(pred_stack_07$building_height_sd_3x3)[is.na(values(pred_stack_07$building_height_sd_3x3))]<-0
values(pred_stack_07$building_height_sd_5x5)[is.na(values(pred_stack_07$building_height_sd_5x5))]<-0

#create output list
pred_list<-vector(mode='list', length=100)
x=0 #initialise x
for(i in 700:800){
  print(i)
  x=x+1
  if(i<=222){ #take june pred stack
    #load meteo data
    meteo_name<-paste("meteo__", i, ".grd", sep = "")
    meteo_stack<-stack(paste("D:/Meteo/", meteo_name, sep = ""))
    #stack predictors
    pred_stack_temp<-stack(meteo_stack, pred_stack_06)
    #predict
    pred_list[[x]]<-predict(pred_stack_temp, model, savePrediction=TRUE)
  }else{ #take july pred stack
    #load meteo data
    meteo_name<-paste("meteo__", i, ".grd", sep = "")
    meteo_stack<-stack(paste("D:/Meteo/", meteo_name, sep = ""))
    #stack predictors
    pred_stack_temp<-stack(meteo_stack, pred_stack_07)
    #predict
    pred_list[[x]]<-predict(pred_stack_temp, model, savePrediction=TRUE)
  }
}
beep()
setwd("C:/Users/Dana/Desktop")
saveRDS(pred_list, file="pred_list_notime.RDS")
rm(pred_stack_06, pred_stack_07)
pred_list<-readRDS(file="pred_list_notime.RDS")
#stack all predictions
pred_plot_stack <- stack(pred_list)
rm(pred_list)
#get rows for meteodata
#rows<-as.numeric(substr(names(pred_plot_stack), start=7, stop=9))
#load meteo data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/Meteorologie")
meteo<-read.csv("meteo_all.csv")
meteo$datetime<-as.POSIXct(meteo$datetime)
#rename to time
names(pred_plot_stack)<-meteo$datetime[700:800]
#animate all layers of stack
saveGIF(animate(pred_plot_stack[[50:100]], pause=0.2, col=heat.colors(n=30,rev = T)),
        movie.name = "pred_plot_no_time_2.gif")
beep()
