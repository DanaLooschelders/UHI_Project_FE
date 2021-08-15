#stats and validation of model 3
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Maps")
varimp<-varImp(model_3)
rownames(varimp$importance)[5]<-"lcz"
plot(varimp)
#plot
png(filename="VarImp_model3.png", width=700, height=200)
plot(varimp, main="Model 3", 
     cex.lab=5, cex.axis=5, cex.main=10)
dev.off()

##model 1 day
model_3_day_predict<-mask(model_3_day_predict, gadm) #Mask areas outside Münster
mapview(model_3_day_predict)

diff(range(model_3_day_predict@data@values, na.rm=T))
sd(model_3_day_predict@data@values, na.rm=T)
mean(model_3_day_predict@data@values, na.rm=T) 

#model 1 night
diff(range(model_3_night_predict@data@values, na.rm=T))
sd(model_3_night_predict@data@values, na.rm=T)
mean(model_3_night_predict@data@values, na.rm=T)

pred_stack