#calculate statistical parameters of predicted temperature

#model 1 day
diff(range(model_1_day_predict@data@values, na.rm=T))
sd(model_1_day_predict@data@values, na.rm=T)
mean(model_1_day_predict@data@values, na.rm=T) 

#model 1 night
diff(range(model_1_night_predict@data@values, na.rm=T))
sd(model_1_night_predict@data@values, na.rm=T)
mean(model_1_night_predict@data@values, na.rm=T)

#model 2 day
diff(range(model_2_day_predict@data@values, na.rm=T))
sd(model_2_day_predict@data@values, na.rm=T)
mean(model_2_day_predict@data@values, na.rm=T)

#model 2 night
diff(range(model_2_night_predict@data@values, na.rm=T))
sd(model_2_night_predict@data@values, na.rm=T)
mean(model_2_night_predict@data@values, na.rm=T)

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Maps")

#change name from ucz to lcz for model 1
names=model_1[["selectedvars"]]
varimp_1<-varImp(model_1)
rownames(varimp_1$importance)[2]<-"lcz"
#plot
png(filename="VarImp_model1.png", width=700, height=200)
plot(varimp_1, main="Model 1", 
     cex.lab=5, cex.axis=5, cex.main=10)
dev.off()
#change name from ucz to lcz for model 2
names=model_2[["selectedvars"]]
varimp_2<-varImp(model_2)
rownames(varimp_2$importance)[4]<-"lcz"

#plot
png(filename="VarImp_model2.png", width=700, height=200)
plot(varimp_2, main="Model 2", 
    cex.lab=5, cex.axis=5, cex.main=10)
dev.off()
#plot variable importance
plot(model_1) # see tuning results
plot(varImp(model_1))# variablenwichtigkeit
varImp(model_1)

plot(model_2) # see tuning results
plot(varImp(model_2)) # variablenwichtigkeit
varImp(model_2)

#model metrics
model_1$modelType
model_1$results
model_1$bestTune
model_1$selectedvars
model_1$selectedvars_perf_SE
model_1$metric
model_1$selectedvars_perf

#model metrics
model_2$modelType
model_2$results
model_2$bestTune
model_2$selectedvars
model_2$selectedvars_perf_SE
model_2$metric
model_2$selectedvars_perf