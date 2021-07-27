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