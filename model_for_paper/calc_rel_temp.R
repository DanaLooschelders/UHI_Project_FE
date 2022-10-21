#substract meteo_temp from training data to get the relative difference in temperature

#load training data
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/")
total_stack<-read.csv(file="total_stack_20221012.csv")
#calculate relative temperature
total_stack$rel_temp<-total_stack$Temp-total_stack$meteo_Temp
write.csv("total_stack_20221020_rel_temp.csv", row.names = F)

