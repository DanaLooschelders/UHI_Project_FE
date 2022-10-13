setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Prädiktoren")
#load total stack
total_stack<-read.csv("total_stack_20221012.csv")  
#check how many rows will be removed
length(total_stack$Temp[is.na(total_stack$Temp)])

#complete cases
#sicherstellen, dass keine NAs in Daten sind
total_stack <- total_stack[complete.cases(total_stack),]

#delete columns
total_stack<-total_stack[,-c(21)]

write.csv(total_stack, "total_stack_20221012.csv", row.names=F)