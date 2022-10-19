#Check Meteo Data
library(ggplot2)
setwd("C:/Users/Dana/sciebo/ndom/klaus/meteoplots")

for(i in 2:ncol(meteo)){
plotname<-substr(colnames(meteo)[i], start=7, stop=30)
ggplot(data=meteo, aes(x=datetime, meteo[,i]))+
  geom_line()+
  theme_bw()+
  ylab(label=substr(colnames(meteo)[i], start=7, stop=30))
ggsave(filename=paste("Meteo_", 
                      substr(colnames(meteo)[i], start=7, stop=30),
                      ".jpg", sep=""), width = 300, 
       height=200, units="mm")
}

model$selectedvars

range(model$trainingData$.outcome) #6.5 41.4

setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Praediktoren/")
total_stack<-read.csv(file="total_stack_20221012.csv")

total_stack$time<-as.factor(total_stack$time)
setwd("C:/Users/Dana/sciebo/ndom/klaus/traindataplots")
#plot every column as boxplot for every timestep
for(i in 2:ncol(total_stack)){
  if(i==20){}#do nothing
  else{
  #plot
ggplot(data=total_stack)+
  geom_boxplot(aes(x = time, y = total_stack[,i]))+
    theme_bw()+
    xlab(colnames(total_stack)[i])
  #save
ggsave(filename=paste("train_dat_", 
                      colnames(total_stack)[i],
                      ".jpg", sep=""), width = 300, 
       height=200, units="mm")
}
}
