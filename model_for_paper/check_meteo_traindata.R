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

#check times where prediction is wrong
starttime<-"2020-06-11 20:00:00"
endtime<-"2020-06-12 10:00:00"
#2022 06 07 nachts
meteo_weird<-meteo[meteo$datetime>=starttime& meteo$datetime<=endtime,]
#set wd
setwd("C:/Users/Dana/sciebo/ndom/klaus/meteo_weird")
#plot only weird part
for(i in 2:ncol(meteo_weird)){
  plotname<-substr(colnames(meteo_weird)[i], start=7, stop=30)
  ggplot(data=meteo_weird, aes(x=datetime, meteo_weird[,i]))+
    geom_line()+
    theme_bw()+
    ylab(label=substr(colnames(meteo_weird)[i], start=7, stop=30))
  ggsave(filename=paste("Meteo_weird_", 
                        substr(colnames(meteo_weird)[i], start=7, stop=30),
                        ".jpg", sep=""), width = 300, 
         height=200, units="mm")
}

#plot training data for only that weird time
setwd("C:/Users/Dana/sciebo/ndom/klaus/meteo_weird")
#total_stack$time<-as.POSIXct(total_stack$time)
total_stack_weird<-total_stack[total_stack$time>=starttime&total_stack$time<=endtime,]
total_stack_weird$time<-as.factor(total_stack_weird$time)
#plot training data
#plot
ggplot(data=total_stack_weird)+
  geom_boxplot(aes(x = time, y = Temp))+
  theme_bw()+
  xlab("Temperature [°C]")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#save
ggsave(filename="training_temperatures_weird.jpg", width = 300, 
       height=200, units="mm")

#plot all
for(i in 2:ncol(total_stack_weird)){
  if(i==20){}#do nothing
  else{
    #plot
    ggplot(data=total_stack_weird)+
      geom_boxplot(aes(x = time, y = total_stack_weird[,i]))+
      theme_bw()+
      xlab(colnames(total_stack_weird)[i])
    #save
    ggsave(filename=paste("train_dat_weird_", 
                          colnames(total_stack_weird)[i],
                          ".jpg", sep=""), width = 300, 
           height=200, units="mm")
  }
}
