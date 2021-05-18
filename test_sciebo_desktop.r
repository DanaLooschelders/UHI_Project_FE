#download the sciebo desktop client from:
#https://hochschulcloud.nrw/de/download/index.html
#synchronize with local hard disc
#the sciebo symbol should appear in your file structure
#set the working directory to the sciebo file
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Orga/")
#test to write into an dread from the file
write.csv(x = rain,file="raintest.csv")
read.csv(file = "raintest.csv")
#whoooooo :D 