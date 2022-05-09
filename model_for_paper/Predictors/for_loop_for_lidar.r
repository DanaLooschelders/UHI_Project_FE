library(sf)
library(raster)
#as for loop
setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/Paper/gml/gml")
#list files
files<-list.files(pattern=".gml")
#create empty files list
files_list<-vector(mode='list', length=length(files))
#set names of list to match files
names(files_list)<-files
#loop through all the files
for(i in files){
  tryCatch({
    files_list[[i]]<-read_sf(i, layer="Building")
  })
}
