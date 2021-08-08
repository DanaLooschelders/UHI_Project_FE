#training data
#create dataframe per time
which(colnames(all_temp)=="datetime")
for(i in 1:length(all_temp$datetime)){
  if(i==1){
    all_temp_list<-list()
    temp_dat<-data.frame(ID<-as.character(colnames(all_temp)[-1]), 
                         temperature<-t(all_temp[i,-1]))
    all_temp_list[[i]]<-merge(all_metadata, temp_dat, by="row.names" )
    names(all_temp_list[[i]])<-c("rownames", "Lat", "Lon","index", "Logger_ID", "Temperature")
    names(all_temp_list)[[i]]<-i
  }else{
    temp_dat<-data.frame(ID<-as.character(colnames(all_temp)[-1]), 
                         temperature<-t(all_temp[i,-1]))
    all_temp_list[[i]]<-merge(all_metadata,temp_dat, by="row.names" )
    names(all_temp_list[[i]])<-c("rownames", "Lat", "Lon","index", "Logger_ID", "Temperature")
    names(all_temp_list)[[i]]<-i
    }
}


#create spatialpointsdataframe with logger coordinates
for(i in 1:length(all_temp_list)){
  if(i ==1){
    spatial_list_all=all_temp_list
    spatial_list_all[[i]]<-SpatialPointsDataFrame(coords=spatial_list_all[[i]][,c(3,2)], 
                                              data=data.frame(Temp=spatial_list_all[[i]][,6]),
                                              proj4string=CRS(as.character(crs(gadm))))
    
  }else{
    spatial_list_all[[i]]<-SpatialPointsDataFrame(coords=spatial_list_all[[i]][,c(3,2)],
                                              data=data.frame(Temp=spatial_list_all[[i]][,6]),
                                              proj4string=CRS(as.character(crs(gadm))))
  }
}

#spTransform(spatial_list_all[[1]], CRSobj = crs(gadm))