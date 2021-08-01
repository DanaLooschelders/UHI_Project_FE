#third model 
#prep predictors
  #meteo: Temp, rH, cloud cover, wind, stability
      #GeoDach: cloud cover
      #Steinf: Temp, rH, wind, stability (calculated from temp)
  #LCZ (disaggregate to 10m)
  #Tree cover (10m)
setwd("C:/Users/Dana/sciebo/UHI_Projekt_Fernerkundung/Daten_bearbeitet/Copernicus")
#read in 10m Copernicus data
raster("copernicus_tree_cover_crop_MS_10m.tif")
#prep training data: logger and netatmo (30min res)
