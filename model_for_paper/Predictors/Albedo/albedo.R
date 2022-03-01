library(raster)
library(rgdal)
library(mapview)
library(ggplot2)
library(reshape2)
library(RStoolbox)

setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/")

sen2020_06_23 <- stack("NDVI/2020_1/out/TOA/S2A1C_20200623_108_ms_TOA_10.tif")
sen2020_06_01 <- stack("NDVI/2020_1/out/TOA/S2B1C_20200601_008_ms_TOA_10.tif")

sen2020_07 <- stack("NDVI/2020_2/out/TOA/S2B1C_20200731_008_ms_TOA_10.tif")

sen_10m_2020_07 <- stack(sen2020_07$S2B1C_20200731_008_ms_TOA_10.2, 
                         sen2020_07$S2B1C_20200731_008_ms_TOA_10.3,
                         sen2020_07$S2B1C_20200731_008_ms_TOA_10.4,
                         sen2020_07$S2B1C_20200731_008_ms_TOA_10.8)

mapview(sen_10m_2020_07)

#######  
#convert to 0-255 using the calc. function and basic raster algebra
ras8b <- calc(sen2020_07, fun=function(x){((x - min(x)) * 255)/(max(x)- min(x)) + 0})

#export 8b raster
writeRaster(ras8b, 'NDVI/2020_2/out/TOA/ras8b.tif', datatype='INT1U')
sen_8b_2020_07 <- stack("NDVI/2020_2/out/TOA/ras8b.tif")

sen_8b_10m_2020_07 <- stack(sen_8b_2020_07$ras8b.2,
                            sen_8b_2020_07$ras8b.3,
                            sen_8b_2020_07$ras8b.4,
                            sen_8b_2020_07$ras8b.8)

plotRGB(sen_8b_2020_07, r =4, b =2, g = 3)

set.seed(23)
pca_2020_07 <- rasterPCA(sen_8b_10m_2020_07, nComp = nlayers(sen_8b_10m_2020_07),maskCheck=F)
pca_2020_07$map

summary(pca_2020_07$model)
loadings(pca_2020_07$model)

ggRGB(pca_2020_07$map,2,3,1, stretch="lin", q=0)
mapview(pca_2020_07$map$PC1) 

#########

scaled_sen2020_07 <- rescaleImage(sen2020_07, ymin = 0, ymax= 255)
  
scaled_sen_10m_2020_07 <- stack(scaled_sen2020_07$layer.2,
                                scaled_sen2020_07$layer.3,
                                scaled_sen2020_07$layer.4,
                                scaled_sen2020_07$layer.8)
set.seed(23)
pca2_2020_07 <- rasterPCA(scaled_sen_10m_2020_07, nComp = nlayers(scaled_sen_10m_2020_07))
pca2_2020_07$map

summary(pca2_2020_07$model)
loadings(pca2_2020_07$model)

ggRGB(pca2_2020_07$map,2,3,1, stretch="lin", q=0)
mapview(pca2_2020_07$map$PC1)
