library(raster)
library(rgdal)
library(mapview)
library(RStoolbox)

setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/")

#data has been downloaded in the script "ndvi.R" 
boa2020_06_23 <- stack("/Volumes/work/l2_dat/out/BOA/S2A2A_20200623_108_ms_BOA_10.tif") #load boa data for june 
boa2020_06_01 <- stack("/Volumes/work/l2_dat/out/BOA/S2B2A_20200601_008_ms_BOA_10.tif")

uberlay <- function(..., fun) {
  fun <- match.fun(fun)
  L <- lapply(list(...), unstack)
  stack(do.call(mapply, c(FUN=function(...) calc(stack(...), fun), L)))
}

boa2020_06_mean <- uberlay(boa2020_06_01, boa2020_06_23, fun='mean') #calculate mean of two scenes

scaled_boa_10m_2020_06 <- stack(boa2020_06_mean$layer.2, 
                                boa2020_06_mean$layer.3,
                                boa2020_06_mean$layer.4,
                                boa2020_06_mean$layer.8) #select bands 
set.seed(23)
pca_boa_2020_06 <- rasterPCA(scaled_boa_10m_2020_06, nComp = nlayers(scaled_boa_10m_2020_06)) #calculate pca
albedo_boa_06 <- pca_boa_2020_06$map$PC1 # irst pc as albedo

#boa data for july 
boa_2020_07_13 <- stack("/Volumes/work/l2_dat/out/BOA/S2A2A_20200713_108_ms_BOA_10.tif") #one cloud right in the centre
boa_2020_07_23 <- stack("/Volumes/work/l2_dat/out/BOA/S2A2A_20200723_108_ms_BOA_10.tif") #too many clouds in the southeast
boa_2020_07_31 <- stack("/Volumes/work/l2_dat/out/BOA/S2B2A_20200731_008_ms_BOA_10.tif")


mapview(boa_2020_07_13$S2A2A_20200713_108_ms_BOA_10.2)
ggRGB(boa_2020_07_31, r = 4, g = 3, b = 2)

scaled_boa_10m_2020_07 <- stack(boa_2020_07_31$S2B2A_20200731_008_ms_BOA_10.2,
                                boa_2020_07_31$S2B2A_20200731_008_ms_BOA_10.3,
                                boa_2020_07_31$S2B2A_20200731_008_ms_BOA_10.4,
                                boa_2020_07_31$S2B2A_20200731_008_ms_BOA_10.8)

set.seed(23)
pca_boa_2020_07 <- rasterPCA(scaled_boa_10m_2020_07, nComp = nlayers(scaled_boa_10m_2020_07))
albedo_boa_07 <- pca_boa_2020_07$map$PC1

#comparison of differences between june and july 
dif <- overlay(albedo_boa_07, albedo_boa_06, fun=function(r1, r2){return(r2-r1)}) #calculate difference in albedo

ndvi_06 <- raster("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/ndvi/ndvi_ms_final_2020_06.tif")
ndvi_07 <- raster("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/ndvi/ndvi_ms_final_2020_07.tif")
dif_ndvi <- overlay(ndvi_07, ndvi_06, fun=function(r1, r2){return(r2-r1)}) #calculate difference in ndvi 

par(mfrow=c(1,2))
plot(dif, col=brewer.pal(name = "RdBu"))
plot(dif_ndvi) 

#write raster 
setwd("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/Albedo")
writeRaster(albedo_boa_06, "albedo_boa_ms_2020_06.tif")
writeRaster(albedo_boa_07, "albedo_boa_ms_2020_07.tif")

##### put albedo and ndvi together ### 
albedo_06 <- raster("albedo_boa_ms_2020_06.tif")
albedo_07 <- raster("albedo_boa_ms_2020_07.tif")

ndvi_06 <- raster("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/ndvi/ndvi_ms_final_2020_06.tif")
ndvi_07 <- raster("/Users/ameliewendiggensen/sciebo/UHI_Projekt_Fernerkundung/Paper/Prädiktoren/ndvi/ndvi_ms_final_2020_07.tif")

albedo_ndvi_06 <-stack(albedo_06, ndvi_06)
names(albedo_ndvi_06) <- c("albedo","ndvi")
writeRaster(albedo_ndvi_06, "albedo_ndvi_final_06.tif", overwrite=TRUE)

albedo_ndvi_07 <-stack(albedo_07, ndvi_07)
names(albedo_ndvi_07) <- c("albedo","ndvi")
writeRaster(albedo_ndvi_07, "albedo_ndvi_final_07.tif", overwrite=TRUE)
