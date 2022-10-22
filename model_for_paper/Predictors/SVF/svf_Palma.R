#sky view factor
#library(sf)
#library(sfheaders)
library(sp)
#install.packages("shadow")
library(shadow)
library(parallel)
library(doParallel)
#library(purrr)
library(raster)
#install.packages("readr")
#library(readr)

setwd("V:/home/Dana_Looschelders/UHI_Modell/sky view factor")

load("svf.RData")

no_cores <- detectCores() - 1 

#mit raster
ext = as(raster::extent(obstacles_df_complete), "SpatialPolygons")
r = raster::raster(ext, res = c(10))
proj4string(r) = proj4string(obstacles_df_complete)

#calculate svf
svf_raster<-SVF(
  location=r,
  obstacles=obstacles_df_complete,
  obstacles_height_field="height_vec",
  parallel = no_cores
)


