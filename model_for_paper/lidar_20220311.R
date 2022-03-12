setwd("/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/rbeit/lidar_daten/3dm_l_las_05515000_Münster_EPSG25832/")
#install.packages("rgdal")
library(rgdal)
library(sf)
library(ggplot2)
library(parallel)
library(multiplex)
library(mapedit)
library(shiny)
library(sp)
library(raster)
library(mapview)
library(rgl)
#install.packages("RCSF")
library(RCSF)
#####package lidR nutzen wie im Forum link: https://github.com/r-lidar/lidR
#remove.packages("lidR")
#install.packages("lidR")
library(lidR)
#install.packages("remotes")
#remotes::install_github("USGS-R/inlmisc")
library(remotes)

## Schritt 1
### lidar Daten als catalog einladen
LidarMS = readLAScatalog("/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/rbeit/lidar_daten/3dm_l_las_05515000_Münster_EPSG25832/")
LidarMS
plot(LidarMS)
plot(LidarMS, map=T)
plot(LidarMS, chunk = TRUE)
plot(LidarMS["Max.Z"])

str(LidarMS)
las_check(LidarMS)
summary(LidarMS)

# Testplot vom Geogebäude
geogebaeude <- clip_circle(LidarMS, x = 403550, y = 5758527, radius = 40)
plot(geogebaeude, bg = "white", size = 4, zcol="intensity")


ctg <- catalog("/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/rbeit/lidar_daten/3dm_l_las_05515000_Münster_EPSG25832/")
opt_output_files(ctg) <- "/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/rbeit/lidar_daten/"

rasterize_terrain(
  ctg,
  res = 1,
  algorithm = tin(),
  use_class = c(2L, 9L),
  shape = "convex")



