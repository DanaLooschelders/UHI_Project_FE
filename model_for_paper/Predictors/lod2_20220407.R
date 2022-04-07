rm(list=ls() )

setwd("/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/lod2/3d-gm_lod2_kacheln/")

library(abind)
library(sf)
library(stars)
library(multiplex)
library(mapview)
library(shadow)
library(shadow)
library(igraph)
library(ggplot2)
install.packages("R.oo")
library(R.oo)


gmllist <- list.files("/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/lod2/3d-gm_lod2_kacheln/", pattern = "*.gml", full.names = TRUE)

gmllist <- lapply(gmllist, read_sf)

gfslist <- list.files("/Users/pialoettert/Documents/masterdesaster/fernerkundung/uhi-paper/lod2/3d-gm_lod2_kacheln/", pattern = "*.gfs", full.names = TRUE)

gfslist <- lapply(gfslist, read_sf)

test <- read_sf("3d-gm_lod2_kacheln/LoD2_32_395_5756_1_NW.gml")

test2 <- read.graph("3d-gm_lod2_kacheln/LoD2_32_395_5756_1_NW.gml",format=c("gml"))

test2 <- read.graph("testdaten_3d-gm-lod2_citygml.gml",format=c("gml"))

test <- read_sf("testdaten_3d-gm-lod2_citygml.gml")

# Danas GML Skript 
lod2test<-read_sf("testdaten_3d-gm-lod2_citygml.gml",layer = "Building")

lod2<-read_sf("3d-gm_lod2_kacheln/LoD2_32_395_5756_1_NW.gml",layer = "Building")

gmllist <- tryCatch(lapply(gmllist, read_sf, layer = "Building"))


gmllist2<- as.list(list.files(pattern='.gml'))
files <- lapply(gmllist2, function(x) read_sf(x))

gmllist

str(gmllist)

lod2<-read_sf(gmllist[[1]],layer = "Building")

ggplot(data=lod2)+
  geom_sf(aes(color=measuredHeight))





