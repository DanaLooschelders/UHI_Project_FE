#Sensebox data
library(tidyverse)
library(sp)
library(leaflet)

setwd("C:/00_Dana/Uni/2. Mastersemester/Fernerkungsprojekt/SenseBox")

#read data
rain=read.table(file="Regenmenge_Juli2020.csv",
                sep=",", dec=".", header=T)
str(rain)
unique(rain$value)
rain$value[rain$value>0]

unique(rain$boxId)
