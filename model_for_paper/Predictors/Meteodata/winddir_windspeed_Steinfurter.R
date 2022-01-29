### Skript fÃ¼r die Berechnung 
### 	1) der Windrichtung
### 	2) der mittleren Windrichtung
### 	3) der Standardabweichung der Windrichtung
### 	4) der Windgeschwindigkeit
### 	5) der mittleren Windgeschwindigkeit
### 	6) der Standardabweichung der Windgeschwindigkeit
### 	7) des Angle of Attacks
### 	8) der mittleren Angle of Attacks
### 	9) der Standardabweichung des Angle of Attacks
### aus u, v und w Daten von Anemometern, Modell: Gill R3-50 -> das ist wichtig zu wissen fÃ¼r das zugrundeliegende Koordinatensystem
### mit einem Loop der auf mehreren Kernen gerechnet wird, damit es schneller geht

### basiert auf folgendem Paper: 	Yamartino (1994): A Comparison of Several "Single-Pass" Estimators of the Standard Deviation of Wind Direction. 
###					Journal of Climate and applied Meteorology. Volume 23. p. 1362 ff.
### 					--> Formel 1 aus der Einleitung mit der anschlieÃenden Fallunterscheidung
### und Buch:				Stull (2000): Meteorology for scientists and engineers. A technical companion book with Ahrens' "Meteorology today".
###					2. Ed. Pacific Grove, CA, Brooks/Cole, Thomson Learning. p. 2.
###					--> Formel 1.2b

### Anmerkung 1: Im Paket "circular" von R ist eine NÃ¤herungsberechnung fÃ¼r die Standardabweichung der Windrichtung in einem Rechendurchgang implementiert. 
### 		Diese versagt aber bei Nordwind.
### 		Eine ordentliche Berechnung der Standardabweichung der Windrichtung benÃ¶tigt zwei DurchgÃ¤nge durch die Daten.

### Anmerkung 2: Es empfiehlt sich hochfrequente Anemometerdaten, die Ã¼ber heterogenem Terrain aufgenommen wurden einmal von EddyPro oder einem 
### Ã¤hnlichen Programm mit einer angle-of-attack Korrektur bearbeiten und ausgeben zu lassen. Bei EddyPro entspricht das einer "Rohdatenausgaben Level 4.
### Mit solchen Daten arbeitet das folgende Skript.


# Petra Steffen, Juni 2017
#modified by Dana Looschelders, Juni 2021
#######################################################################################
### Start
#######################################################################################

#install.packages("foreach") 
#install.packages("doParallel") 
library(foreach)
#library(doParallel)
### beide Packete werden benÃ¶tigt, damit alle Kerne im PC groÃe Loops parallel bearbeiten kÃ¶nnen um es schneller zu machen

### working directory, wo die Daten liegen
setwd("Z:/junk/Dana/20190820wind_conv/")
#list files to read in 
files_list=list.files(pattern ="\\.dat")

#read in Metadata
metadata_wind=read.table(files_list[1], sep = ",", dec = ".", header = F,skip=1,
                         na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)

metadata_wind=metadata_wind[1:3,]
#create header
header_wind=metadata_wind[1,]



#########################################################################################################
### Funktionen eingeben

### Windrichtung berechnen
fun.winddir <- function(u,v) ((180/(pi))*atan2(-v, u)) + 180 	### Formel 1.2b aus Stull(2000)
### u und v sind die jeweiligen Windkomponenten
### Angle of Attack (aoa) berechnen
fun.aoa <- function(u, v, w) (atan(w/(sqrt((u^2) + (v^2)))))*180/pi 

### Vektorielle Mittelung fÃ¼r die Windrichtung und die Berechnung der Standardabweichung, Formel 1.2b aus Stull(2000) mit mean
fun.mean_winddir <- function(u,v) ((180/(pi))*atan2(mean(-v, na.rm = TRUE),mean(u, na.rm = TRUE)))+180 

### nur fÃ¼r !nicht circulÃ¤re! Vektoren, normale sd-Berechnung mit vektoriellem Mittelwert (xq), fÃ¼r stdv vom aoa
fun.sd_vektor <- function(dat,x,xq) sqrt((1/(nrow(dat)-1))*sum((x - xq)^2, na.rm = TRUE)) ### dat: Dataframe zur Berechnung, 
### x: Spalte mit einzelnen EintrÃ¤gen,
### xq: mittlerer Vektor (z.B. Formel fun.mean_aoa)

### Mittelung des Angle-of-Attack auch vektoriell
fun.mean_aoa <- function(u, v, w) (atan(mean(w, na.rm = TRUE)/(sqrt(((mean(u, na.rm = TRUE))^2) + ((mean(v, na.rm = TRUE))^2)))))*180/pi

#########################################################################################################
### groÃer loop zum Einlesen der Daten und um alles zu berechnen

### Einstellungen fÃ¼r's parallele Abarbeiten mit mehreren Kernen
#l <- detectCores()
#cl<-makeCluster(l)
#registerDoParallel(cl)
remove(output)
dat=metadata_wind[-c(1:3),]
for(i in files_list) {
  if(!exists("output")) {
    j=1
    dat=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                   na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
    colnames(dat)=header_wind #set colnames
    dat$winddir <- fun.winddir(dat$u, dat$v)                          ### einzelne Windrichtung
    dat$attack <- fun.aoa(dat$u, dat$v, dat$w)                        ### einzelner AoA
    dat$hor_windspeed <- sqrt((dat$u^2)+(dat$v^2))                    ### einzelne Windgeschwindigkeit
    mean_winddir_me <- fun.mean_winddir(dat$u, dat$v)                 ### Berechnung der mittleren Windrichtung
    dat$abs1 <- abs(dat$winddir - mean_winddir_me)                    ### x - xq  (Yamartino 1994)
    dat$abs2 <- 360 - abs(dat$winddir - mean_winddir_me)              ### 360 - (x-xq) (Yamartino 1994)
    dat$min <- apply(dat[, c("abs1", "abs2")], MARGIN = 1, FUN = min) ### Minimum der letzten beiden Rechnungen raussuchen (Yamartino 1994)
    dat$min_sq <- dat$min^2                                           ### Minimum quadrieren, damit die nÃ¤chste Formel einfacher wird (Yamartino 1994)
    div <- 1/(nrow(dat) - sum(is.na(dat$min_sq))) 
    na_vals=colSums(is.na(dat[5:8]))
    #write in output
    output<-data.frame(name = i,											### was alles ausgegeben werden soll, Dateiname
                       timestamp_start = dat$TIMESTAMP[1],									### timestamp start of Interval 
                       timestamp_end=dat$TIMESTAMP[length(dat$TIMESTAMP)], ###timestamp end of interval
                       mean_windspeed = mean(dat$hor_windspeed, na.rm = TRUE),					### gemittelte Windgeschwindigkeit, normales "mean"
                       stdv_windspeed = sd(dat$hor_windspeed, na.rm = TRUE),					### Standardabweichung der Windgeschwindigkeit, normales "sd"
                       mean_aoa = fun.mean_aoa(dat$u, dat$v, dat$w),						### gemittelter Angle-of-Attack
                       stdv_aoa = fun.sd_vektor(dat, dat$attack, fun.mean_aoa(dat$u, dat$v, dat$w)),		### Standardabweichung des Angle-of-Attack
                       mean_winddir = fun.mean_winddir(dat$u, dat$v),								### mittlere Windrichtung
                       stdv_winddir_yama = sqrt((div * sum(dat$min_sq, na.rm = TRUE)) - ((div * sum(dat$min, na.rm = TRUE))^2)),	### Standardabweichung der Windrichtung nach Yamartino(1994)
                       nas_u = na_vals[1],
                       nas_v = na_vals[2],
                       nas_w = na_vals[3],
                       nas_Ts = na_vals[4]
    )
  } else {
    j=j+1
    dat=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                   na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
    colnames(dat)=header_wind #set colnames
    dat$winddir <- fun.winddir(dat$u, dat$v)                          ### einzelne Windrichtung
    dat$attack <- fun.aoa(dat$u, dat$v, dat$w)                        ### einzelner AoA
    dat$hor_windspeed <- sqrt((dat$u^2)+(dat$v^2))                    ### einzelne Windgeschwindigkeit
    mean_winddir_me <- fun.mean_winddir(dat$u, dat$v)                 ### Berechnung der mittleren Windrichtung
    dat$abs1 <- abs(dat$winddir - mean_winddir_me)                    ### x - xq  (Yamartino 1994)
    dat$abs2 <- 360 - abs(dat$winddir - mean_winddir_me)              ### 360 - (x-xq) (Yamartino 1994)
    dat$min <- apply(dat[, c("abs1", "abs2")], MARGIN = 1, FUN = min) ### Minimum der letzten beiden Rechnungen raussuchen (Yamartino 1994)
    dat$min_sq <- dat$min^2                                           ### Minimum quadrieren, damit die nÃ¤chste Formel einfacher wird (Yamartino 1994)
    div <- 1/(nrow(dat) - sum(is.na(dat$min_sq))) 
    na_vals=colSums(is.na(dat[5:8]))
    #write in output
    output_temp<-data.frame(name = i,											### was alles ausgegeben werden soll, Dateiname
                            timestamp_start = dat$TIMESTAMP[1],									### timestamp start of Interval 
                            timestamp_end=dat$TIMESTAMP[length(dat$TIMESTAMP)], ###timestamp end of interval
                            mean_windspeed = mean(dat$hor_windspeed, na.rm = TRUE),					### gemittelte Windgeschwindigkeit, normales "mean"
                            stdv_windspeed = sd(dat$hor_windspeed, na.rm = TRUE),					### Standardabweichung der Windgeschwindigkeit, normales "sd"
                            mean_aoa = fun.mean_aoa(dat$u, dat$v, dat$w),						### gemittelter Angle-of-Attack
                            stdv_aoa = fun.sd_vektor(dat, dat$attack, fun.mean_aoa(dat$u, dat$v, dat$w)),		### Standardabweichung des Angle-of-Attack
                            mean_winddir = fun.mean_winddir(dat$u, dat$v),								### mittlere Windrichtung
                            stdv_winddir_yama = sqrt((div * sum(dat$min_sq, na.rm = TRUE)) - ((div * sum(dat$min, na.rm = TRUE))^2)),	### Standardabweichung der Windrichtung nach Yamartino(1994)
                            nas_u = na_vals[1],
                            nas_v = na_vals[2],
                            nas_w = na_vals[3],
                            nas_Ts = na_vals[4]
    )
    output=rbind(output, output_temp)
    remove(output_temp)
  }
}

### abspeichern, eigenen Pfad eingeben
setwd("Z:/junk/Dana/20190820wind_conv/")
write.table(output, file = "wind_30min.csv",
            row.names = FALSE, col.names = TRUE, sep = ",", dec = ".", quote = FALSE)