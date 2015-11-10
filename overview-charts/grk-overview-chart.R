#loading required libraries
library(data.table)
library(ggplot2)
library(grid)
library(magrittr)
library(dplyr)
library(gtable)
library(ggthemes)
library(gridBase)
library(png)
library(gridExtra)
library(scales)

Girkaliai <- GRK.sub
Girkaliai <- setkey(setDT(Girkaliai), Date_m, Well_N)[
  CJ(Date_m=seq(min(Date_m), max(Date_m), by='1 month'),
     Well_N=unique(Well_N))][is.na(QOM), QOM:=0][is.na(QWM), QWM:=0][is.na(QFM), QFM:=0][is.na(QIW), QIW:=0][is.na(QOD), QOD:=0][is.na(QWD), QWD:=0][is.na(QFD), QFD:=0][order(Well_N)]

mindate <- min(as.Date(Girkaliai$Date_m))
minyear <- year(mindate)+1
minlab <- as.Date(paste(minyear, 01, 01, sep = "-"))
maxdate <- max(as.Date(Girkaliai$Date_m))
maxyear <- year(maxdate)
maxlab <- as.Date(paste(maxyear, 01, 01, sep = "-"))

Girkaliai <- Girkaliai[order(as.Date(Girkaliai$Date_m, format="%Y-%m-%d")),]
Girkaliai = Girkaliai %>% group_by(Well_N) %>% mutate(CumOil=cumsum(QOM))
Girkaliai = Girkaliai %>% group_by(Well_N) %>% mutate(CumWat=cumsum(QWM))
Girkaliai = Girkaliai %>% group_by(Well_N) %>% mutate(CumF=cumsum(QFM))
Girkaliai = Girkaliai %>% group_by(Well_N) %>% mutate(CumInj=cumsum(QIW))
Girkaliai = Girkaliai %>% group_by(Date_m) %>% mutate(QOMF = sum(QOM))
Girkaliai = Girkaliai %>% group_by(Date_m) %>% mutate(QWMF = sum(QWM))
Girkaliai = Girkaliai %>% group_by(Well_N) %>% mutate(CumOilF=cumsum(QOMF))

pdf("overview-charts/Girkaliai.pdf", width = 23, height = 3)
# Charting (calling external function)
grid.newpage()
pushViewport(viewport(layout = grid.layout(8,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
pgrk2 <- dtuwellprodchartow(Girkaliai, "GRK2")
pgrk5 <- dtuwellprodchartow(Girkaliai, "GRK5")
pgrk6 <- dtuwellprodchartow(Girkaliai, "GRK6")
pgrk6r <- dtuwellprodchartow(Girkaliai, "GRK6R")
pgrk7 <- dtuwellprodchartow(Girkaliai, "GRK7")
pgrk8 <- dtuwellprodchartow(Girkaliai, "GRK8")
pgrk9 <- dtuwellprodchartow(Girkaliai, "GRK9")
fprod <- dtufieldprodchartow(Girkaliai)
print(pgrk2, vp = vplayout(1,1))
print(pgrk5, vp = vplayout(2,1))
print(pgrk6, vp = vplayout(3,1))
print(pgrk6r, vp = vplayout(4,1))
print(pgrk7, vp = vplayout(5,1))
print(pgrk8, vp = vplayout(6,1))
print(pgrk9, vp = vplayout(7,1))
print(fprod, vp = vplayout(8,1))

dev.off()
