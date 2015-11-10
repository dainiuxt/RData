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

Liziai <- LIZ.sub
Liziai <- setkey(setDT(Liziai), Date_m, Well_N)[
  CJ(Date_m=seq(min(Date_m), max(Date_m), by='1 month'),
     Well_N=unique(Well_N))][is.na(QOM), QOM:=0][is.na(QWM), QWM:=0][is.na(QFM), QFM:=0][is.na(QIW), QIW:=0][is.na(QOD), QOD:=0][is.na(QWD), QWD:=0][is.na(QFD), QFD:=0][order(Well_N)]

mindate <- min(as.Date(Liziai$Date_m))
minyear <- year(mindate)
minlab <- as.Date(paste(minyear, 01, 01, sep = "-"))
maxdate <- max(as.Date(Liziai$Date_m))
maxyear <- year(maxdate)+1
maxlab <- as.Date(paste(maxyear, 01, 01, sep = "-"))

Liziai <- Liziai[order(as.Date(Liziai$Date_m, format="%Y-%m-%d")),]
Liziai = Liziai %>% group_by(Well_N) %>% mutate(CumOil=cumsum(QOM))
Liziai = Liziai %>% group_by(Well_N) %>% mutate(CumWat=cumsum(QWM))
Liziai = Liziai %>% group_by(Well_N) %>% mutate(CumF=cumsum(QFM))
Liziai = Liziai %>% group_by(Well_N) %>% mutate(CumInj=cumsum(QIW))
Liziai = Liziai %>% group_by(Date_m) %>% mutate(QOMF = sum(QOM))
Liziai = Liziai %>% group_by(Date_m) %>% mutate(QWMF = sum(QWM))
Liziai = Liziai %>% group_by(Well_N) %>% mutate(CumOilF=cumsum(QOMF))

pdf("overview-charts/Liziai.pdf", width = 23, height = 3)
# Charting (calling external function)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
pliz1 <- dtuwellprodchartow(Liziai, "LIZ1")
pliz2 <- dtuwellprodchartow(Liziai, "LIZ2")
pliz3 <- dtuwellprodchartow(Liziai, "LIZ3")
pliz4 <- dtuwellprodchartow(Liziai, "LIZ4")
fprod <- dtufieldprodchartow(Liziai)
print(pliz1, vp = vplayout(1,1))
print(pliz2, vp = vplayout(2,1))
print(pliz3, vp = vplayout(3,1))
print(pliz4, vp = vplayout(4,1))

dev.off()
