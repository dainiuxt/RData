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

Ablinga <- ABL.sub
Ablinga <- setkey(setDT(Ablinga), Date_m, Well_N)[
  CJ(Date_m=seq(min(Date_m), max(Date_m), by='1 month'),
     Well_N=unique(Well_N))][is.na(QOM), QOM:=0][is.na(QWM), QWM:=0][is.na(QFM), QFM:=0][is.na(QIW), QIW:=0][is.na(QOD), QOD:=0][is.na(QWD), QWD:=0][is.na(QFD), QFD:=0][order(Well_N)]

mindate <- min(as.Date(Ablinga$Date_m))
minyear <- year(mindate)+1
minlab <- as.Date(paste(minyear, 01, 01, sep = "-"))
maxdate <- max(as.Date(Ablinga$Date_m))
maxyear <- year(maxdate)+1
maxlab <- as.Date(paste(maxyear, 01, 01, sep = "-"))

Ablinga <- Ablinga[order(as.Date(Ablinga$Date_m, format="%Y-%m-%d")),]
Ablinga = Ablinga %>% group_by(Well_N) %>% mutate(CumOil=cumsum(QOM))
Ablinga = Ablinga %>% group_by(Well_N) %>% mutate(CumWat=cumsum(QWM))
Ablinga = Ablinga %>% group_by(Well_N) %>% mutate(CumF=cumsum(QFM))
Ablinga = Ablinga %>% group_by(Well_N) %>% mutate(CumInj=cumsum(QIW))
Ablinga = Ablinga %>% group_by(Date_m) %>% mutate(QOMF = sum(QOM))
Ablinga = Ablinga %>% group_by(Date_m) %>% mutate(QWMF = sum(QWM))
Ablinga = Ablinga %>% group_by(Well_N) %>% mutate(CumOilF=cumsum(QOMF))

pdf("overview-charts/Ablinga.pdf", width = 23, height = 3)
# Charting (calling external function)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
pabl2 <- dtuwellprodchartow(Ablinga, "ABL2")
pabl7 <- dtuwellprodchartow(Ablinga, "ABL7")
pabl8 <- dtuwellprodchartow(Ablinga, "ABL8")
fprod <- dtufieldprodchartow(Ablinga)
print(pabl2, vp = vplayout(1,1))
print(pabl7, vp = vplayout(2,1))
print(pabl8, vp = vplayout(3,1))
print(fprod, vp = vplayout(4,1))

dev.off()
