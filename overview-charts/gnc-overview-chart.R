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

Genciai <- GEN.sub
Genciai <- setkey(setDT(Genciai), Date_m, Well_N)[
  CJ(Date_m=seq(min(Date_m), max(Date_m), by='1 month'),
     Well_N=unique(Well_N))][is.na(QOM), QOM:=0][is.na(QWM), QWM:=0][is.na(QFM), QFM:=0][is.na(QIW), QIW:=0][is.na(QOD), QOD:=0][is.na(QWD), QWD:=0][is.na(QFD), QFD:=0][order(Well_N)]

mindate <- min(as.Date(Genciai$Date_m))
minyear <- year(mindate)+1
minlab <- as.Date(paste(minyear, 01, 01, sep = "-"))
maxdate <- max(as.Date(Genciai$Date_m))
maxyear <- year(maxdate)
maxlab <- as.Date(paste(maxyear, 01, 01, sep = "-"))

Genciai <- Genciai[order(as.Date(Genciai$Date_m, format="%Y-%m-%d")),]
Genciai = Genciai %>% group_by(Well_N) %>% mutate(CumOil=cumsum(QOM))
Genciai = Genciai %>% group_by(Well_N) %>% mutate(CumWat=cumsum(QWM))
Genciai = Genciai %>% group_by(Well_N) %>% mutate(CumF=cumsum(QFM))
Genciai = Genciai %>% group_by(Well_N) %>% mutate(CumInj=cumsum(QIW))
Genciai = Genciai %>% group_by(Date_m) %>% mutate(QOMF = sum(QOM))
Genciai = Genciai %>% group_by(Date_m) %>% mutate(QWMF = sum(QWM))
Genciai = Genciai %>% group_by(Well_N) %>% mutate(CumOilF=cumsum(QOMF))

pdf("overview-charts/Genciai.pdf", width = 23, height = 3)
# Charting (calling external function)
grid.newpage()
pushViewport(viewport(layout = grid.layout(8,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
pgen2 <- dtuwellprodchartow(Genciai, "GEN2")
pgen3 <- dtuwellprodchartow(Genciai, "GEN3")
pgen6 <- dtuwellprodchartow(Genciai, "GEN6")
pgen8h <- dtuwellprodchartow(Genciai, "GEN8H")
pgen9 <- dtuwellprodchartow(Genciai, "GEN9")
pgen13h <- dtuwellprodchartow(Genciai, "GEN13H")
pgen14 <- dtuwellprodchartow(Genciai, "GEN14")
fprod <- dtufieldprodchartow(Genciai)
print(pgen2, vp = vplayout(1,1))
print(pgen3, vp = vplayout(2,1))
print(pgen6, vp = vplayout(3,1))
print(pgen8h, vp = vplayout(4,1))
print(pgen9, vp = vplayout(5,1))
print(pgen13h, vp = vplayout(6,1))
print(pgen14, vp = vplayout(7,1))
print(fprod, vp = vplayout(8,1))

dev.off()
