library(data.table)
library(dplyr)
Nausodis <- NSD.sub
Nausodis <- setkey(setDT(Nausodis), Date_m, Well_N)[
  CJ(Date_m=seq(min(Date_m), max(Date_m), by='1 month'),
     Well_N=unique(Well_N))][is.na(QOM), QOM:=0][is.na(QWM), QWM:=0][is.na(QFM), QFM:=0][is.na(QIW), QIW:=0][order(Well_N)]

Nausodis <- Nausodis[order(as.Date(Nausodis$Date_m, format="%Y-%m-%d")),]
Nausodis = Nausodis %>% group_by(Well_N) %>% mutate(CumOil=cumsum(QOM))
Nausodis = Nausodis %>% group_by(Well_N) %>% mutate(CumWat=cumsum(QWM))
Nausodis = Nausodis %>% group_by(Well_N) %>% mutate(CumF=cumsum(QFM))
Nausodis = Nausodis %>% group_by(Well_N) %>% mutate(CumInj=cumsum(QIW))
Nausodis = Nausodis %>% group_by(Date_m) %>% mutate(QOMF = sum(QOM))
Nausodis = Nausodis %>% group_by(Date_m) %>% mutate(QWMF = sum(QWM))
Nausodis = Nausodis %>% group_by(Well_N) %>% mutate(CumOilF=cumsum(QOMF))
