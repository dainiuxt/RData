library("dplyr")
library("lubridate")


#Replacing NAs with "0"
monthly$QOM[is.na(monthly$QOM)] <- 0
monthly$QWM[is.na(monthly$QWM)] <- 0
monthly$QIW[is.na(monthly$QIW)] <- 0

#Creating calculated variables from read data
monthly$QFM <- monthly$QOM + monthly$QWM #Liquid production, m3/month
monthly$WCT <- (monthly$QWM / monthly$QFM) * 100 #WaterCut, % (WCT)
monthly$QOD <- monthly$QOM / day(monthly$Date_m) #Average oil daily flowrate
monthly$QWD <- monthly$QWM / day(monthly$Date_m) #Average water daily flowrate
monthly$QFD <- monthly$QOD + monthly$QWD #Average liquid daily flowrate
monthly$QIWD <- monthly$QIW / day(monthly$Date_m) #Average water daily injection

monthly <- monthly[order(as.Date(monthly$Date_m, format="%Y-%m-%d")),]
monthly = monthly %>% group_by(Well_N) %>% mutate(CumOil=cumsum(QOM))
monthly = monthly %>% group_by(Well_N) %>% mutate(CumWat=cumsum(QWM))
monthly = monthly %>% group_by(Well_N) %>% mutate(CumF=cumsum(QFM))
monthly = monthly %>% group_by(Well_N) %>% mutate(CumInj=cumsum(QIW))

monthly$year <- year(monthly$Date_m)
monthly$month <- month(monthly$Date_m)
monthly$Date <- as.Date(paste(monthly$year, monthly$month, 01, sep = "-"))
monthly$Date_m <- monthly$Date
monthly$Date <- NULL
monthly$year <- NULL
monthly$month <- NULL
