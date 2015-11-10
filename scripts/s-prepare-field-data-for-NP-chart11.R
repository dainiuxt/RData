library(dplyr)
# Calculate Field production from wells production
LIZ.ow <- LIZ.sub[order(as.Date(LIZ.sub$Date_m, format="%Y-%m-%d")),]
LIZ.ow$QOF <- LIZ.ow %>% group_by(Date_m) %>% summarize(QOM = sum(QOM))
LIZ.ow$QWM <- LIZ.sub %>% group_by(Date_m) %>% summarize(QWM = sum(QWM))
# Field production cumulative
LIZ.ow$CumOil <- cumsum(LIZ.ow$QOM)
