library(dplyr)
# Calculate Field production from wells production
NSD.sub <- NSD.sub %>% group_by(Date_m) %>% summarize(QOM = sum(QOM))
# Field production cumulative
NSD.sub$CumOil <- cumsum(NSD.sub$QOM)
