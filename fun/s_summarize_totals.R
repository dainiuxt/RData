library("dplyr")
by_wells <- group_by(monthly, Well_N)
#Calculates production totals by well up to date and writes CSV in working directory.
totals <- summarise(by_wells, sum(QOM, na.rm=TRUE), sum(QWM, na.rm=TRUE), sum(QFM, na.rm=TRUE), sum(QIW, na.rm=TRUE))
write.table(totals, file=paste(Sys.Date(),"prod-totals-by-wells.csv"), dec=",", sep=";",row.names=F)
writeLines("\"prod-totals-by-wells.csv\" saved.")
