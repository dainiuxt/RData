monthly2 <- monthly
monthly3 <- monthly2[!is.na(monthly2),]

#subsetting data for plotting
wellprod <- split(monthly, monthly$Well_N, drop=TRUE)
i<-1;
while (i<=93) {
     lapply(wellprod[[i]], as.double);
     i<-i+1;
}
unlist(wellprod)
#grouping data for charts
idx <- c(1, diff(monthly$Date_m))
i2 <- c(1,which(idx != 1), nrow(monthly)+1)
monthly$grp <- rep(1:length(diff(i2)), diff(i2))
monthly[["grp"]] <- as.numeric(monthly[["grp"]])

#Setting NA values to zero in Oil, Water production rates & water injection rates
monthly$QOM[is.na(monthly$QOM)]<-0
monthly$QWM[is.na(monthly$QWM)]<-0
monthly$QIW[is.na(monthly$QIW)]<-0

g<-ggplot(monthly[monthly$Well_N == "KRT5",], aes(x = Date_m)) +
     geom_line(aes(y = QOM, colour = "Oil, m3/month")) +
     geom_line(aes(y = QWM, colour = "Water, m3/month")) +
     geom_line(aes(y = QFM, colour = "Liquid, m3/month")) +
     geom_line(aes(y = WCT*10, colour = "WCT (100=10%)")) +

     geom_point(aes(y = QOM, colour = "Oil, m3/month")) +
     geom_point(aes(y = QWM, colour = "Water, m3/month")) +
     geom_point(aes(y = QFM, colour = "Liquid, m3/month")) +
     geom_point(aes(y = WCT*10, colour = "WCT (100=10%)"))
#ggtitle(monthly[monthly$Well_N],)
g

#  geom_line(aes(y = CumOil, colour = "CumOil, 1000 m3"))

#export all production data to csv
write.table(monthly,file="prod-mon.csv", sep=";",row.names=F)
writeLines("Do not forget to replace dots with commas before opening in Excel!!!")

library("zoo")
rtotals_by_wells <- rollapply(
     data = by_wells,
     width = 10,
     FUN = sum,
     by = by_wells$QOM,
     fill = NA,
     partial = TRUE,
)

library("plyr")
monthly1 <- monthly
monthly1$CumOil <- unlist(tapply(monthly1$QOM1, monthly1$Well_N, cumsum))
unlist(monthly$CumOil)

monthly1$CumOil <- cbind(monthly1,
                         CumOil=c(lapply(split(monthly1, monthly1$Well_N),
                                         function(Date_m) cumsum(Date_m$QOM1)), recursive=T))

data.matrix(monthly$CumOil, rownames.force = NA)

monthly$CumOil <- as.numeric(as.character(monthly1$CumOil))

by_wells$CumOil1 <- vapply(by_wells$QOM1, FUN=cumsum, FUN.VALUE=c(2) )
by_wells$CumOil1 <- unlist(within(by_wells, CumOil1 <- cumsum(QOM1)))
by_wells$CumOil1 <- cumsum(by_wells$QOM1)
by_wells$CumOil1 <-
     transform(by_wells, CUMSUM = do.call(c, with(by_wells, tapply(CumOil1, rev(QOM1), cumsum))))

library(plyr)

## Prepare field data from wells data
qomnsd <- data.frame(aggregate(QOM ~ Date_m, data = NSD.sub, FUN = sum, na.rm = TRUE))
qwmnsd <- data.frame(aggregate(QWM ~ Date_m, data = NSD.sub, FUN = sum, na.rm = TRUE))
qomnsd <- qomnsd %>% mutate(CumOil=cumsum(QOM))
qomnsd$"Nafta, \'000 m3/mėn" <- qomnsd$QOM/1000
qomnsd$"Suminė gavyba, \'000 m3" <- qomnsd$CumOil/1000
qomnsd$Data <- qomnsd$Date_m

p1 <- ggplot(data="qomnsd", aes_string(x = Data)) +
     ggtitle("Nausodžio telkino gavybos istorija") +
     theme_bw() +
     geom_line(aes(y = "Nafta, \'000 m3/mėn", col = "brown")) +
     geom_line(aes(y = "Suminė gavyba, \'000 m3", col = "blue")) +
     scale_colour_tableau() +
     theme(legend.position = "bottom")

p1 <- ggplot(qomnsd, aes_string(x = "Data")) +
     theme_bw() +
     xlab("Metai") +
     ylab("Naftos gavyba/Suminė naftos gavyba") +
     geom_line(aes(y = CumOil/100000, col = "Suminė naftos gavyba, 100 tūkst. m3")) +
     geom_line(aes(y = QOM/1000, col = "Naftos gavyba, tūkst. m3/mėn")) +
     ggtitle("Nausodžio telkinio gavybos grafikas") +
     theme_few() +
     scale_colour_economist() +
     theme(legend.position = "bottom", legend.title = element_blank())
p1

#ms#access with linux
library(Hmisc)
mydata <- mdb.get('AccessFile.mdb', tables='NameOfAccessTable'')
