library("lattice")
latplot <- xyplot(QOM ~ Date_m | factor(Well_N), data = VEZ.sub[VEZ.sub$Well_N=="VEZ19",])
latplot <- xyplot(WCT ~ CumOil | factor(Well_N), data = LIZ.sub)
latplot

p1 <- xyplot(WCT ~ CumOil | factor(Well_N), data = LIZ.sub)
p1
