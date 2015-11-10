#drop factor levels in subset
# LIZ.sub$Well_N <- factor(LIZ.sub$Well_N)
library("ggplot2")
library("gridExtra")
LIZ.sub$Index <- ave( 1:nrow(LIZ.sub), LIZ.sub$Well_N, factor( LIZ.sub$Well_N), FUN=seq_along )
# pl1 <- ggplot(data=LIZ.sub,
#      aes(x=Date_m, y=QFM, group=Well_N, color=Well_N)) +
#      geom_line()
# pl2 <- ggplot(data=LIZ.sub,
#               aes(x=CumOil, y=QFM, group=Well_N, color=Well_N)) +
#      geom_line()
plot1 <- qplot(QOM, WCT, data = LIZ.sub, colour = Well_N, size = I(1.5)) + theme_bw() +theme(legend.position = 'bottom')
plot2 <- qplot(QFM, WCT, data = LIZ.sub, colour = Well_N, size = I(1.5)) + theme_bw() + theme(legend.position = 'bottom')
plot3 <- qplot(CumOil, WCT, data = LIZ.sub, colour = Well_N, size = I(1.5), log = "x") + theme_gray() + theme(legend.position = 'right')

# qplot(CumOil, WCT, data = LIZ.sub, colour = Well_N, size = I(2)) + theme_bw()

# plot3 <- plot(LIZ.sub$QFM, colour = LIZ.sub$Well_N)
# pdf("LIZ-x-vs-QFM.pdf")
grid.arrange(plot1, plot2, plot3, nrow = 3, ncol = 1)
png("LIZ-x-vs-QFM.png", width = 8, height = 11, units = "in", res = 600)
grid.arrange(plot1, plot2, plot3, nrow = 3, ncol = 1)
dev.off()
