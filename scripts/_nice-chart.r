library(ggthemes)
library(ggplot2)
p1 <- ggplot(monthly[monthly$Well_N == "VEZ17",], aes_string(x = "Date_m")) +
     theme_igray() +
     xlab("Year") +
     ylab("Monhly/Cumulative Oil Production") +
     geom_line(aes(y = CumOil/100000, col = "CumOil, 100M m3"), size = 1) +
     geom_line(aes(y = QOM/1000, col = "Monthly Oil Production, M m3")) +
     geom_line(aes(y = QWM/1000, col = "Monthly Water Production, M m3")) +
     #      ggtitle("Nausodžio telkinio gavybos grafikas") +
     #      scale_color_wsj() +
     theme(legend.position = "bottom", legend.title = element_blank())
p1
