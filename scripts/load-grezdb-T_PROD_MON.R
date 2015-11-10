dtu_well_chart <- function(data, wellname, ...) {
     require("ggplot2", quietly=TRUE)
     require("magrittr", quietly=TRUE)
     require("dplyr", quietly=TRUE)
ggplot(data=data %>% filter(Well_N == wellname), aes_string(x = "Date_m")) +
     geom_line(aes(y = QFM, colour = "Liquid, m3/month", col = "darkgreen")) +
     geom_line(aes(y = QOM, colour = "Oil, m3/month"), col="brown") +
     geom_line(aes(y = QWM, colour = "Water, m3/month", col = "blue")) +
     geom_line(aes(y = CumOil/100, colour = "CumOil, m3 (1.000=100.000)")) +
#      geom_line(aes(y = WCT*10, colour = "WCT (100=10%)", col = "darkred")) +
#      geom_point(aes(y = QOM, colour = "Oil, m3/month", col="brown")) +
#      geom_point(aes(y = QWM, colour = "Water, m3/month", col = "blue")) +
#      geom_point(aes(y = QFM, colour = "Liquid, m3/month", col = "khaki")) +
     geom_point(aes(y = WCT*10, colour = "WCT (100=10%)", col = "darkred")) +
     ggtitle(wellname) +
     theme_bw()
#      theme(legend.position = "bottom")
}
