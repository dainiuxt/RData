png("v17.png")
p1 <- ggplot(data=monthly %>% filter(Well_N == "VEZ17"),
             aes_string(x = "Date_m"), xlim = xlimits) +
     ggtitle("VEZ17") +
     theme_igray() +
     geom_line(aes(y = QFM, colour = "Liquid, m3/month")) +
     geom_line(aes(y = QOM, colour = "Oil, m3/month")) +
     geom_line(aes(y = QWM, colour = "Water, m3/month")) +
     scale_colour_tableau() +
     theme(legend.position = "bottom")
p1
dev.off()
