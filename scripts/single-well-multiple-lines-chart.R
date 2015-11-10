#pdf(file = "gavyba-%03d.pdf", onefile = FALSE, height = 600, width = 800)
ggtitle("Chart title")
ggplot(monthly[monthly$Well_N == "ABL7",], aes(Date_m)) +
  geom_line(aes(y = QOM, colour = "Oil")) +
  geom_line(aes(y = QWM, colour = "Water"))
ggplot(monthly[monthly$Well_N == "ABL2",], aes(Date_m)) +
  geom_line(aes(y = QOM, colour = "Oil")) +
  geom_line(aes(y = QWM, colour = "Water"))
#dev.off()
