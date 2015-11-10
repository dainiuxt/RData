p1 <- ggplot(monthly[monthly$Well_N == "NSD9",], aes(Date_m)) +
  geom_line(aes(y = QOM, colour = "Oil")) +
  geom_line(aes(y = QWM, colour = "Water")) +
  ggtitle("Production chart")
par("new" = TRUE)

p2 <- ggplot(monthly[monthly$Well_N == "NSD8",], aes(Date_m)) +
  geom_line(aes(y = QOM, colour = "Oil")) +
  geom_line(aes(y = QWM, colour = "Water"))
par("new" = TRUE)

p3 <- ggplot(monthly[monthly$Well_N == "NSD10",], aes(Date_m)) +
  geom_line(aes(y = QOM, colour = "Oil")) +
  geom_line(aes(y = QWM, colour = "Water"))
par("new" = TRUE)
p1
p2
p3
