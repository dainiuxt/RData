library(ggthemes)
library(ggplot2)
library(dplyr)
library(data.table)

# Prepare data
# Calculate Field production from wells production
liziai <- LIZ.sub %>% group_by(Date_m) %>% summarize(QOM = sum(QOM), QWM = sum(QWM))

# Field production cumulative
liziai$CUMOIL <- cumsum(liziai$QOM)

# Chart drawing
p1 <- ggplot(liziai, aes_string(x = "Date_m")) +
     theme_igray() +
     xlab("Metai") +
     ylab("Naftos gavyba/Vandens gavyba, m3/mėn") +
     geom_line(aes(y = CUMOIL, col = "Suminė naftos gavyba, tūkst. m3"), size = 1) +
     geom_line(aes(y = QOM, col = "Naftos gavyba, m3/mėn")) +
     geom_line(aes(y = QWM, col = "Vandens gavyba, m3/mėn")) +
#     ggtitle("liziai") +
     scale_color_wsj() +
     theme(legend.position = "bottom", legend.title = element_blank())
p1
png("liziai-sumineGavyba.png", width = 11, height = 8, units = "in", res = 600)
p1
dev.off()
