require("ggplot2", quietly = TRUE)
require("magrittr", quietly = TRUE)
require("dplyr", quietly = TRUE)
require("gtable", quietly = TRUE)
require("grid", quietly = TRUE)
require("ggthemes", quietly = TRUE)
grid.newpage()

colour1 <- c("#CC9966", "#0000CC")
colour2 <- c("#336600", "#FFFFFF")

# Prepare data, Calculate Field production from wells production and cumulative
liziai <- LIZ.sub %>% group_by(Date_m) %>% summarize(QOM = sum(QOM))
liziai <- transform(liziai, QWM = LIZ.sub %>% group_by(Date_m) %>% summarize(QWM = sum(QWM)))
liziai$CUMOIL <- cumsum(liziai$QOM)

p1 <- ggplot(liziai, aes_string(x = "Date_m"), xlim = xlimits) +
  xlab("Metai") +
  ylab("Naftos, vandens gavyba") +
  theme_igray() +
  geom_line(aes(y = QOM, col = "Naftos gavyba, m3/mėn"), size = 1) +
  geom_line(aes(y = QWM.QWM, col = "Vandens gavyba, m3/mėn"), size = 1) +
  scale_colour_manual(values=colour1) +
  theme(legend.position = "bottom", legend.title = element_blank())

p2 <- ggplot(liziai, aes_string(x = "Date_m")) +
  xlab("Metai") +
  ylab("Suminė naftos gavyba") +
  geom_line(aes(y = CUMOIL, col = "Suminė naftos gavyba, m3"), size = 1) +
  scale_colour_manual(values=colour2) +
  theme_few() %+replace%
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))
# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                     pp$l, pp$b, pp$l)
# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
ia2 <- which(g2$layout$name == "ylab")
ga2 <- g2$grobs[[ia2]]
ga2$rot <- 90
g <- gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)

# draw it
leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
g$grobs[[which(g$layout$name == "guide-box")]] <-
  gtable:::cbind_gtable(leg1, leg2, "first")
grid.draw(g)

png("liziai-sumineGavyba.png", width = 11, height = 8, units = "in", res = 600)
grid.draw(g)
dev.off()

