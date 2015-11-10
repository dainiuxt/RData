#loading required libraries
library(data.table)
library(ggplot2)
library(grid)
library(magrittr)
library(dplyr)
library(gtable)
library(ggthemes)

# Create data subset and infill #NA data with zero values
Nausodis <- NSD.sub
Nausodis <- setkey(setDT(Nausodis), Date_m, Well_N)[
  CJ(Date_m=seq(min(Date_m), max(Date_m), by='1 month'),
     Well_N=unique(Well_N))][is.na(QOM), QOM:=0][is.na(QWM), QWM:=0][is.na(QFM), QFM:=0][is.na(QIW), QIW:=0][order(Well_N)]

#plots
grid.newpage()
################################################################################
#                         NSD1                                                 #
################################################################################
p1 <- ggplot(data=Nausodis %>% filter(Well_N == "NSD1"),
             aes_string(x = "Date_m"), xlim = xlimits) +
  ggtitle("NSD1") +
  theme_igray() +
  #      discrete_scale("colour", "mycolours" , mycolours) +
  geom_line(aes(y = QFM, title = "Liquid, m3/month", colour = "green")) +
  geom_line(aes(y = QOM, col = "brown", title = "Oil, m3/month")) +
  geom_line(aes(y = QWM, col = "bloe", title = "Water, m3/month")) +
  geom_point(aes(y = WCT*10, col = "red", title = "WCT (100=10%)")) +
  geom_line(aes(y = WCT*10, col = "red", title = "WCT (100=10%)")) +
  geom_line(aes(y = QIW, col = "black", title = "Water injection, m3/month")) +
  #      scale_colour_tableau() +
  theme(legend.position = "bottom")

p2 <- ggplot(data=Nausodis %>% filter(Well_N == "NSD1"), aes_string(x = "Date_m")) +
  geom_line(aes(y = CumOil/1000, col = "black", title = "CumOil, \'000 m3")) +
  geom_line(aes(y = CumInj/1000, col = "black", title = "CumInj, \'000 m3")) +
  theme_few() %+replace%
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
gnsd1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
gnsd1 <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
gnsd1 <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

ia2 <- which(g2$layout$name == "ylab")
ga2 <- g2$grobs[[ia2]]
ga2$rot <- 90
gnsd1 <- gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
gnsd1 <- gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)

# draw it
pnsd1 <- grid.draw(gnsd1)

grid.newpage()
################################################################################
#                         NSD3R                                                #
################################################################################
p1 <- ggplot(data=Nausodis %>% filter(Well_N == "NSD3R"),
             aes_string(x = "Date_m"), xlim = xlimits) +
  ggtitle("NSD3R") +
  theme_igray() +
  #      discrete_scale("colour", "mycolours" , mycolours) +
  geom_line(aes(y = QFM, title = "Liquid, m3/month", colour = "green")) +
  geom_line(aes(y = QOM, col = "brown", title = "Oil, m3/month")) +
  geom_line(aes(y = QWM, col = "bloe", title = "Water, m3/month")) +
  geom_point(aes(y = WCT*10, col = "red", title = "WCT (100=10%)")) +
  geom_line(aes(y = WCT*10, col = "red", title = "WCT (100=10%)")) +
  geom_line(aes(y = QIW, col = "black", title = "Water injection, m3/month")) +
  #      scale_colour_tableau() +
  theme(legend.position = "bottom")

p2 <- ggplot(data=Nausodis %>% filter(Well_N == "NSD3R"), aes_string(x = "Date_m")) +
  geom_line(aes(y = CumOil/1000, col = "black", title = "CumOil, \'000 m3")) +
  geom_line(aes(y = CumInj/1000, col = "black", title = "CumInj, \'000 m3")) +
  theme_few() %+replace%
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
gnsd3r <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                         pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
gnsd3r <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
gnsd3r <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

ia2 <- which(g2$layout$name == "ylab")
ga2 <- g2$grobs[[ia2]]
ga2$rot <- 90
gnsd3r <- gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
gnsd3r <- gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)

# draw it
pnsd3r <- grid.draw(gnsd3r)

grid.newpage()
################################################################################
#                         NSD10                                                #
################################################################################
p1 <- ggplot(data=Nausodis %>% filter(Well_N == "NSD10"),
             aes_string(x = "Date_m"), xlim = xlimits) +
  ggtitle("NSD10") +
  theme_igray() +
  #      discrete_scale("colour", "mycolours" , mycolours) +
  geom_line(aes(y = QFM, title = "Liquid, m3/month", colour = "green")) +
  geom_line(aes(y = QOM, col = "brown", title = "Oil, m3/month")) +
  geom_line(aes(y = QWM, col = "bloe", title = "Water, m3/month")) +
  geom_point(aes(y = WCT*10, col = "red", title = "WCT (100=10%)")) +
  geom_line(aes(y = WCT*10, col = "red", title = "WCT (100=10%)")) +
  geom_line(aes(y = QIW, col = "black", title = "Water injection, m3/month")) +
  #      scale_colour_tableau() +
  theme(legend.position = "bottom")

p2 <- ggplot(data=Nausodis %>% filter(Well_N == "NSD10"), aes_string(x = "Date_m")) +
  geom_line(aes(y = CumOil/1000, col = "black", title = "CumOil, \'000 m3")) +
  geom_line(aes(y = CumInj/1000, col = "black", title = "CumInj, \'000 m3")) +
  theme_few() %+replace%
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
gnsd10 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                           pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
gnsd10 <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
gnsd10 <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

ia2 <- which(g2$layout$name == "ylab")
ga2 <- g2$grobs[[ia2]]
ga2$rot <- 90
gnsd10 <- gtable_add_cols(g, g2$widths[g2$layout[ia2, ]$l], length(g$widths) - 1)
gnsd10 <- gtable_add_grob(g, ga2, pp$t, length(g$widths) - 1, pp$b)

# draw it
pnsd10 <- grid.draw(gnsd10)

# printing
pdf("Nausodis.pdf", width = 24, height = 34)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3,1)))
vplayout <- function(x,y)
viewport(layout.pos.row = x, layout.pos.col = y)

print(pnsd1, vp = vplayout(1,1))
print(pnsd3r, vp = vplayout(2,1))
print(pnsd10, vp = vplayout(3,1))

dev.off()
