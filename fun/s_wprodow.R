#Load required libraries
require("ggplot2", quietly = TRUE)
require("magrittr", quietly = TRUE)
require("dplyr", quietly = TRUE)
require("gtable", quietly = TRUE)
require("grid", quietly = TRUE)
require("ggthemes", quietly = TRUE)
require("gridBase", quietly = TRUE)
require("scales", quietly = TRUE)

################################################################################
#                                                                              #
#                       Well production chart for overview                     #
#                                                                              #
################################################################################

dtuwellprodchartow <- function(data, wellname, ...) {
colour1 <- c("darkgrey", "brown", "blue")
# colour2 <- c("#000000", "#FFFFFF")
colour2 <- c("red", "#FFFFFF")
ypos <- max((data=data %>% filter(Well_N == wellname))$QFD)*.9
grid.newpage()
#plots
p1 <- ggplot(data=data %>% filter(Well_N == wellname),
             aes_string(x = "Date_m"), xlim = xlimits) +
  xlab("") +
  ylab("Debitas, m3/para") +
  # ggtitle(wellname) +
  theme_bw() +
  #      discrete_scale("colour", "mycolours" , mycolours) +
  geom_line(aes(y = QFD, col = "Fluidas, m3/d")) +
  geom_line(aes(y = QOD, col = "Nafta, m3/d")) +
  geom_point(aes(y = QOD, col = "Nafta, m3/d")) +
  geom_line(aes(y = QWD, col = "Vanduo, m3/d")) +
  #     scale_colour_tableau() +
  scale_colour_manual(values=colour1) +
  scale_x_date(limits = c(minlab, maxlab), breaks = date_breaks("year"),
               labels = date_format("%Y")) +
  annotate("text", x=as.Date(minlab, "%Y-%m-%d"), y = ypos, label = wellname) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "darkgrey", size = 0),
        panel.grid.minor = element_line(colour = "darkgrey", linetype = "dotted"))
#   theme(plot.title)
#   geom_text(aes(data = Nausodis, .1, .1, label = "NSD3R"))

p2 <- ggplot(data=data %>% filter(Well_N == wellname), aes_string(x = "Date_m")) +
  ylab("Viso naftos, tukst. m3") +
#   ylab("WCT, %") +
#   geom_line(aes(y = WCT, col = "WCT, %")) +
  geom_line(aes(y = CumOil/1000, col = "Viso nafta, \'000 m3")) +
  scale_colour_manual(values=colour2) +
  scale_x_date(limits = c(minlab, maxlab)) +
  theme_few() %+replace%
  theme(legend.position = "none", legend.title = element_blank()) +
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

# extract legend
# leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
# leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
#
# g$grobs[[which(g$layout$name == "guide-box")]] <-
#   gtable:::cbind_gtable(leg1, leg2, "first")

# draw it
grid.draw(g)
}
