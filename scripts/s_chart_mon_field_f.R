dtu_field_chart <- function(data, ...) {
require("ggplot2", quietly = TRUE)
require("magrittr", quietly = TRUE)
require("dplyr", quietly = TRUE)
require("gtable", quietly = TRUE)
require("grid", quietly = TRUE)
require("ggthemes", quietly = TRUE)
grid.newpage()

#plots
p1 <- ggplot(data=data, aes_string(x = "Date_m")) +
     theme_bw() +
     ylab("Naftos gavyba, m3/mėn.") +
     xlab("Metai") +
     geom_line(aes(y = QOM, col = "brown", title = "Naftos gavyba, m3/mėn.")) +
     scale_colour_tableau()


# p1 <- barplot(qomnsd$QOM, names.arg = qomnsd$Date_m, xlab = "Data", ylab = "Naftos gavyba, m3/mėn.")

p2 <- ggplot(data=data, aes_string(x = "Date_m")) +
     geom_line(aes(y = CumOil/1000, col = "blue",  title = "Suminė naftos gavyba, \'000 m3")) +
     ylab("Suminė naftos gavyba, \'000 m3") +
     theme(legend.position = "bottom") +
     theme_few() %+replace%
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
grid.draw(g)
}
