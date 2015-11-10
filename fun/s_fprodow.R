#Load required libraries
require("ggplot2", quietly = TRUE)
require("magrittr", quietly = TRUE)
require("dplyr", quietly = TRUE)
require("gtable", quietly = TRUE)
require("grid", quietly = TRUE)
require("ggthemes", quietly = TRUE)
require("gridBase", quietly = TRUE)

################################################################################
#                                                                              #
#                       field production chart for overview charting           #
#                                                                              #
################################################################################

dtufieldprodchartow <- function(data, ...) {
  require("ggplot2", quietly = TRUE)
  require("magrittr", quietly = TRUE)
  require("dplyr", quietly = TRUE)
  require("gtable", quietly = TRUE)
  require("grid", quietly = TRUE)
  require("ggthemes", quietly = TRUE)
  colour1 <- c("brown", "blue", "brown")
  colour2 <- c("#000000", "#FFFFFF")
  ypos <- max((data=data)$QWMF)*.9

  grid.newpage()
  #plots
  p1 <- ggplot(data=data, aes_string(x = "Date_m")) +
    theme_bw() +
    ylab("Gavyba, m3/men.") +
    xlab("") +
    scale_x_date(limits = c(minlab, maxlab), breaks = date_breaks("year"), labels = date_format("%Y")) +
    theme(legend.position = "none",
          panel.grid.major = element_line(colour = "darkgrey", size = 0),
          panel.grid.minor = element_line(colour = "darkgrey", linetype = "dotted")) +
    geom_line(aes(y = QOMF, col = "Naftos gavyba, m3/mėn.")) +
    geom_line(aes(y = QWMF, col = "Vandens gavyba, m3/mėn.")) +
    annotate("text", x=as.Date(minlab, "%Y-%m-%d"), y = ypos, label = "Telkinys") +
    scale_colour_manual(values=colour1)

  # p1 <- barplot(qomnsd$QOM, names.arg = qomnsd$Date_m, xlab = "Data", ylab = "Naftos gavyba, m3/mėn.")

  p2 <- ggplot(data=data, aes_string(x = "Date_m")) +
    geom_line(aes(y = CumOilF/1000, col = "Viso naftos tukst. m3")) +
    ylab("Viso naftos tukst. m3") +
    theme(legend.position = "none") +
    scale_colour_manual(values=colour2) +
    scale_x_date(limits = c(minlab, maxlab)) +
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
