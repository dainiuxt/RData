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
#                       Well injection for overview chart                      #
#                                                                              #
################################################################################

dtuwellinjchartow <- function(data, wellname, ...) {
  colour1 <- c("blue", "darkgrey", "brown")
  colour2 <- c("#000000", "#FFFFFF")
  grid.newpage()
  #plots
  p1 <- ggplot(data=data %>% filter(Well_N == wellname),
               aes_string(x = "Date_m"), xlim = xlimits) +
    xlab("") +
    ylab("Injekcija, m3/men") +
    ggtitle(wellname) +
    theme_igray() +
    scale_colour_manual(values=colour1) +
    #      discrete_scale("colour", "mycolours" , mycolours) +
    geom_line(aes(y = QIW, col = "Water injection, m3/month")) +
    #     scale_colour_tableau() +
    theme(legend.position = "none")

  p2 <- ggplot(data=data %>% filter(Well_N == wellname), aes_string(x = "Date_m")) +
    geom_line(aes(y = CumInj/1000, col = "CumInj, \'000 m3")) +
    ylab("Injekcija viso, tukst. m3") +
    scale_colour_manual(values=colour2) +
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