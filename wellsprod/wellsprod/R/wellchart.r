#' Function for dataloading
#'
#' @param data R dataset name
#' @param wellname Name of the well you would like to plot.
#' @keywords wells data chart
#' @export
#' @examples
#' wellchart(monthly, "NSD10")

wellchart <- function(data, wellname, ...) {
    require("ggplot2", quietly = TRUE)
    require("magrittr", quietly = TRUE)
    require("dplyr", quietly = TRUE)
    require("gtable", quietly = TRUE)
    require("grid", quietly = TRUE)
    require("ggthemes", quietly = TRUE)
    grid.newpage()

#     xlimits <- c("1994-01-01 00:00:01", "2014-12-31 23:59:59")  # example data
#     xlimits <- as.numeric(xlimits)  # new step to convert data
#     mycolours <- c("red", "brown", "darkgreen", "black", "blue", "darkred", "grey")

    #plots
    p1 <- ggplot(data=data %>% filter(Well_N == wellname),
                 aes_string(x = "Date_m"), xlim = xlimits) +
        ggtitle(wellname) +
        xlab("Year") +
        ylab("Rates, m3/month; WCT (100=10%)") +
        theme_igray() +
        #      discrete_scale("colour", "mycolours" , mycolours) +
        geom_line(aes(y = QFM, colour = "Liquid, m3/month"), size=1.25) +
        geom_line(aes(y = QOM, colour = "Oil, m3/month"), size=1.25) +
        geom_line(aes(y = QWM, colour = "Water, m3/month"), size=1.25) +
        geom_point(aes(y = WCT*10, colour = "WCT (100=10%)")) +
        geom_line(aes(y = WCT*10, colour = "WCT (100=10%)")) +
#         geom_line(aes(y = QIW, colour = "Water injection, m3/month"), size=1.25) +
        scale_colour_manual(values = c("#339900", "#993300", "#0066cc", "#ff0000", "black")) +
        theme(legend.position = "bottom")

    p2 <- ggplot(data=data %>% filter(Well_N == wellname), aes_string(x = "Date_m")) +
        geom_line(aes(y = CumOil/1000, colour = "CumOil, \'000 m3"), size=1.25) +
#         geom_line(aes(y = CumInj/1000, col = "black", colour = "CumInj, \'000 m3"), size=1.25) +
        scale_colour_manual(values = c("black", "blue")) +
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
