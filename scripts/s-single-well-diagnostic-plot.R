
#' Function for dataloading
#'
#' @param data R dataset name
#' @param wellname Name of the well you would like to plot.
#' @keywords wells data chart
#' @export
#' @examples
#' wellchart(monthly, "NSD10")

swchrt <- function(data, wellname, ...) {
     require("ggplot2", quietly = TRUE)
     require("magrittr", quietly = TRUE)
     require("dplyr", quietly = TRUE)
     require("gtable", quietly = TRUE)
     require("grid", quietly = TRUE)
     require("ggthemes", quietly = TRUE)
     #plot
     ggplot(data=data %>% filter(Well_N == wellname),
            aes_string(x = "WCT", y = "CumOil"), log = "y") +
     ggtitle(wellname) +
     xlim(0, 100) +
     theme_bw() +
     #      discrete_scale("colour", "mycolours" , mycolours) +
     geom_point(shape = 1)
#      geom_smooth(method = lm) +
#      #      scale_colour_tableau() +
#      legend("topright", bty="n",
#             legend=paste("R2 is",
#                          format(summary(data)$adj.r.squared, digits=4)))
}
