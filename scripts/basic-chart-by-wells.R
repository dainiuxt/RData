library("ggplot2")
ggplot(data=monthly,
       aes(x=Date_m, y=QOM, group=Well_N, color=Well_N)) +
       geom_line()
