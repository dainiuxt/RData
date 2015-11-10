library()
#drop factor levels in subset
# GRK.sub$Well_N <- factor(GRK.sub$Well_N)
c25 <- c("dodgerblue2","#E31A1C", # red
         "green4",
         "#6A3D9A", # purple
         "#FF7F00", # orange
         "black","gold1",
         "skyblue2","#FB9A99", # lt pink
         "palegreen2",
         "#CAB2D6", # lt purple
         "#FDBF6F", # lt orange
         "gray70", "khaki2",
         "maroon","orchid1","deeppink1","blue1","steelblue4",
         "darkturquoise","green1","yellow4","yellow3",
         "darkorange4","brown")

GRK.col <- with(GRK.sub,
                data.frame(Well_N = levels(Well_N),
                           color = "c25"))

data.frame(subset(GRK.sub, select = c(Well_N)),
           matchRetVal = match(GRK.sub$Well_N))

colors = c25
png("GRK-CumOil-vs-WCT.png", width = 11, height = 8, units = "in", res = 600)
plot(GRK.sub$CumOil, GRK.sub$WCT, type="n")
points(GRK.sub$CumOil, GRK.sub$WCT, pch=19, col=GRK.sub$Well_N)
legend("bottomright", legend = levels(GRK.sub$Well_N), pch = 19, col=1:length(GRK.sub$Well_N), title = "Well")
dev.off()
