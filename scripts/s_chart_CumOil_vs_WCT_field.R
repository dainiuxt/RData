#Screen output
factorchart <- plot(GRK.sub$CumOil, GRK.sub$WCT, type="n", ylim=c(0,100))
grid(NULL, NULL)
points(GRK.sub$CumOil, GRK.sub$WCT, pch=19, col=GRK.sub$Well_N)
legend("bottomright", legend = levels(GRK.sub$Well_N), pch = 19, col=1:length(GRK.sub$Well_N), title = "Well")
factorchart

#high-res PNG output
dev.copy(png,
         filename = "GRK-CumOil-vs-WCT.png",
         res = 600,
         units = "in",
         height = 8,
         width = 11)
dev.off()

# png("GRK-CumOil-vs-WCT.png", width = 11, height = 8, units = "in", res = 600)
# plot(GRK.sub$CumOil, GRK.sub$WCT, type="n")
# points(GRK.sub$CumOil, GRK.sub$WCT, pch=19, col=GRK.sub$Well_N)
# legend("bottomright", legend = levels(GRK.sub$Well_N), pch = 19, col=1:length(GRK.sub$Well_N), title = "Well")
# dev.off()
