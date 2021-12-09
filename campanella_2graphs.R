par(mar = c(4,6,1,1), mgp = c(1.6,0.3,0), tcl = 0.35, font.lab = 2, 
    lwd = 2, lend = "square", ljoin = "mitre")
layout_matrix <- cbind(1,1,2)
layout(layout_matrix)
# layout.show()
with(campanella, 
     plot(seq(1,9,1), 
          Percent.black, 
          type="o",
          cex = 1.4, 
          col = "blue", 
          ylim=c(0,80), 
          xaxt="n", 
          xlab="", 
          ylab=paste0("Percent of population of blocks containing at\n",
                      "least one sample in given lead content range"),
          cex.lab = 1.8, cex.axis = 1.4)
     )
grid(lwd = 1)
with(campanella, 
     points(seq(1,9,1), 
          Percent.black, 
          type="o",
          cex = 1.8, 
          col = "blue",
          pch = 21, bg = "white")
)
axis(1, at = seq(1,9,2), 
     labels = c("<20","50-100","200-400","1000-5000",">20000"), 
     mgp = c(3,0.2,0), cex.axis = 1.4)
axis(1, at = seq(2, 8 ,2), labels = campanella$Range[c(2,4,6,8)], 
     mgp = c(3,1.3,0), cex.axis = 1.4)
mtext("Soil Pb content range (mg/kg)", side = 1, line = 2.8, cex=1.2, font=2)
with(campanella, 
     points(seq(1,9,1), 
          Percent.white, 
          type="o", 
          pch = 19,
          cex = 1.8,
          col = "tan3")
)
with(campanella, 
     points(seq(1,9,1), 
            Percent.Hispanic, 
            type="o", 
            pch = 22,
            cex = 1.6,
            col = "green4", bg = "white")
)
with(campanella, 
     points(seq(1,9,1), 
            Percent.Asian, 
            type="o", 
            pch = 15,
            cex = 1.6,
            col = "red3")
)
text(seq(1,9,1), cex=1.4, 
     campanella$Percent.black, 
     labels = campanella$Percent.black,
     pos = 3, col = "blue")
text(seq(1,9,1), cex=1.4, 
     campanella$Percent.white, 
     labels = campanella$Percent.white,
     pos = 1, col = "tan3")
text(seq(1,9,1), cex=1.4, 
     campanella$Percent.Hispanic, 
     labels = campanella$Percent.Hispanic,
     pos = 3, col = "green4")
text(seq(1,9,1), cex=1.4, 
     campanella$Percent.Asian, 
     labels = campanella$Percent.Asian,
     pos = c(rep(1,8), 4), col = "red3", offset = 0.3)
legend(1.9, 81.5, legend = c("Black","White","Hispanic","Asian"),
       pch = c(1,19,0,15), col = c("blue","tan3","green4","red3"),
       ncol = 2, inset = 0.02, bty = "o", pt.cex = 2, cex = 1.8, 
       bg = "white", box.col = "grey", box.lwd = 3)
mtext("(a)", side = 3, line = -1.8, adj = 0.02, cex = 1.4, font = 2)
#
# plot layout frame 2
plot(seq(1,9), campanella$Cum_lt_5, 
     type="b",
     cex = 1.6, 
     col = "darkcyan",
     pch = 17, 
     ylim=c(0,100), 
     xaxt="n", 
     xlab="", 
     ylab=paste0("Cumulative proportion of < 5 years old children\n",
                 "who are exposed to lead above each range"),
     cex.lab = 1.8, cex.axis = 1.4)
grid(lwd = 1)
points(seq(1,9), campanella$Cum_lt_5, 
     type="b",
     cex = 1.6, 
     col = "darkcyan",
     pch = 17)
axis(1, at = seq(1,9,2), labels = c("20","100","400","5000",">20001"), 
     mgp = c(3,0.15,0), cex.axis = 1.4)
axis(1, at = seq(2, 8 ,2), labels = c("50","200","1000","20000"), 
     mgp = c(3,1.3,0), cex.axis = 1.4)
mtext("Upper limit of soil Pb range (mg/kg)", 
      side = 1, line = 2.8, cex=1.2, font=2, adj=1)
mtext("(b)", side = 3, line = -1.8, adj = 0.98, cex = 1.4, font = 2)
