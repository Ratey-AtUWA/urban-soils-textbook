require(Rmisc)
par(mar=c(4,4,1,1), mgp=c(2,0.25,0), mfrow=c(1,1), font.lab=2, 
    lend="square", ljoin="mitre", las=1, tcl = 0.5)
palette(c("black","red3","darkgreen","blue2","#DDEE99",
          "#FFEFA3","#e0e0e0","white","slategray1","thistle",
          "purple"))
# 
# change data object, variable & factor names to suit your data 
ci0 <- group.CI(Ca.log~Type, data=sv18)
boxplot(sv18$Ca.log~sv18$Type, varwidth=T, col=c(6, 7, 9),
        ylab=expression(bold(paste(log[10],"(Ca, mg/kg)"))),
        cex.lab=1.5, cex.axis=1.25, xlab = "",
        xlim = c(-0.5,3.4)) # , log="y", notch=T, xlab="Sampling Zone"
mtext("Sample Type", 1, 2, font = 2, cex = 1.5)
#
# use arrow function to make background for [optional] error bars
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,2], 
       col=8, angle=90, length=0.1, lwd=6) # optional
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,4], 
       col=8, angle=90, length=0.1, lwd=6) # optional
#
# draw lines to join points first, so the points overplot lines
lines(seq(1,NROW(ci0)), ci0[,3], col=8, lwd=3, 
      type="c") # optional
lines(seq(1,NROW(ci0)), ci0[,3], col=1, lwd=1, 
      type="c", lty=3) # optional
#
# use arrow function to make actual [optional] error bars
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,2], 
       col=c(2,1,1,3,3,9), angle=90, length=0.1, lwd=2)
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,4], 
       col=c(2,1,1,3,3,9), angle=90, length=0.1, lwd=2)
#
# draw the points with white background for contrast
points(seq(1,NROW(ci0)), ci0[,3], col=8, pch=1, lwd=3, cex=1.6)
points(seq(1,NROW(ci0)), ci0[,3], col=c(2,1,1,3,3,9), pch=1, lwd=1, 
       cex=1.2)
#
# add a legend
# legend("topright", legend=c("Mean \u00B1 95% CI","Potential outliers"), 
#        pch=c(16,1), cex=1., pt.cex=c(1.4,1.2), col=c(9,1), lwd=c(1,NA), 
#        lty=c(3,NA), bty="n", inset=0.01, y.intersp=1.5)
#
# delete temporary objects
rm(ci0)
tapply(sv18$Ca.log, sv18$Type, mean)
# [end code]