require(soilphysics)
# vg <- read.table('clipboard', sep="\t", header=TRUE)
# pots <- as.data.frame(10^seq(-1,6,0.1))
# pots[43,] <- 1500.
# thetas <- as.data.frame(rep(NA,NROW(pots)))
# colnames(thetas) <- c("pot")
# thetas$pot <- pots[,1]
for (i in 1:NROW(vg)) {
  thetas[,as.character(vg$Texture[i])] <- 
    soilwater(pots, vg$theta_r[i], vg$theta_sat[i],
               vg$alpha_per_cm[i], vg$n[i])
}
# start plotting!
par(mfrow=c(2,1), mar=c(3.5,3.5,0.75,0.75), mgp=c(1.8,0.3,0), 
    cex.lab=1.25, cex.axis=1.2, font.lab=1, tcl=0.4)
palette(c("black", rev(rainbow(11, v=0.5, end=0.77))))
Values <- thetas[c(1,21,43),c(2,5,7,11,13,14)]
rownames(Values) <- c("theta_sat","theta_pwp","theta_fc")
bars <- Values
bars[1,] <- Values[1,] - Values[2,]
bars[2,] <- Values[2,] - Values[3,]
bars[3,] <- Values[3,]
bars <- as.matrix(bars)
rownames(bars) <- c("theta_sat","theta_pwp","theta_fc")
barplot(bars, col=c("navy","lightseagreen","chocolate3"), xlab="", yaxt="n", 
        ylab=expression(paste("Volumetric water content, ",theta[v]," (v/v)")),
        ylim=c(0,0.55), yaxs="i", density=c(12,NA,12), angle=c(45,NA,135),
        names.arg=c("\u00A0\nSand\n\u00A0","\u00A0\nLoam\n\u00A0","\u00A0\nSilt\nloam",
                    "\u00A0\nSandy\nclay","Clay\n\u00A0","\u00A0\nCompac-\nted clay"),
        mgp=c(1.8,1.3,0))
axis(2, mgp=c(1.8,0.2,0))
legend(-0.15,0.59, ncol=3, density=c(12,NA,12), angle=c(45,NA,135),
       legend=c("____",
                "____",
                "____"), 
       fill=c("navy","lightseagreen","chocolate3"),
       cex=2.4, bty="n", inset=0.01, x.intersp=0.5, text.col="transparent")
legend(0.6,0.56, ncol=3, 
       legend=c("Drainage\n(\u2265 \u221210kPa)",
                "Available\n(\u221210 to \u22121500kPa)",
                "Unavailable\n(\u2264 \u22121500kPa)"), 
       col=c("transparent","transparent","transparent"),
       cex=1, bty="n", inset=0.01, x.intersp=0.1)
box()
#
plot(c(0.1,1e6),c(0,0.48), 
     type="n", cex=0.6, log='x', ylim=c(0,0.499), yaxs="i", xaxt="n", 
     pch=1, col=1, 
     xlab=expression(paste("Matric potential, ",psi[m]," (kPa)")), 
     ylab=expression(paste("Volumetric water content, ",theta[v]," (v/v)")))
abline(v=c(10,1500), col="gray", lty=2)
text(c(10,1500),c(0.475,0.475), labels=c(expression(psi[FC]),expression(psi[PWP])),
     pos=c(2,4), cex=1.6, col="gray50", offset=0.15)
axis(1, at=(c(0.1,10,1000,100000)),
     labels=c('-0.1','-10','-1000','-100000'))
# for all textures replace vector with 1:NROW(vg)
selected <- c(1,4,6,10,12)
for (i in selected) {
  lines(thetas$pot, thetas[,i+1], type="b", cex=0.7, pch=i+12, col=i)
}
lines(thetas$pot, thetas[,14],
      type="l", cex=0.7, pch=i+12, col="grey50", lwd=3)
legend("bottomleft", bty="n", cex=1.2, seg.len=1.5, 
       legend=c("Sand","Loam","Silt loam","Sandy clay","Clay","Compacted clay"), 
       col=c(selected,"gray50"), pch=c(selected+12,NA), 
       lty=c(rep(NA,length(selected)),1), lwd=c(rep(1,length(selected)),3))
