require(soilphysics)
par(mfrow=c(2,1), mar=c(3.5,3.5,0.75,0.75), mgp=c(1.8,0.3,0), 
    cex.lab=1.25, cex.axis=1., font.lab=1, tcl=0.4)
palette(c("black", rev(rainbow(11, v=0.5, end=0.77))))
#
vg_alpha$theta_r <- rep(0.01,13)
vg_alpha$theta_sat <- rep(0.48,13)
vg_alpha$n <- rep(1.8,13)
vg_alpha$alpha_per_cm <- signif(10^seq(log10(0.005),log10(0.2),length.out = 13),3)
vg_alpha$Texture <- paste0("\u03B1 ",vg_alpha$alpha_per_cm)
#
thetas <- as.data.frame(rep(NA,NROW(pots)))
colnames(thetas) <- c("pot")
thetas$pot <- pots[,1]
for (i in 1:13) {
  thetas[,as.character(vg_alpha$Texture[i])] <- 
    soilwater(pots, vg_alpha$theta_r[i], vg_alpha$theta_sat[i],
              vg_alpha$alpha_per_cm[i], vg_alpha$n[i])
}
plot(c(0.1,1e6),c(0,0.48), 
     type="n", cex=0.6, log='x', ylim=c(0,0.59), yaxs="i", xaxt="n", 
     pch=1, col=1, 
     xlab=expression(paste("Matric potential, ",psi[m]," (kPa)")), 
     ylab=expression(paste("Volumetric water content, ",
                           theta[v]," (v/v)")))
abline(v=c(10,1500), col="gray", lty=2)
text(c(10,1500),c(0.525,0.525), 
     labels=c(expression(psi[FC]),expression(psi[PWP])),
     pos=c(3,3), cex=1.6, col="gray50", offset=0.25, srt=90)
axis(1, at=(c(0.1,10,1000,100000)),
     labels=c('-0.1','-10','-1000','-100000'))
# for all textures replace vector with 1:NROW(vg)
selected <- seq(1,13)
for (i in selected) {
  lines(thetas$pot, thetas[,i+1], type="b", cex=0.7, pch=i+12, col=i)
}
# lines(thetas$pot, thetas[,14],
#       type="l", cex=0.7, pch=i+12, col="grey50", lwd=3)
text(3e4,0.4, pos=2, labels=paste("n = ",vg_n$n[1],"/cm"))
legend("topright", bty="o", cex=1., seg.len=1.2, 
       y.intersp=0.8, legend=vg_alpha$Texture, 
       col=c(selected,"gray50"), pch=c(selected+12,NA), 
       x.intersp=0.5, box.col="transparent", inset=0.01,
       lty=c(rep(NA,length(selected)),1), 
       lwd=c(rep(1,length(selected)),3))
# __________________________________________________
#
vg_n$theta_r <- rep(0.01,13)
vg_n$theta_sat <- rep(0.48,13)
vg_n$alpha_per_cm <- rep(1.1,13)
vg_n$n <- signif(10^seq(log10(1.1),log10(3),length.out = 13),3)
vg_n$Texture <- paste0("n ",vg_n$n)
thetas <- as.data.frame(rep(NA,NROW(pots)))
colnames(thetas) <- c("pot")
thetas$pot <- pots[,1]
for (i in 1:NROW(vg_n)) {
  thetas[,as.character(vg_n$Texture[i])] <- 
    soilwater(pots, vg_n$theta_r[i], vg_n$theta_sat[i],
              vg_n$alpha_per_cm[i], vg_n$n[i])
}
plot(c(0.1,1e6),c(0,0.48), 
     type="n", cex=0.6, log='x', ylim=c(0,0.59), yaxs="i", xaxt="n", 
     pch=1, col=1, 
     xlab=expression(paste("Matric potential, ",psi[m]," (kPa)")), 
     ylab=expression(paste("Volumetric water content, ",
                           theta[v]," (v/v)")))
abline(v=c(10,1500), col="gray", lty=2)
text(c(10,1500),c(0.525,0.525), 
     labels=c(expression(psi[FC]),expression(psi[PWP])),
     pos=c(3,3), cex=1.6, col="gray50", offset=0.25, srt=90)
axis(1, at=(c(0.1,10,1000,100000)),
     labels=c('-0.1','-10','-1000','-100000'))
# for all textures replace vector with 1:NROW(vg)
selected <- seq(1,13)
for (i in selected) {
  lines(thetas$pot, thetas[,i+1], type="b", cex=0.7, pch=i+12, col=i)
}
# lines(thetas$pot, thetas[,14],
#       type="l", cex=0.7, pch=i+12, col="grey50", lwd=3)
text(3e4,0.4, pos=2, labels=paste("alpha = ",vg_n$alpha[1],"/cm"))
legend("topright", bty="o", cex=1., seg.len=1.2, 
       y.intersp=0.8, legend=vg_n$Texture, 
       col=c(selected,"gray50"), pch=c(selected+12,NA), 
       x.intersp=0.5, box.col="transparent", inset=0.01,
       lty=c(rep(NA,length(selected)),1), 
       lwd=c(rep(1,length(selected)),3))
