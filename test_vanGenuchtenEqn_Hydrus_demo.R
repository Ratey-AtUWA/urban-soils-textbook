require(soilphysics)

pots <- as.data.frame(10^seq(-1,6,0.1))
pots[43,] <- 1500.
thetas <- as.data.frame(rep(NA,NROW(pots)))
colnames(thetas) <- c("pot")
thetas$pot <- pots[,1]

par(mfrow=c(2,1), mar=c(3.5,3.5,0.75,0.75), mgp=c(1.5,0.3,0), 
    cex.lab=1.1, cex.axis=1., font.lab=1, tcl=0.4)
palette(c("black", rev(rainbow(4, v=0.67, end=0.77))))
#
vg_alpha <- data.frame(theta_r = rep(0.01,3),
                       theta_sat = rep(0.48,3),
                       n = rep(1.8,3),
                       alpha_per_cm = c(0.005,0.03,0.2))

vg_alpha$Texture <- paste0("\u03B1 = ",vg_alpha$alpha_per_cm)
#
for (i in 1:length(vg_alpha$alpha_per_cm)) {
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
     pos=c(3,3), cex=1.2, col="gray50", offset=0.25, srt=90)
axis(1, at=(c(0.1,10,1000,100000)),
     labels=c('-0.1','-10','-1000','-100000'))
# for all textures replace vector with 1:NROW(vg)
selected <- seq(1,3)
for (i in selected) {
  lines(thetas$pot, thetas[,i+1], type="b", cex=0.7, pch=i-1, col=i)
}
# lines(thetas$pot, thetas[,14],
#       type="l", cex=0.7, pch=i+12, col="grey50", lwd=3)
legend("topright", bty="n", cex=1.2, seg.len=1.2, 
       y.intersp=1., legend=vg_alpha$Texture, 
       col=c(selected,"gray50"), pch=c(selected-1,NA), 
       x.intersp=0.5, box.col="transparent", inset=0.03,
       lty=c(rep(NA,length(selected)),1), 
       lwd=c(rep(1,length(selected)),3),
       title=paste("n = ",vg_alpha$n[1],"/cm"))
text(c(0.3,1e5), c(0.485,0.02), labels = c("Qs = 0.48","Qr = 0.01"), pos = 3)
# __________________________________________________
#
vg_n <- data.frame(theta_r = rep(0.01,3),
                       theta_sat = rep(0.48,3),
                       n = c(1.1,1.4,2.7),
                       alpha_per_cm = rep(0.03,3))
vg_n$Texture <- paste0("n = ",vg_n$n)
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
     pos=c(3,3), cex=1.2, col="gray50", offset=0.25, srt=90)
axis(1, at=(c(0.1,10,1000,100000)),
     labels=c('-0.1','-10','-1000','-100000'))
# for all textures replace vector with 1:NROW(vg)
selected <- seq(1,13)
for (i in selected) {
  lines(thetas$pot, thetas[,i+1], type="b", cex=0.7, pch=i+6, col=i)
}
# lines(thetas$pot, thetas[,14],
#       type="l", cex=0.7, pch=i+12, col="grey50", lwd=3)
text(c(0.3,3e5), c(0.485,0.02), labels = c("Qs = 0.48","Qr = 0.01"), pos = 3)
legend("topright", bty="n", cex=1.2, seg.len=1.2, 
       y.intersp=1., legend=vg_n$Texture, 
       col=c(selected,"gray50"), pch=c(selected+6,NA), 
       x.intersp=0.5, box.col="transparent", inset=0.03,
       lty=c(rep(NA,length(selected)),1), 
       lwd=c(rep(1,length(selected)),3), 
     title=paste("Alpha = ",vg_n$alpha[1],"/cm"))
