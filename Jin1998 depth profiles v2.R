Jim2 <- Jim1998[,c(1,5,6,23,2,26,30)]
Jim2$Depth_sq <- Jim2$Depth_mean^2
Jim2$Depth_cu <- Jim2$Depth_mean^3
par(mfrow=c(2,3), mar=c(3,3,1,1), mgp=c(1.5,0.3,0),
    font.lab=2, lend="square", ljoin="mitre", tcl=0.3,
    cex.lab=1.25, cex.axis=1.2)
palette(c("black","red3","green4","blue","darkcyan","purple","sienna","gray"))
# plot stones ####
plot(Jim2$Depth_mean~Jim2$Stones,
     type="p", xlim=c(0,1.05*max(Jim2$Stones, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim2$Profile=="P",
     xlab="Stones (% >2 mm)", ylab="Depth (cm)")
text(par('usr')[1] - 0.12*(par('usr')[2] - par('usr')[1]),0,
     labels="(a)",xpd=T, font=2, cex=1.4)
  lm0 <- lm(Jim2$Stones ~ Jim2$Depth_mean + 
                Jim2$Depth_sq + Jim2$Depth_cu, 
            subset=Jim2$Profile=="P") 
  xseq <- seq(min(Jim2$Depth_mean[1:4], na.rm=T), max(Jim2$Depth_mean[1:4], na.rm=T), 
            length.out = 100)
  xseq2 <- xseq^2
  xseq3 <- xseq^3
  yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
      lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
  lines(xseq ~ yseq, lwd = 2, lty = 1)
#
points(Jim2$Depth_mean~Jim2$Stones,
       type="p", pch=17, col = 4,
       subset=Jim2$Profile=="R")
  lm0 <- lm(Jim2$Stones ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="R") 
  xseq <- seq(min(Jim2$Depth_mean[10:14], na.rm=T), 
              max(Jim2$Depth_mean[10:14], na.rm=T), 
            length.out = 100)
  xseq2 <- xseq^2
  xseq3 <- xseq^3
  yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
  lines(xseq ~ yseq, lwd = 2, lty = 3, col = 4)
#
points(Jim2$Depth_mean~Jim2$Stones,
       type="p", pch=0, col = 7,
       subset=Jim2$Profile=="U")
lm0 <- lm(Jim2$Stones ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="U") 
xseq <- seq(min(Jim2$Depth_mean[25:29], na.rm=T), max(Jim2$Depth_mean[25:29], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 6, col = 7)
#
# plot porosity ####
plot(Jim2$Depth_mean~Jim2$Porosity_vol,
     type="p", xlim=c(0,1.05*max(Jim2$Porosity_vol, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim2$Profile=="P",
     xlab="Porosity (volumetric %)", ylab="Depth (cm)")
text(par('usr')[1] - 0.12*(par('usr')[2] - par('usr')[1]),0,
     labels="(b)",xpd=T, font=2, cex=1.4)
lm0 <- lm(Jim2$Porosity_vol ~ Jim2$Depth_mean + 
              Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="P") 
xseq <- seq(min(Jim2$Depth_mean[1:4], na.rm=T), max(Jim2$Depth_mean[1:4], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 1)
#
points(Jim2$Depth_mean~Jim2$Porosity_vol,
       type="p", pch=17, col = 4,
       subset=Jim2$Profile=="R")
lm0 <- lm(Jim2$Porosity_vol ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="R") 
xseq <- seq(min(Jim2$Depth_mean[10:14], na.rm=T), 
            max(Jim2$Depth_mean[10:14], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 3, col = 4)
#
points(Jim2$Depth_mean~Jim2$Porosity_vol,
       type="p", pch=0, col = 7,
       subset=Jim2$Profile=="U")
lm0 <- lm(Jim2$Porosity_vol ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="U") 
xseq <- seq(min(Jim2$Depth_mean[25:29], na.rm=T), max(Jim2$Depth_mean[25:29], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 6, col = 7)
#
# plot legend ####
plot(c(0,1),c(0,1),ann=F, bty="n", xaxt="n", yaxt="n", type="n")
legend("left", cex=1.7, title = expression(bolditalic("Parkland Zone")),
       legend=c("Lawn", "Lawn (degraded)", "Local hill"),
       pch=c(19,17,0), lty = c(1,3,6), lwd = 2, seg.len = 3,
       col=c(1,4,7), pt.cex=1.7, bty="n", 
       inset = 0.02, ncol=1) #  
# plot pH ####
plot(Jim2$Depth_mean~Jim2$pH,
     type="p", xlim=c(0.95*min(Jim2$pH, na.rm=T),
                      1.05*max(Jim2$pH, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim2$Profile=="P",
     xlab="pH", ylab="Depth (cm)")
text(par('usr')[1] - 0.12*(par('usr')[2] - par('usr')[1]),0,
     labels="(c)",xpd=T, font=2, cex=1.4)
lm0 <- lm(Jim2$pH ~ Jim2$Depth_mean + 
              Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="P") 
xseq <- seq(min(Jim2$Depth_mean[1:4], na.rm=T), max(Jim2$Depth_mean[1:4], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 1)
#
points(Jim2$Depth_mean~Jim2$pH,
       type="p", pch=17, col = 4,
       subset=Jim2$Profile=="R")
lm0 <- lm(Jim2$pH ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="R") 
xseq <- seq(min(Jim2$Depth_mean[10:14], na.rm=T), 
            max(Jim2$Depth_mean[10:14], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 3, col = 4)
#
points(Jim2$Depth_mean~Jim2$pH,
       type="p", pch=0, col = 7,
       subset=Jim2$Profile=="U")
lm0 <- lm(Jim2$pH ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="U") 
xseq <- seq(min(Jim2$Depth_mean[25:29], na.rm=T), max(Jim2$Depth_mean[25:29], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 6, col = 7)
#
# plot organic carbon ####
plot(Jim2$Depth_mean~Jim2$OC,
     type="p", xlim=c(0,1.05*max(Jim2$OC, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim2$Profile=="P",
     xlab="Organic carbon (g/kg)", ylab="Depth (cm)")
text(par('usr')[1] - 0.11*(par('usr')[2] - par('usr')[1]),0,
     labels="(d)",xpd=T, font=2, cex=1.4)
lm0 <- lm(Jim2$OC ~ Jim2$Depth_mean + 
              Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="P") 
xseq <- seq(min(Jim2$Depth_mean[1:4], na.rm=T), max(Jim2$Depth_mean[1:4], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 1)
#
points(Jim2$Depth_mean~Jim2$OC,
       type="p", pch=17, col = 4,
       subset=Jim2$Profile=="R")
lm0 <- lm(Jim2$OC ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="R") 
xseq <- seq(min(Jim2$Depth_mean[10:14], na.rm=T), 
            max(Jim2$Depth_mean[10:14], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 3, col = 4)
#
points(Jim2$Depth_mean~Jim2$OC,
       type="p", pch=0, col = 7,
       subset=Jim2$Profile=="U")
lm0 <- lm(Jim2$OC ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="U") 
xseq <- seq(min(Jim2$Depth_mean[25:29], na.rm=T), max(Jim2$Depth_mean[25:29], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 6, col = 7)
#
# plot p_extr ####
plot(Jim2$Depth_mean~Jim2$P_extr,
     type="p", xlim=c(0,1.05*max(Jim2$P_extr, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim2$Profile=="P",
     xlab="Extractable phosphorus (mg/kg)", ylab="Depth (cm)")
text(par('usr')[1] - 0.11*(par('usr')[2] - par('usr')[1]),0,
     labels="(e)",xpd=T, font=2, cex=1.4)
lm0 <- lm(Jim2$P_extr ~ Jim2$Depth_mean + 
              Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="P") 
xseq <- seq(min(Jim2$Depth_mean[1:4], na.rm=T), max(Jim2$Depth_mean[1:4], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 1)
#
points(Jim2$Depth_mean~Jim2$P_extr,
       type="p", pch=17, col = 4,
       subset=Jim2$Profile=="R")
lm0 <- lm(Jim2$P_extr ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="R") 
xseq <- seq(min(Jim2$Depth_mean[10:14], na.rm=T), 
            max(Jim2$Depth_mean[10:14], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 3, col = 4)
#
points(Jim2$Depth_mean~Jim2$P_extr,
       type="p", pch=0, col = 7,
       subset=Jim2$Profile=="U")
lm0 <- lm(Jim2$P_extr ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
          subset=Jim2$Profile=="U") 
xseq <- seq(min(Jim2$Depth_mean[25:29], na.rm=T), max(Jim2$Depth_mean[25:29], na.rm=T), 
            length.out = 100)
xseq2 <- xseq^2
xseq3 <- xseq^3
yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
    lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
lines(xseq ~ yseq, lwd = 2, lty = 6, col = 7)
#
# names(Jim2)
rm(list = c("lm0", "xseq", "yseq"))

#### -=-=-=-=-=-=- unused code -=-=-=-=-=-=- ####
# points(Jim2$Depth_mean~Jim2$Stones,
#        type="p", pch=1, col=2,
#        subset=Jim2$Profile=="Q")
#   lm0 <- lm(Jim2$Stones ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
#             subset=Jim2$Profile=="Q") 
#   xseq <- seq(min(Jim2$Depth_mean[5:9], na.rm=T), max(Jim2$Depth_mean[5:9], na.rm=T), 
#             length.out = 100)
#   xseq2 <- xseq^2
#   xseq3 <- xseq^3
#   yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
#       lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
#   lines(xseq ~ yseq, lwd = 2, lty = 2, col =2)
#
# points(Jim2$Depth_mean~Jim2$Stones,
#        type="p", pch=2, col=4,
#        subset=Jim2$Profile=="S")
# lm0 <- lm(Jim2$Stones ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
#           subset=Jim2$Profile=="S") 
# xseq <- seq(min(Jim2$Depth_mean[15:19], na.rm=T), 
#             max(Jim2$Depth_mean[15:19], na.rm=T), 
#             length.out = 100)
# xseq2 <- xseq^2
# xseq3 <- xseq^3
# yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
#     lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
# lines(xseq ~ yseq, lwd = 2, lty = 4, col =4)
# #
# points(Jim2$Depth_mean~Jim2$Stones,
#        type="p", pch=15, col=5,
#        subset=Jim2$Profile=="T")
# lm0 <- lm(Jim2$Stones ~ Jim2$Depth_mean + Jim2$Depth_sq + Jim2$Depth_cu, 
#           subset=Jim2$Profile=="T") 
# xseq <- seq(min(Jim2$Depth_mean[20:24], na.rm=T), 
#             max(Jim2$Depth_mean[20:24], na.rm=T), 
#             length.out = 100)
# xseq2 <- xseq^2
# xseq3 <- xseq^3
# yseq <- lm0$coef[1] + lm0$coef[2]*xseq + 
#     lm0$coef[3]*xseq2 + lm0$coef[4]*xseq3
# lines(xseq ~ yseq, lwd = 2, lty = 5, col =5)
#
