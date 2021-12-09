par(mfrow=c(2,3), mar=c(3,3,1,1), mgp=c(1.5,0.3,0),
    font.lab=2, lend="square", ljoin="mitre", tcl=0.3,
    cex.lab=1.25, cex.axis=1.2)
palette(c("black","red3","green4","blue","darkcyan","purple","sienna","gray"))
# plot stones ####
plot(Jim1998$Depth_mean~Jim1998$Stones,
     type="b", xlim=c(0,1.05*max(Jim1998$Stones, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim1998$Profile=="P",
     xlab="Stones (% >2 mm)", ylab="Depth (cm)")
text(par('usr')[1] - 0.14*(par('usr')[2] - par('usr')[1]),0,
     labels="(a)",xpd=T, font=2, cex=1.4)
points(Jim1998$Depth_mean~Jim1998$Stones,
       type="b", pch=1, col=2,
       subset=Jim1998$Profile=="Q")
points(Jim1998$Depth_mean~Jim1998$Stones,
       type="b", pch=17, col=3,
       subset=Jim1998$Profile=="R")
points(Jim1998$Depth_mean~Jim1998$Stones,
       type="b", pch=2, col=4,
       subset=Jim1998$Profile=="S")
points(Jim1998$Depth_mean~Jim1998$Stones,
       type="b", pch=15, col=5,
       subset=Jim1998$Profile=="T")
points(Jim1998$Depth_mean~Jim1998$Stones,
       type="b", pch=0, col=6,
       subset=Jim1998$Profile=="U")
# legend("topright", legend=levels(Jim1998$Profile),
#        title = "Profile", pch=c(16,1,17,2,15,0),
#        col=seq(1,6), pt.cex=1.4, bty="n", 
#        inset = 0.002, ncol=2, y.intersp = 0.85)
# plot porosity ####
plot(Jim1998$Depth_mean~Jim1998$Porosity_vol,
     type="b", xlim=c(0,1.05*max(Jim1998$Porosity_vol, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim1998$Profile=="P",
     xlab="Porosity (volumetric %)", ylab="Depth (cm)")
text(par('usr')[1] - 0.14*(par('usr')[2] - par('usr')[1]),0,
     labels="(b)",xpd=T, font=2, cex=1.4)
points(Jim1998$Depth_mean~Jim1998$Porosity_vol,
       type="b", pch=1, col=2,
       subset=Jim1998$Profile=="Q")
points(Jim1998$Depth_mean~Jim1998$Porosity_vol,
       type="b", pch=17, col=3,
       subset=Jim1998$Profile=="R")
points(Jim1998$Depth_mean~Jim1998$Porosity_vol,
       type="b", pch=2, col=4,
       subset=Jim1998$Profile=="S")
points(Jim1998$Depth_mean~Jim1998$Porosity_vol,
       type="b", pch=15, col=5,
       subset=Jim1998$Profile=="T")
points(Jim1998$Depth_mean~Jim1998$Porosity_vol,
       type="b", pch=0, col=6,
       subset=Jim1998$Profile=="U")
# plot legend ####
plot(c(0,1),c(0,1),ann=F, bty="n", xaxt="n", yaxt="n", type="n")
legend("center", cex=1.3, 
       legend=c("Lawn","Degraded N","Lawn (degraded)",
                "Degraded S","Degraded W","Local hill"),
       pch=c(19,1,17,2,15,0),
       col=seq(1,6), pt.cex=1.4, bty="n", 
       inset = 0.02, ncol=1) # title = "Profile", 
# plot pH ####
plot(Jim1998$Depth_mean~Jim1998$pH,
     type="b", xlim=c(0.95*min(Jim1998$pH, na.rm=T),
                      1.05*max(Jim1998$pH, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim1998$Profile=="P",
     xlab="pH", ylab="Depth (cm)")
text(par('usr')[1] - 0.14*(par('usr')[2] - par('usr')[1]),0,
     labels="(c)",xpd=T, font=2, cex=1.4)
points(Jim1998$Depth_mean~Jim1998$pH,
       type="b", pch=1, col=2,
       subset=Jim1998$Profile=="Q")
points(Jim1998$Depth_mean~Jim1998$pH,
       type="b", pch=17, col=3,
       subset=Jim1998$Profile=="R")
points(Jim1998$Depth_mean~Jim1998$pH,
       type="b", pch=2, col=4,
       subset=Jim1998$Profile=="S")
points(Jim1998$Depth_mean~Jim1998$pH,
       type="b", pch=15, col=5,
       subset=Jim1998$Profile=="T")
points(Jim1998$Depth_mean~Jim1998$pH,
       type="b", pch=0, col=6,
       subset=Jim1998$Profile=="U")
# legend("bottom", legend=levels(Jim1998$Profile),
#        title = "Profile", pch=c(16,1,17,2,15,0),
#        col=seq(1,6), pt.cex=1.4, bty="n", 
#        inset = 0.02, ncol=2)
# plot organic carbon ####
plot(Jim1998$Depth_mean~Jim1998$OC,
     type="b", xlim=c(0,1.05*max(Jim1998$OC, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim1998$Profile=="P",
     xlab="Organic carbon (g/kg)", ylab="Depth (cm)")
text(par('usr')[1] - 0.14*(par('usr')[2] - par('usr')[1]),0,
     labels="(d)",xpd=T, font=2, cex=1.4)
points(Jim1998$Depth_mean~Jim1998$OC,
       type="b", pch=1, col=2,
       subset=Jim1998$Profile=="Q")
points(Jim1998$Depth_mean~Jim1998$OC,
       type="b", pch=17, col=3,
       subset=Jim1998$Profile=="R")
points(Jim1998$Depth_mean~Jim1998$OC,
       type="b", pch=2, col=4,
       subset=Jim1998$Profile=="S")
points(Jim1998$Depth_mean~Jim1998$OC,
       type="b", pch=15, col=5,
       subset=Jim1998$Profile=="T")
points(Jim1998$Depth_mean~Jim1998$OC,
       type="b", pch=0, col=6,
       subset=Jim1998$Profile=="U")
# plot p_extr ####
plot(Jim1998$Depth_mean~Jim1998$P_extr,
     type="b", xlim=c(0,1.05*max(Jim1998$P_extr, na.rm=T)),
     ylim=c(90,0), cex=1.4, 
     pch=19, col=1,subset=Jim1998$Profile=="P",
     xlab="Extractable phosphorus (mg/kg)", ylab="Depth (cm)")
text(par('usr')[1] - 0.14*(par('usr')[2] - par('usr')[1]),0,
     labels="(e)",xpd=T, font=2, cex=1.4)
points(Jim1998$Depth_mean~Jim1998$P_extr,
       type="b", pch=1, col=2,
       subset=Jim1998$Profile=="Q")
points(Jim1998$Depth_mean~Jim1998$P_extr,
       type="b", pch=17, col=3,
       subset=Jim1998$Profile=="R")
points(Jim1998$Depth_mean~Jim1998$P_extr,
       type="b", pch=2, col=4,
       subset=Jim1998$Profile=="S")
points(Jim1998$Depth_mean~Jim1998$P_extr,
       type="b", pch=15, col=5,
       subset=Jim1998$Profile=="T")
points(Jim1998$Depth_mean~Jim1998$P_extr,
       type="b", pch=0, col=6,
       subset=Jim1998$Profile=="U")
names(Jim1998)
