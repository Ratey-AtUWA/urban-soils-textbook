require(geoR)
require(OpenStreetMap)
require(prettymapr)
#
pred.grid.sv <- expand.grid(seq(min(sv17soil$Easting), max(sv17soil$Easting), by=10),
                         seq(min(sv17soil$Northing), max(sv17soil$Northing), by=10))
##################### make geodata object from datafile #####################
# [11] "ph" "ec" "al" "as" "ba" "ca" "cd" "ce" "cr" "cu" "fe" "gd" "k"  "la" "mg" "mn" "mo"
# [28] "na" "nd" "ni" "p"  "pb" "s"  "sr" "th" "v"  "y"  "zn"  #  "ec (µs/cm)"
geo.As.AR <-
    as.geodata(na.omit(as.matrix(cbind(sv17soil$Easting,sv17soil$Northing,
                                       log10(sv17soil$Cr)))))
# sv_boundary <- read.csv("sv_boundary_new.csv")
# Smiths_Lake_boundary <- read.csv("SmithsLk.boundary.csv")
# View(sv_boundary)
geo.As.AR$borders <- cbind(sv_boundary$Easting, sv_boundary$Northing)
summary(geo.As.AR)
plot(geo.As.AR)

attributes(geo.As.AR)
summary(geo.As.AR)
# dev.new()
plot(geo.As.AR)
points.geodata(geo.As.AR, xlab = "Easting (m)", ylab = "Northing (m)", 
               cex.min = 1, cex.max = 3, 
    pt.divide = quantile(geo.As.AR$data,probs=c(0.02,0.05,0.25,0.5,0.75,0.95,0.98)),
               col.seq=rainbow(8, v=0.7, start=0.01, end=.7,alpha=0.75))
legend("bottomleft", bty="n",inset=0.02,
       legend=c("0-2nd","2-5th","5-25th","25-50th","50-75th","75-95th","95-98th","98-100th"),
       pch=19,pt.cex=seq(1,3,l=8), col=rev(rainbow(8, v=0.9, start=0.01, end=.7,alpha=0.75)))
#
#
#### VARIOGRAMS ####
# bin.As.AR <- variog(geo.As.AR, 
#                           option="bin", bin.cloud=T,
#                           estimator.type="modulus", 
#                           trend="cte", max.dist=240.) #####  , breaks=10^(seq(0,4,by=0.29))
par(mar = c(3, 3, 1, 3), font.lab = 2, mgp = c(1.7, 0.3, 0), tcl = 0.5,
    cex.lab = 1.6, cex.axis = 1.4, xaxs = "i", yaxs = "i",
    lend = "square", ljoin = "mitre")
plot(bin.As.AR$v ~ bin.As.AR$u, pch = 3, lwd = 4, 
     col = "blue", cex = 1.3, xaxt = "n", yaxt = "n",
     xlab = "Distance (degrees)", ylab = "",
     xlim = c(0, 20), ylim = c(0, 0.6))
axis(1)
axis(2, col = "blue", col.ticks = "blue")
axis(4, col = "grey60", col.ticks = "grey40", mgp = c(1.6, 0.5, 0), 
     at = seq(0.1, 0.6, 0.1), 
     labels = seq(0.1, 0.6, 0.1)*4)
mtext("Semivariance (binned)", 2, 1.7, cex = 1.6, font = 2, col = "blue")
mtext("Semivariance (cloud)", 4, 1.8, cex = 1.6, font = 2, col = "grey40")
# cloud.As.AR <- variog(geo.As.AR, 
#                             option="cloud", bin.cloud=T,
#                           estimator.type="modulus", 
#                           trend="1st", max.dist=240.) #####  , breaks=10^(seq(0,4,by=0.29))
abline(v = seq(0.7,10, length.out = 16), col = "grey70", lty = 2)
points(cloud.As.AR$v/4 ~ cloud.As.AR$u, pch = 1, cex = 0.8, col = "grey60")
#
lines.variomodel(cov.model = "sph", cov.pars = c(0.3765,16.64), nugget = 0.09, lwd = 2,
                 col="blue", max.dist=20)
abline(h = 0.09, lwd = 2, col = "red3", lty = 2)
abline(h = 0.4665, lwd = 2, col = "red3", lty = 2)
points(bin.As.AR$v ~ bin.As.AR$u, pch = 3, lwd = 4, 
     col = "blue", cex = 1.3)
arrows(1, 0, 1, 0.09, lwd = 2, col = "red3", code = 3, length = 0.15)
text(1.02, 0.043, pos = 4, labels = "Nugget semivariance", 
     col = "white", cex = 1.6)
text(1, 0.045, pos = 4, labels = "Nugget semivariance", 
     col = "red3", cex = 1.6)
arrows(1, 0.09, 1, 0.4665, lwd = 2, col = "sienna", code = 3, length = 0.15)
text(1, 0.4, pos = 2, labels = "Sill semivariance", 
     col = "sienna", cex = 1.6, srt = 90, offset = 0.8)
legend("topleft", legend = c("Individual mean square differences (cloud)",
                             "Mean square differences in bins",
                             "Spherical variogram model"),
       pch = c(1, 3, NA), pt.lwd = c(1, 4, NA), lwd = c(NA, NA, 2),
       col = c("grey60", "blue", "blue"), pt.cex = c(0.9, 1.3, NA),
       cex = 1.5, inset = 0.02, box.col = "grey90", box.lwd = 2)
# 
# 
# 
# 
# 
# 
# 
# 
# plot(bin.As.AR, bin.cloud=T, main="Binned sample variogram")
smooth.As.AR <- variog(geo.As.AR, option = "smooth", 
                             n.points = 100, kernel = "normal",
                            band = 0.2, max.dist=240.)
plot(smooth.As.AR,pch=15,col="blue", main="Smoothed")
#
phimax<-as.numeric(max(geo.As.AR$data,na.rm=T))
lim0 <- pars.limits(phi = c(0,phimax), sigmasq = c(0, 240.),
                    nugget.rel = c(0,phimax))
fit.As.AR <- variofit(bin.As.AR, ini.cov.pars=eye.As.AR, 
                            fix.nugget=F, cov.model="exp",
                           weights="npairs", max.dist=240., limits=lim0)
print(fit.As.AR)
# summary(fit.As.AR)
plot(bin.As.AR,pch=19,cex=1.5, main="Binned sample variogram")
lines(fit.As.AR, col="purple", lwd=2)
#
eye.As.AR <- eyefit(bin.As.AR)
print(eye.As.AR)
#
##### DIRVTIONAL VARIOGRAMS #####
v4.As.AR <- variog4(geo.As.AR)
plot(v4.As.AR, col=c(1,3,5,7),lwd=2,cex.axis=1.4,cex.lab=1.4,cex=1.5)
#
#
