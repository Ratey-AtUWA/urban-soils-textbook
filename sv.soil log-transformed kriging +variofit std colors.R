require(geoR)
require(OpenStreetMap)
require(prettymapr)
#
pred.grid.sv <- expand.grid(seq(min(sv17soil$Easting), max(sv17soil$Easting), by=10),
                         seq(min(sv17soil$Northing), max(sv17soil$Northing), by=10))
##################### make geodata object from datafile #####################
# [11] "ph" "ec" "al" "as" "ba" "ca" "cd" "ce" "cr" "cu" "fe" "gd" "k"  "la" "mg" "mn" "mo"
# [28] "na" "nd" "ni" "p"  "pb" "s"  "sr" "th" "v"  "y"  "zn"  #  "ec (µs/cm)"
geo.sv17soil.EC <-
    as.geodata(na.omit(as.matrix(cbind(sv17soil$Easting,sv17soil$Northing,
                                       log10(sv17soil$EC)))))
# sv_boundary <- read.csv("sv_boundary_new.csv")
# Smiths_Lake_boundary <- read.csv("SmithsLk.boundary.csv")
# View(sv_boundary)
geo.sv17soil.EC$borders <- cbind(sv_boundary$Easting, 
                                 sv_boundary$Northing)
summary(geo.sv17soil.EC)
plot(geo.sv17soil.EC)
#####################  kriging interpolation of data for map image ######################
bin.sv17soil.EC <- variog(geo.sv17soil.EC, option="bin", bin.cloud=T, 
                          estimator.type="modulus", max.dist=240.) 
# names(bin.sv17soil.EC)
#
dev.new()
eye.sv17soil.EC <- eyefit(bin.sv17soil.EC)
print(eye.sv17soil.EC)
dev.off()
names(sv17soil)
#
fit.sv17soil.EC <- variofit(bin.sv17soil.EC, 
                            ini.cov.pars=eye.sv17soil.EC, 
                            fix.nugget=F, cov.model="exp",
                           weights="npairs", max.dist=240.) 
print(fit.sv17soil.EC)
# summary(fit.sv17soil.EC)
plot(bin.sv17soil.EC,pch=19,cex=1.5, 
     main="Binned sample variogram")
lines(fit.sv17soil.EC, col="purple", lwd=2)
#
kc.sv17soil.EC <- krige.conv(geo.sv17soil.EC, 
                             loc = pred.grid.sv, 
                             krige = 
                               krige.control(obj.m = 
                                               eye.sv17soil.EC))
10^summary(kc.sv17soil.EC$predict)
summary(kc.sv17soil.EC$krige.var)
#
##################### just the map(s) #####################
# dev.new(width=8, height=8.00)
palette(c("black","blue4","blue2","cyan4",
          "green3","gold2","darkorange1",
          "tomato3","red2","white","transparent"))
par(lend=2,ljoin=1, mgp=c(2.0,0.5,0), 
    mar=c(4.5,4.5,1.5,1), mfrow=c(1,1))
plot(smith2.utm, removeMargin=FALSE)
# ADD AXES, LABELS, TITLES, GRIDLINES, NORTH ARROW, SCALE BAR
axis(1, tcl=0.1, padj = 0, cex.axis=1.4) # HORIZ # 
mtext("Zone 50 UTM Easting (m)", side=1, line=2., 
      cex=1.4, font=2) # HORIZ AXIS LABEL
axis(2, tcl=0.1, padj = 0, cex.axis=1.4, 
     at=seq(6466200,6466700,100), 
     labels=seq(6466200,6466700,100)) # VERT # 
mtext("Zone 50 UTM Northing (m)", side=2, line=2., 
      cex=1.4, font=2) # VERT AXIS LABEL
image(kc.sv17soil.EC, loc = pred.grid.sv, 
      values=kc.sv17soil.EC$pred, add=T 
      cex.lab=1.4, cex.axis=1.5,
      col = colorRampPalette(c("#C0C0FF", 
                               "#D0D000", 
                               "#C00000"), 
                             space="Lab")(21))
polygon(cvr.buildings$Easting, cvr.buildings$Northing,
        col="grey70", border="grey60")
polygon(c(391375,391375,391395,391395),
        c(6466645,6466665,6466665,6466645), 
        col="#004E75", border="#004E75")
text(391385,6466655,labels="P", col=10, cex=1.5, font=2)
contour(kc.sv17soil.EC, add=T, values=kc.sv17soil.EC$pred, 
        col="grey20", labcex=1,
  levels=log10(c(0.01,0.02,0.03,0.1,0.2,0.3,
                 1,2,3,10,20,30,50,100,200,300,
                 500,1000,2000,3000,1e4)),
  labels=c(0.01,0.02,0.03,0.1,0.2,0.3,
           1,2,3,10,20,30,50,100,200,300,
           500,1000,2000,3000,10000))
lines(geo.sv17soil.EC$borders[,1],geo.sv17soil.EC$borders[,2],
      col="olivedrab3",lwd=2) # "#DBF0DA"
polygon(Smiths_Lake_boundary$Easting, 
        Smiths_Lake_boundary$Northing, lwd=1, 
        col="#40D0F0", border="#3080E0")
text(391355,6466464,labels="Smiths\n  Lake", 
     col="blue3", font=3, cex=0.7)
text(391280,6466500, labels="Electrical\nSupply\nSubstation", 
     font=3, col="grey33", family='nar')
contour(sv.grid, add=T, labcex=1.2, nlevels=8, vfont=NULL, 
        col="orange3", method="flattest", lty=2)
# polygon(c(391115,391115,391145,391145), 
#         c(6466515,6466495,6466495,6466515), lwd=1,
#         col="#E0E0E0", border="#E0E0E0")
text(391130, 6466505,labels="\u2588\u2588",
     col="#E0E0E0", cex=1.2, srt=45)
text(391130, 6466505,labels="18", col="orange3", 
     cex=1.2, srt=45)
text(391470, 6466400, labels="\u2588\u2588",
     col="#E0E0E0", cex=1.2, srt=90)
text(391470, 6466400, labels="16", col="orange3", 
     cex=1.2, srt=90)
# abline(v=seq(391100,391500,100), col="grey", lty=3)
# abline(h=seq(6466200,6466700,100), col="grey", lty=3) \u2588
# 
addnortharrow(padin=c(0.3,0.3), scale=1.2, lwd=2)
addscalebar(plotunit="m", htin=0.2, padin=c(0.3,0.3), 
            label.cex=1.2, 
            lwd=2, pos="bottomright")
#
polygon(c(390940,390940,391210,391210), 
        c(6466355,6466275,6466275,6466355), lwd=2,
  col="grey88", border="white")
# text(390951, 6466299, labels="Soil EC", cex=1.2, pos=4, 
#      font=2, col="white")
text(390950, 6466340, labels="Soil EC (mg/kg)", cex=1.2, pos=4, 
     font=2, col="black")
legend.krige(x.leg=c(390950,391200), y.leg=c(6466280,6466320), 
             val=10^kc.sv17soil.EC$pred, cex=1.2,
  col = colorRampPalette(c("#C0C0FF", "#D0D000", "#C00000"), 
                         space="Lab")(21), 
  offset.leg=-0.5)
legend(390940,6466270, 
       legend=c("Sample points", "     ... >95% CI", "Project boundary     "), 
       cex=1.2, horiz=F, pch=c(3,15,NA), pt.cex=c(1.,2.5,.01), lwd=c(NA,NA,2), 
       lty=c(NA,NA,1), pt.lwd=c(2,2,NA), col=c(1,8,"olivedrab3"), bty="o", 
       text.col=1, box.col=10, box.lwd=2, bg="gray88", inset=0.03, y.intersp=1.2)
legend(390940,6466270, legend=c("\u00A0", "\u00A0", "\u00A0"), cex=1.2, col=c(11,10,11), 
       horiz=F, pch=c(NA,1,NA), pt.cex=c(NA,1.5,NA), lwd=NA, lty=NA, pt.lwd=c(NA,3,NA), 
       bty="n", text.col=1, box.lwd=2, inset=0.03, y.intersp=1.2)
legend(390940,6466270, legend=c("\u00A0", "\u00A0", "\u00A0"), cex=1.2, col=c(11,1,11), 
       horiz=F, pch=c(NA,3,NA), pt.cex=c(NA,1.,NA), lwd=NA, lty=NA, pt.lwd=c(NA,2,NA), 
       bty="n", text.col=1, box.lwd=2, inset=0.03, y.intersp=1.2)
#### add sampling points ####
points(sv17soil$Northing~sv17soil$Easting, pch=3, col=1, cex=1, lwd=2)
q95 <- signif(10^quantile(geo.sv17soil.EC$data,probs=c(0.95)),3)
points(sv17soil$Northing~sv17soil$Easting, pch=1, col=10, cex=1.6, 
       lwd=3, subset=sv17soil$V>q95)
points(sv17soil$Northing~sv17soil$Easting, pch=3, col=1, cex=1., 
       lwd=2, subset=sv17soil$V>q95)
rm(q95)
box()
#
# end of code
#
#
#
#
#
#?????????????????? IN CASE LOOKING AT THE ACTUAL VARIOGRAM IS NEEDED #??????????????????
attributes(geo.sv17soil.EC)
summary(geo.sv17soil.EC)
# dev.new()
plot(geo.sv17soil.EC)
points.geodata(geo.sv17soil.EC, xlab = "Easting (m)", ylab = "Northing (m)", 
               cex.min = 1, cex.max = 3, 
    pt.divide = quantile(geo.sv17soil.EC$data,probs=c(0.02,0.05,0.25,0.5,0.75,0.95,0.98)),
               col.seq=rev(rainbow(8, v=0.7, start=0.01, end=.7,alpha=0.75)))
legend("bottomleft", bty="n",inset=0.02,
       legend=c("0-2nd","2-5th","5-25th","25-50th","50-75th","75-95th","95-98th","98-100th"),
       pch=19,pt.cex=seq(1,3,l=8), col=rev(rainbow(8, v=0.7, start=0.01, end=.7,alpha=0.75)))
#
#
# VARIOGRAMS #
bin.sv17soil.EC <- variog(geo.sv17soil.EC, 
                          option="bin", bin.cloud=T,
                          estimator.type="modulus", 
                          trend="cte", max.dist=240.) #####  , breaks=10^(seq(0,4,by=0.29))
plot(bin.sv17soil.EC)
cloud.sv17soil.EC <- variog(geo.sv17soil.EC, 
                            option="cloud", bin.cloud=T,
                          estimator.type="modulus", 
                          trend="1st", max.dist=240.) #####  , breaks=10^(seq(0,4,by=0.29))
plot(cloud.sv17soil.EC)
#
lines.variomodel(cov.model = "exp", cov.pars = c(0.12,200.), nugget = 0.2, lwd = 2,
                 col="skyblue", max.dist=240.)
# plot(bin.sv17soil.EC, bin.cloud=T, main="Binned sample variogram")
smooth.sv17soil.EC <- variog(geo.sv17soil.EC, option = "smooth", 
                             n.points = 100, kernel = "normal",
                            band = 0.2, max.dist=240.)
plot(smooth.sv17soil.EC,pch=15,col="blue", main="Smoothed")
#
phimax<-as.numeric(max(geo.sv17soil.EC$data,na.rm=T))
lim0 <- pars.limits(phi = c(0,phimax), sigmasq = c(0, 240.),
                    nugget.rel = c(0,phimax))
fit.sv17soil.EC <- variofit(bin.sv17soil.EC, ini.cov.pars=eye.sv17soil.EC, 
                            fix.nugget=F, cov.model="exp",
                           weights="npairs", max.dist=240., limits=lim0)
print(fit.sv17soil.EC)
# summary(fit.sv17soil.EC)
plot(bin.sv17soil.EC,pch=19,cex=1.5, main="Binned sample variogram")
lines(fit.sv17soil.EC, col="purple", lwd=2)
#
eye.sv17soil.EC <- eyefit(bin.sv17soil.EC)
print(eye.sv17soil.EC)
#
##### directional variograms #####
v4.sv17soil.EC <- variog4(geo.sv17soil.EC)
plot(v4.sv17soil.EC, col=c(1,3,5,7),lwd=2,cex.axis=1.4,cex.lab=1.4,cex=1.5)
#
#
