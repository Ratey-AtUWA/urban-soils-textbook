require(OpenStreetMap)
require(prettymapr)
# require(astro)
# outdoors-v11 satellite-v9
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}"
#
wellington.osm <- openmap(c(-41.2634,174.762), 
                          c(-41.3070,174.792),
                         type=paste0(baseUrl,apiKey), zoom=15.)
wellington.utm <- openproj(wellington.osm, projection="+proj=utm +zone=60 +south") # 
rm(wellington.osm)
windowsFonts('mon2'='Consolas')
palette(c("black","dodgerblue","skyblue","cyan","green",
          "yellow","orange","tomato","red2","darkorchid1",
          "white","transparent"))

sf <- 3
png(file = "wellington_utm.png", width = 400 * sf, height = 725 * sf)

ll_x <- cbind(c(174.77,174.78,174.79), rep(-41.306,3))
colnames(ll_x) <- c("X","Y")
attr(ll_x, "projection") <- "LL"
utm_x <- convUL(ll_x, km=FALSE, southern=TRUE)

ll_y <- cbind(rep(174.768,4), c(-41.30,-41.29,-41.28,-41.27))
colnames(ll_y) <- c("X","Y")
attr(ll_y, "projection") <- "LL"
utm_y <- convUL(ll_y, km=FALSE, southern=TRUE)

par(mfrow=c(1,1), mar=c(6,3,0.5,1)*sf, mgp=c(1.5,0.5,0)*sf, 
    oma=c(0,0,0,0), lend=2, ljoin=1, xpd = TRUE)
plot(wellington.utm, removeMargin=FALSE)
axis(1, tcl=0.3*sf, las = 2, cex.axis=1. * sf, lwd = 1*sf, 
     mgp=c(1.8,0.5,0)*sf, at = utm_x[,1], labels = ll_x[,1])
# mtext("Longitude, \u00B0E", side=1, line=3*sf, cex=1.2*sf, font=2) 
text(312800, 5424100, labels = "Longitude\n      \u00B0E", 
     srt = 90, font = 2, pos=4, cex = 1.2*sf)
axis(2, tcl=0.3*sf, cex.axis= 1 * sf, lwd = 1*sf,
     at = utm_y[,2], labels = ll_y[,2]) 
mtext("Latitude, \u2013\u00B0S", side=2, line=1.5*sf, cex=1.2*sf, font=2)
polygon(recl2$Easting,recl2$Northing,border="#98F5FF",col="#98F5FF80", lwd = 1*sf)
polygon(infills$Easting,infills$Northing, border="#FFFF00", col = "#FFFF0080", lwd = 1*sf)
# polygon(v_fills$Long[1:275],v_fills$Lat[1:275], border="#FFA500", col = "#FFA50080", lwd = 1*sf)
# polygon(v_fills$Long[277:287],v_fills$Lat[277:287], border="#FFA500", col = "#6F806F", lwd = 1*sf)
addnortharrow(pos="topleft", text.col=1, scale=1.2 * sf, padin=c(0.225,0.22)*sf, 
              lwd=1 * sf, border=12, cols = c(12,12))
addnortharrow(pos="topleft", text.col=11, scale=1.2 * sf, padin=c(0.2,0.2)*sf, 
              lwd=1 * sf, border=11)
addscalebar(plotepsg=32760, htin=0.2*sf, label.cex=1.2*sf, widthhint=0.4,
            label.col=1, linecol=12, lwd=1*sf, padin=c(0.38,0.25)*sf,
            bar.cols = c(12,12))
addscalebar(plotepsg=32760, htin=0.2*sf, label.cex=1.2*sf, widthhint=0.4, 
            label.col=11, linecol=11, lwd=1*sf, padin=c(0.36,0.27)*sf)
box(lwd = 1*sf)
rm(list = c("sf", "ll_x","ll_y","utm_x","utm_y"))
dev.off()
abline(h=-41.3, col="red", lty=2)
abline(v=174.78, col="red", lty=2)
symcols <- colorRampPalette(c("#0000FF","#577220", "#FFE000"),
                            alpha=T)( nrow(afs19) )
# addnortharrow(pos="bottomright", text.col=1, scale=1.2, padin=c(0.32,0.32),
#               lwd=1, border=1)
# symbols(afs19$Easting, afs19$Northing, add=T, circles=afs19$pH-3, 
#         bg="#ffff0080", fg="#FFFF80", inches=0.2)
# symbols(afs19$Easting, afs19$Northing, add=T, circles=sqrt(afs19$Zn), 
#         bg=symcols[afs19$color_fac], 
#         fg="white", inches=0.15)
afs19$color_fac <- as.factor(afs19$Zn)
points(afs19$Easting, afs19$Northing, pch=15, 
       cex=seq(0.8,3,len=nrow(afs19))[afs19$color_fac],
       col=symcols[afs19$color_fac])
points(afs19$Easting, afs19$Northing, pch=0,
       cex=seq(0.8,3,len=nrow(afs19))[afs19$color_fac],
       col="white")
afs19$color_fac <- NULL
# points(afs19$Easting, afs19$Northing, pch=15, cex=1, 
#        col=symcols[afs19$color_fac])
# text(afs19$Easting, afs19$Northing, col="#FFFF20", labels=afs19$Group,
#      cex=0.5, pos=4, offset = 0.15)
legend(399830,6468380, bty="o", pch=22, bg="#102000", box.lwd=2, 
       box.col="#204000",col=11, text.col=11, pt.cex=c(0.8,1.9,3), 
       title=expression(italic("Continous size/colour scale from:")), 
       legend=c("minimum Zn, to","median Zn, to","maximum Zn."), 
       pt.bg=symcols[c(1,round(nrow(afs19)/2,0),nrow(afs19))])
rm(symcols)
text(afs19$Easting, afs19$Northing, labels=signif(afs19$Zn,3), cex=0.5,
     col=1)
#
# symbols(afs19$Easting, afs19$Northing, circles=sqrt(afs19$Zn), add=T,
#         inches=0.25, fg=11, 
#         bg=bgcols)
#
stripchart(subset(afs19$Zn, subset=afs19$Zn<200.), method="jitter", 
           xlab="Zn (mg/kg)", log="x", xlim=c(10,5210))
stripchart(subset(afs19$Zn, subset=afs19$Zn>=200.&afs19$Zn<410), 
           method="jitter", xlab="Zn (mg/kg)", log="x", 
           xlim=c(10,5210), pch=0, col="tomato", add=TRUE)
stripchart(subset(afs19$Zn, subset=afs19$Zn>=410), method="jitter", 
           xlab="Zn (mg/kg)", log="x", add=TRUE, col="red", pch=15)
abline(v=410, col="red", lty=2)
text(410, 0.8, pos=4, labels="ISQG-high\n(410 mg/kg)", col="red",
     offset=0.2)
abline(v=200, col="orange3", lty=3)
text(200, 1.2, pos=4, labels="ISQG-low\n(200\nmg/kg)", col="tomato",
     offset=0.2)
#
# [end code]