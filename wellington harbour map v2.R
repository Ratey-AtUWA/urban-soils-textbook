require(OpenStreetMap)
require(prettymapr)
require(PBSmapping)
# require(astro)
# outdoors-v11 satellite-v9 streets-v11
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}"
#
wellington.osm <- openmap(c(-41.27,174.762),
                          c(-41.30,174.792),
                         type=paste0(baseUrl,apiKey), zoom=15.)
wellington2.utm <- openproj(wellington.osm, projection="+proj=utm +zone=60 +south") #
rm(wellington.osm)
windowsFonts('mon2'='Consolas')
palette(c("black","dodgerblue","skyblue","cyan","green",
          "yellow","orange","tomato","red2","darkorchid1",
          "white","transparent"))

sf <- 3
png(file = "wellington3_utm.png", width = 450 * sf, height = 560 * sf)

ll_x <- cbind(c(174.77,174.78,174.79), rep(-41.306,3))
colnames(ll_x) <- c("X","Y")
attr(ll_x, "projection") <- "LL"
utm_x <- convUL(ll_x, km=FALSE, southern=TRUE)

ll_y <- cbind(rep(174.768,4), c(-41.30,-41.29,-41.28,-41.27))
colnames(ll_y) <- c("X","Y")
attr(ll_y, "projection") <- "LL"
utm_y <- convUL(ll_y, km=FALSE, southern=TRUE)

par(mfrow=c(1,1), mar=c(3,3,0.8,1)*sf, mgp=c(1.5,0.5,0)*sf, 
    oma=c(0,0,0,0), lend=2, ljoin=1, xpd = FALSE)
plot(wellington2.utm, removeMargin=FALSE)
axis(1, tcl=0.2*sf, las = 1, cex.axis=1.2 * sf, lwd = 1*sf, 
     mgp=c(1.85,0.65,0)*sf, at = utm_x[,1], labels = ll_x[,1])
mtext("Longitude, \u00B0E", side=1, line=1.75*sf, cex=1.4*sf, font=2)
# text(312800, 5424100, labels = "Longitude\n      \u00B0E", 
#      srt = 90, font = 2, pos=4, cex = 1.2*sf)
axis(2, tcl=0.2*sf, cex.axis= 1.2 * sf, lwd = 1*sf,
     mgp=c(1.75,0.35,0)*sf, at = utm_y[,2], labels = ll_y[,2]*-1) 
mtext("Latitude, \u00B0S", side=2, line=1.75*sf, cex=1.4*sf, font=2)
polygon(recl2$Easting,recl2$Northing,border="#98F5FF",col="#98F5FF80", lwd = 1*sf)
polygon(infills$Easting,infills$Northing, border="#FFFF00", col = "#FFFF0080", lwd = 1*sf)
# polygon(v_fills$Long[1:275],v_fills$Lat[1:275], border="#FFA500", col = "#FFA50080", lwd = 1*sf)
# polygon(v_fills$Long[277:287],v_fills$Lat[277:287], border="#FFA500", col = "#6F806F", lwd = 1*sf)
addnortharrow(pos="topright", text.col=1, scale=1 * sf, padin=c(0.275,0.22)*sf, 
              lwd=1 * sf, border=12, cols = c(12,12))
addnortharrow(pos="topright", text.col=11, scale=1 * sf, padin=c(0.25,0.2)*sf, 
              lwd=1 * sf, border=11)
addscalebar(plotepsg=32760, htin=0.15*sf, label.cex=1.2*sf, widthhint=0.2,
            label.col=1, linecol=12, lwd=1*sf, padin=c(0.28,0.15)*sf,
            bar.cols = c(12,12))
addscalebar(plotepsg=32760, htin=0.15*sf, label.cex=1.2*sf, widthhint=0.2, 
            label.col=11, linecol=11, lwd=1*sf, padin=c(0.26,0.17)*sf)
box(lwd = 1*sf)
legend("topleft", inset = 0.011, bty = "n",
       legend = c("Reclaimed harbour","In-filled valley"),
       pch = c(22,22), col = c(12,12), 
       pt.bg = c(12,12), text.col = 1,
       pt.cex = 2.4*sf, cex = 1.4*sf, pt.lwd = 1*sf)
legend("topleft", inset = 0.01, bty = "n",
       legend = c("Reclaimed harbour","In-filled valley"),
       pch = c(22,22), col = c("#98F5FF","yellow"), 
       pt.bg = c("#98F5FF80","#FFFF0080"), text.col = 11,
       pt.cex = 2.4*sf, cex = 1.4*sf, pt.lwd = 1*sf,
       y.intersp = 1.4)
rm(list = c("sf", "ll_x","ll_y","utm_x","utm_y"))
dev.off()
options(device = "RStudioGD")
getOption("device")
#
# [end code]