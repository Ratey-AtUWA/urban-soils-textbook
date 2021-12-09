require(OpenStreetMap)
require(png)
# cities <- read.csv(file="cities.csv")
# styles are: mapbox://styles/mapbox/streets-v11
# mapbox://styles/mapbox/outdoors-v11
# mapbox://styles/mapbox/light-v10
# mapbox://styles/mapbox/dark-v10
# mapbox://styles/mapbox/satellite-v9
# mapbox://styles/mapbox/satellite-streets-v11

# make map ####
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJ",
                 "jamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0",
                 ".sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- paste0("https://api.mapbox.com/styles/",
                  "v1/mapbox/outdoors-v11/tiles/256/{z}/{x}/{y}")
SV_USTB.osm <- openmap(c(-31.9296, 115.8471), 
                       c(-31.9348, 115.8518),
                       type=paste0(baseUrl,apiKey), zoom=17)
SV_USTB.utm <- openproj(SV_USTB.osm, 
                        projection = "+proj=utm +zone=50 +south")
# _._._._._._._._._._._._._._._._._._._._._._
#
par(mfrow = c(1,1), mar = c(3.5,3.5,1,1), 
    mgp=c(1.7,0.3,0), font.lab = 2, 
    lend = "square", ljoin = "mitre")
layout(cbind(1,1,2,2,3))
plot(SV_USTB.utm, removeMargin = FALSE)
box()
axis(1, cex.axis=1)
mtext(side=1, line=1.7, text="Easting (UTM Zone 50, m)",
      font=2, cex=1.1)
axis(2, cex.axis=1)
mtext(side=2, line=1.7, text="Northing (UTM Zone 50, m)",
      font=2, cex=1.1)
image(kc.sv17soil.pH, loc = pred.grid.sv, 
      values=kc.sv17soil.pH$pred, add=T, 
      xlab = "UTM Zone 50 Easting (m)", 
      ylab = "UTM Zone 50 Northing (m)", 
      cex.lab=1.4, cex.axis=1.5,
      col = colorRampPalette(c("#FFC0C0", "#D0D000", "#0000C0"), 
                             space="Lab")(21))
lines(geo.sv17soil.pH$borders, col = "#D2CE39", lwd = 2)
mtext("(a)",3,-1.7, adj = 0.02, cex = 1.4, font = 2)
lines(c(391240,391415),c(6466530,6466534), lwd = 12, 
      col = "white")
lines(c(391240,391415,NA,391240,391415),
      c(6466536,6466539,NA,6466525,6466528), col = "grey90")
legend.krige(x.leg=c(391020,391270), y.leg=c(6466280,6466310), 
      val=kc.sv17soil.pH$pred,cex=1.2,
      col = colorRampPalette(c("#FFC0C0", "#D0D000", "#0000C0"), 
                             space="Lab")(21), 
      offset.leg=-0.5)
text(391020, 6466325, labels="Soil pH", pos = 4, 
     cex = 2, font =2)
# points(sv17soil$Northing ~ sv17soil$Easting, 
#   subset = sv17soil$pH <= quantile(sv17soil$pH, probs = 0.05),
#   col = "red", pch = 1, lwd = 3, cex = 1.5)
points(sv17soil$Northing ~ sv17soil$Easting, 
  subset = sv17soil$pH <= quantile(sv17soil$pH, probs = 0.05),
  col = 1, pch = 1, lwd = 1, cex = 2)
points(sv17soil$Northing ~ sv17soil$Easting, pch = 3, lwd = 2)
#
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#
plot(SV_USTB.utm, removeMargin = FALSE)
box()
axis(1, cex.axis=1)
mtext(side=1, line=1.7, text="Easting (UTM Zone 50, m)",
      font=2, cex=1.2)
axis(2, cex.axis=1)
mtext(side=2, line=1.7, text="Northing (UTM Zone 50, m)",
      font=2, cex=1.2)
image(kc.sv17soil.EC, loc = pred.grid.sv, 
      values=kc.sv17soil.EC$pred, add=T, 
      xlab = "UTM Zone 50 Easting (m)", 
      ylab = "UTM Zone 50 Northing (m)", 
      cex.lab=1.4, cex.axis=1.5,
      col = colorRampPalette(c("#C0C0FF", "#D0D000", "#C00000"), 
                             space="Lab")(21))
lines(geo.sv17soil.pH$borders, col = "#D3CC55", lwd = 2)
mtext("(b)",3,-1.7, adj = 0.02, cex = 1.4, font = 2)
lines(c(391240,391415),c(6466530,6466534), lwd = 12, 
      col = "white")
lines(c(391240,391415,NA,391240,391415),
      c(6466536,6466539,NA,6466525,6466528), col = "grey90")
legend.krige(x.leg=c(391020,391270), y.leg=c(6466280,6466310), 
             val=10^kc.sv17soil.EC$pred,cex=1.2,
             col = colorRampPalette(c("#C0C0FF", "#D0D000", 
                                      "#C00000"), 
                                    space="Lab")(21), 
             offset.leg=-0.5)
text(391020, 6466325, labels="Soil EC (\u00B5S/cm)", 
     pos = 4, cex = 2, font = 2)
# points(sv17soil$Northing ~ sv17soil$Easting, 
#        subset = sv17soil$EC >= 
#          quantile(sv17soil$EC, probs = 0.95, na.rm = TRUE),
#        col = "red", pch = 1, lwd = 3, cex = 1.5)
points(sv17soil$Northing ~ sv17soil$Easting, 
       subset = sv17soil$EC >= 
         quantile(sv17soil$EC, probs = 0.95, na.rm = TRUE),
       col = 1, pch = 1, lwd = 1, cex = 2)
points(sv17soil$Northing ~ sv17soil$Easting, pch = 3, lwd = 2)
#
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#
x3 <- c(par("usr")[1],
        par("usr")[1]+((par("usr")[2]-par("usr")[1])/2))
y3 <- c(par("usr")[3], par("usr")[4])
plot(x3, y3, type = "n", bty = "n", xaxt = "n", yaxt = "n", 
     ann = FALSE, xlim = x3, ylim = y3)
addnortharrow(padin = c(0,0), pos="topleft")
addscalebar(plotepsg = 32750, pos="topleft", padin = c(0,1),
            label.cex = 2, htin = 0.125, widthhint = 0.45)
legend(390980,6466440, 
       legend=c("Samples", 
                " >95% CI", 
                "Boundary"), 
       cex = 2, horiz = F, pch = c(3,1,NA), 
       pt.cex=c(1.,2,.01), lwd = c(NA,NA,2), 
       lty = c(NA,NA,1), pt.lwd = c(2,1,NA), 
       col=c(1,1,"#D2CE39"), bty="n", 
       text.col = 1, inset = 0.03, 
       x.intersp = 0.6, y.intersp = 1.2,
       seg.len = 1.2)
legend(390980,6466440, legend=rep("\u00A0", 3), 
       cex=2, col=c(11,1,11), 
       horiz = F, pch = c(NA,3,NA), pt.cex = c(1,1,NA), 
       lwd = NA, lty = NA, pt.lwd = c(NA,2,NA), 
       bty = "n", text.col = 1, inset = 0.03, 
       x.intersp = 0.6, y.intersp = 1.2,
       seg.len = 1.2)
inset <- readPNG("Aust_map_inset2.png")
addImg(inset, x = 391100, y = 6466550, width = 200)
rm(inset)
#
# end code