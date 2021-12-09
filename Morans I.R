require(lctools)
require(geoR)
require(OpenStreetMap)
require(prettymapr)
require(knitr)
# styles are: mapbox://styles/mapbox/streets-v11
# mapbox://styles/mapbox/outdoors-v11
# mapbox://styles/mapbox/light-v10
# mapbox://styles/mapbox/dark-v10
# mapbox://styles/mapbox/satellite-v9
# mapbox://styles/mapbox/satellite-streets-v11
apiKey <- paste0("?access_token=",
                 "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}"
SV.mapbox.osm <- openmap(c(-31.9296, 115.8472), c(-31.9348, 115.852),
                  type=paste0(baseUrl,apiKey),
                  minNumTiles = 12) #, zoom=16.5 
SV.utm <- openproj(SV.mapbox.osm, projection="+proj=utm +zone=50 +south")
rm(SV.mapbox.osm)
plot(SV.utm, removeMargin = FALSE)

# moran's i using lctools package ####
data_temp <- na.omit(sv17soil[,c("Easting", "Northing", "EC")])
Coords <- cbind(data_temp$Easting, data_temp$Northing)
bw <- 5
mI <- moransI(Coords, bw, data_temp$EC)
moran.table <- matrix(data=NA, nrow=1, ncol=6)
col.names <- c("Moran's I", "Expected I", "Z resampling", "P-value resampling",
               "Z randomization", "P-value randomization")
colnames(moran.table) <- col.names
moran.table[1,1] <- mI$Morans.I
moran.table[1,2] <- mI$Expected.I
moran.table[1,3] <- mI$z.resampling
moran.table[1,4] <- mI$p.value.resampling
moran.table[1,5] <- mI$z.randomization
moran.table[1,6] <- mI$p.value.randomization
t(moran.table)
rm(moran.table)
# -=-=-=-=-=-=-=-=-=-=-=-=-
local_moran_sv17soil_EC <- l.moransI(Coords,bw,data_temp$EC)
plotdata <- as.data.frame(cbind(Coords[,1], Coords[,2], 
                                local_moran_sv17soil_EC$Ii, 
                                local_moran_sv17soil_EC$p.value))
colnames(plotdata) <- c("Easting", "Northing", "MoranI", "p_value")
head(plotdata)
pos0 <- subset(plotdata, plotdata$MoranI>0)
neg0 <- subset(plotdata, plotdata$MoranI<0)
#
sf <- 1
palette(c("black","red3","blue3","orange","skyblue",
          "#80000080","#00008080","#FFA50080","#87CEEB80","white"))
# png(file = "Morans_I_sv17soil_EC.png", height = 691 * sf, width = 660 * sf)
par(mar=c(4,4,1,1)*sf, mgp=c(2, 0.7, 0)*sf, font.lab = 2, 
    lend = "square", tcl = 0.3*sf, lwd = sf)
plot(SV.utm, removeMargin = FALSE)
axis(1, mgp=c(2, 0.7, 0)*sf, cex.axis = 1.3*sf)
mtext("Easting (UTM Zone 50, m)", 1, 2*sf, font = 2, cex = 1.6*sf)
axis(2, mgp=c(2, 0.5, 0)*sf, cex.axis = 1.4*sf, 
     at = seq(6466300, 6466700, 100), 
     labels = seq(6466300, 6466700, 100))
mtext("Northing (UTM Zone 50, m)", 2, 2*sf, font = 2, cex = 1.6*sf)
box()
with(pos0, symbols(Easting, Northing, circles = sqrt(MoranI*30), 
                   fg = 4, bg = 8, lwd = 2, inches = F, add= TRUE))
with(neg0, symbols(Easting, Northing, squares = sqrt(MoranI*-60), 
                   fg = 5, bg = 9, lwd = 2, inches = F, add= TRUE))
rect(391030, 6466225, 391180, 6466445, 
     col = "#000000B0", border = 1, lwd = 2*sf)
symbols(c(391050, 391050), c(6466320, 6466290), 
        circles = sqrt(c(5, 1)*30), lwd = 2, inches = F, 
        fg = 4, bg = 8, add= TRUE)
text(c(391025, 391050, 391050), c(6466410, 6466320, 6466290),
     labels = c("Local Moran's I\nfor soil EC", "5", "1"), 
     cex = 1.2*sf, offset = 1.3*sf, pos = 4, col = 10)
text(391025, 6466360,
     labels = paste("Based on", bw, "nearest\nneighbour points"), 
     cex = 1.0*sf, offset = 1.3*sf, pos = 4, col = 10)
legend("bottomleft", bty = "n", inset = 0.03, cex = 1.2*sf,
       legend = c("Positive I", "Negative I"),
       pch = c(21, 22), pt.cex = c(2, 1.5)*sf, pt.lwd=2,
       col = c(4, 5), pt.bg = c(8, 9), text.col = 10,
       box.col = "white", box.lwd = 1.3*sf, bg = "#c8d0c8")
# dev.off()
rm(list = c("data_temp", "plotdata", "pos0", "neg0"))
# ___________________________________________________________________________
# moran's i using ape package ####
# generate distance matrix
sv17soil.dists <- as.matrix(dist(cbind(sv17soil$Easting, sv17soil$Northing)))
# take inverse of the matrix values
sv17soil.dists.inv <- 1/sv17soil.dists
# replace the diagonal entries with zero
diag(sv17soil.dists.inv) <- 0
# look at the matrix
head(sv17soil.dists.inv[,1:6])
#
# calculate moran’s I
Moran.I(sv17soil$EC, sv17soil.dists.inv)
rm(list = c("sv17soil.dists", "sv17soil.dists.inv"))
