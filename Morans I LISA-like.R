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
# apiKey <- paste0("?access_token=",
#                  "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
# baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}"
# SV.mapbox.osm <- openmap(c(-31.9296, 115.8472), c(-31.9348, 115.852),
#                   type=paste0(baseUrl,apiKey),
#                   minNumTiles = 12) #, zoom=16.5 
# SV.utm <- openproj(SV.mapbox.osm, projection="+proj=utm +zone=50 +south")
# rm(SV.mapbox.osm)
# plot(SV.utm, removeMargin = FALSE)

# moran's i using lctools package ####
data_temp <- na.omit(sv17soil[,c("Easting", "Northing", "IPI")])
data_temp$IPI <- log10(data_temp$IPI) # comment this line if not needed
Coords <- cbind(data_temp$Easting, data_temp$Northing)

# check moran's i for different bandwidths
moran.table <- as.data.frame(matrix(data=NA, nrow=9, ncol=7))
col.names <- c("bw","Moran's I", "Expected I", "Z_resamp", "P_resamp",
               "Z_randomiz", "P_randomiz")
colnames(moran.table) <- col.names
for(bw in 2:10){
  mI <- moransI(Coords, bw, data_temp$IPI)
  moran.table[bw-1, ] <- c(bw, mI$Morans.I, mI$Expected.I, mI$z.resampling, mI$p.value.resampling,
                        mI$z.randomization, mI$p.value.randomization)
}
moran.table$Signif <- cut(moran.table$P_resamp, breaks = c(0,0.001,0.01,0.05,1), 
                          labels = c("***","** ","*  ","ns "))
print(moran.table)
rm(moran.table)

bw <- 4
# -=-=-=-=-=-=-=-=-=-=-=-=-
locMI_sv17soil_IPI <- l.moransI(Coords,bw,data_temp$IPI)
plotdata <- as.data.frame(cbind(Coords[,1:2], data_temp$IPI, 
                                locMI_sv17soil_IPI$Ii, 
                                locMI_sv17soil_IPI$p.value))
colnames(plotdata) <- c("Easting", "Northing", "IPI", "MoranI", "p_value") # ;head(plotdata)
medIPI <- median(plotdata$IPI, na.rm = TRUE)
HiHi <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI>0&plotdata$IPI>=medIPI)
LoLo <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI>0&plotdata$IPI<medIPI)
HiLo <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI<0&plotdata$IPI>=medIPI)
LoHi <- subset(plotdata, plotdata$p_value<=0.05&plotdata$MoranI<0&plotdata$IPI<medIPI)
#
sf <- 1
palette(c("black","red3","blue3","orange","skyblue",
          "#FF8080","#8080FF","#FFA50040","#87CEEB40","white"))
# png(file = "Morans_I_sv17soil_IPI.png", height = 691 * sf, width = 660 * sf)
par(mar=c(3,3,1,1)*sf, mgp=c(2, 0.7, 0)*sf, font.lab = 2, 
    lend = "square", ljoin = "mitre", tcl = 0.3*sf, lwd = sf)
plot(SV.mapboxO.utm, removeMargin = FALSE)
axis(1, mgp=c(2, 0.35, 0)*sf, cex.axis = 1.1*sf)
mtext("Easting (UTM Zone 50, m)", 1, 1.7*sf, font = 2, cex = 1.25*sf)
axis(2, mgp=c(2, 0.3, 0)*sf, cex.axis = 1.1*sf, 
     at = seq(6466300, 6466700, 100), 
     labels = seq(6466300, 6466700, 100))
mtext("Northing (UTM Zone 50, m)", 2, 1.7*sf, font = 2, cex = 1.25*sf)
rect(391334,6466457,391374,6466469, col = "#75CFEF", border = NA)
rect(391374,6466457,391400,6466469, col = "#B5E59E", border = NA)
text(391355,6466464,labels="Smith's\nLake",col="skyblue4", font=4, cex=0.9)
text(391274,6466495,labels="Electrical\nsubstation",col="grey33", font=3, cex=0.8)
text(391535,6466480,labels="Fitzgerald Street",col="grey33", font=2, srt = 270, cex=0.9)
box()
with(HiHi, points(Northing ~ Easting, pch = 22,
                   col = 2, bg = 6, lwd = 2, cex = 1.8))
with(LoLo, points(Northing ~ Easting, pch = 21, 
                   col = 3, bg = 7, lwd = 2, cex = 2))
with(HiLo, points(Northing ~ Easting, pch = 25,  
                   col = 2, bg = 7, lwd = 3, cex = 1.4))
with(LoHi, points(Northing ~ Easting, pch = 24,
                   col = 3, bg = 6, lwd = 3, cex = 1.4))
rect(390930, 6466175, 391110, 6466480,
     col = "#F0F0F0", border = 10, lwd = 2*sf)
text(390930, 6466450, font = 2,
     labels = "Local Moran's I\nfor soil IPI",
     cex = 1.*sf, offset = 0.3*sf, pos = 4, col = 1)
mI <- moransI(Coords, bw, data_temp$IPI)
text(390930, 6466385,
     labels = paste("Based on", bw, "nearest\nneighbour points",
                "\nOnly plotting points\nwith p \u2264 0.05"),
     cex = 1.*sf, offset = 0.3*sf, pos = 4, col = 1, font = 3)
text(390930, 6466340,
     labels = paste("Global Moran's I =", round(mI$Morans.I,3)),
                cex = 1.*sf, offset = 0.3*sf, pos = 4, col = 1)
legend("bottomleft", bty = "n", inset = 0.03, cex = 1.2*sf,
       legend = c("High-High", "Low-Low","High-Low","Low-High"),
       pch = c(22, 21, 25,24), pt.cex = c(1.8,2,1.4,1.4)*sf, 
       pt.lwd=c(2,2,3,3),
       col = c(2,3,2,3), pt.bg = c(6,7,7,6), text.col = 1,
       box.col = 10, box.lwd = 1.3*sf, bg = "#E0E0E0",
       y.intersp = 1.4)
names(sv17soil)
# dev.off()
rm(list = c("data_temp", "plotdata", "HiHi", "LoLo", "HiLo", "LoHi"))
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
# calculate moran's I
Moran.I(sv17soil$IPI, sv17soil.dists.inv)
rm(list = c("sv17soil.dists", "sv17soil.dists.inv"))
