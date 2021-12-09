# rafique2020 <- read.table('clipboard', header = TRUE, sep = "\t")
str(rafique2020)
rafique2020$Sample <- as.character(rafique2020$Sample)
rafique2020$Location <- as.character(rafique2020$Location)

barplot(rafique2020$Total, names.arg = rafique2020$Landuse, horiz = FALSE, las=2)

par(mfrow = c(1,1), mar = c(9,4,1,1), mgp = c(2.9,0.4,0), tcl = 0.2, font.lab = 2,
    lend = "square", ljoin = "mitre")
palette(c("black", "olivedrab3","cyan","lightcoral","grey",
          "palegreen","forestgreen","steelblue2","tan","white"))
boxplot(log10(rafique2020$Total) ~ rafique2020$Landuse, las = 2,
        xlab = "", ylab = "Total microplastics (particles/kg)", 
        ylim = log10(c(1500,12000)), xaxt = "n", yaxt = "n",
        col = seq(2,9), lwd =2, cex.lab = 1.2)
axis(2, at = log10(c(1000,2000,4000,6000,8000,10000)), 
     labels = c(1000,2000,4000,6000,8000,10000), las = 2)
axis(1, at = seq(1,nlevels(rafique2020$Landuse)), 
     labels = levels(rafique2020$Landuse), 
     las = 2, font.axis = 2, cex.axis = 1.2)
meanz <- tapply(log10(rafique2020$Total), rafique2020$Landuse, mean)
lines(seq(1,8), as.numeric(meanz), type = "l", pch = 3, lwd = 2, col = 10)
points(seq(1,8), as.numeric(meanz), type = "p", pch = 3, lwd = 4, col = 10)
require (Rmisc)
ci0 <- group.CI(log10(rafique2020$Total) ~ rafique2020$Landuse, data = rafique2020)
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,2], 
       col=10, angle=90, length=0.08, lwd = 3) # optional
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,2], 
       col=1, angle=90, length=0.1, lwd=1, lty = 3) # optional
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,4], 
       col=10, angle=90, length=0.08, lwd = 3) # optional
arrows(x0=seq(1,NROW(ci0)), y0=ci0[,3], y1=ci0[,4], 
       col=1, angle=90, length=0.1, lwd=1, lty = 3) # optional
points(seq(1,8), as.numeric(meanz), type = "b", pch = 3, lwd = 2, lty = 3)

data0 <- rbind(tapply(rafique2020[,4], rafique2020[,3], mean),
               tapply(rafique2020[,5], rafique2020[,3], mean),
               tapply(rafique2020[,6], rafique2020[,3], mean),
               tapply(rafique2020[,7], rafique2020[,3], mean),
               tapply(rafique2020[,8], rafique2020[,3], mean))
row.names(data0) <- names(rafique2020)[4:8]
par(mar = c(7,5,1,1), mgp = c(1.7,0.3,0), tcl = 0.25)
barplot(data0, horiz = FALSE, las=2, ylim = c(0,11200), 
        col = c("darkblue","red3","steelblue","thistle","grey95"))
mtext("Total microplastics (particles/kg)", 2, 3, font =2, cex = 1.2)
require(Rmisc)
ci0 <- group.CI(rafique2020$Total ~ rafique2020$Landuse, data = rafique2020)
arrows(x0=seq(0.7,par("usr")[2]-0.9, length.out = 8), y0=ci0[,3], y1=ci0[,2], 
       col=1, angle=90, length=0.1, lwd=1, lty = 1) # optional
legend("topleft", bty = "n", legend = rev(row.names(data0)), pch = 22,
       pt.bg = rev(c("darkblue","red3","steelblue","thistle","grey95")),
       pt.cex = 2, cex = 1.2)
abline(h=0)
box()
rm(list = c("data0","ci0"))
