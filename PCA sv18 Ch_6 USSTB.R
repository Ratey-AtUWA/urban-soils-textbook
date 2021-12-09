mregdata <- na.omit(sv18soil[,c("Group","Sample","As.log","pH","EC.log",
           "Al.log","Ca.log","Fe.log","K.log","Mn.log",
           "Na.log","P.log","S.log","Depth_mean")])
attach(mregdata)
scatter3d(append(Fe.log, c(1.5, 5.3)), 
          append(S.log, c(0.68, 3.93)), 
          append(As.log, c(-1.2, 2.14)), 
      data = mregdata,
      bg.col = "white",
      surface = FALSE, axis.col = c("black", "black", "black"),
      ellipsoid = TRUE, level = 0.95, ellipsoid.alpha = 0.15,
      surface.col = "grey40", surface.alpha = 0.25,
      sphere.size = 0.75, point.col = "black",
      xlab = "Fe",
      ylab = "S", 
      zlab = "As")
summary(sv18[,1:40])
require(rgr)
otherdata <- na.omit(sv18[,c("Group", "Type","pH","EC.log",
                             "Al", "As", "Ba", "Ca", "Ce", "Cr", "Fe", 
                             "K",  "La", "Mg", "Mn", "Na", "P", 
                             "Pb", "S",  "Sr", "Th", "V", "Y","Zn")])
NROW(otherdata)
clrdata <- clr(na.omit(otherdata[,c("Al", "As", "Ba", "Ca", "Ce", "Cr", "Fe", 
                       "K",  "La", "Mg", "Mn", "Na", "P", 
                       "Pb", "S",  "Sr", "Th", "V", "Y","Zn")]))
NROW(clrdata)
pcadata <- cbind(otherdata[,c("Group", "Type","pH","EC.log")], clrdata)
NROW(pcadata)
View(pcadata)
#
pca_sv18 <- prcomp(pcadata[,3:24], scale = TRUE, center = TRUE, retx = TRUE)
summary(pca_sv18)
par(mar = c(4,4,1,1), mgp = c(2, 0.6, 0), fomt.lab = 2, cex.lab = 1.4, cex.axis = 1.25, 
    lend = "square", ljoin = "mitre")
plot(pca_sv18, xlab = "Principal Component", main = "", ylim = c(0, 6))
axis(1, at  = seq(0.7,11.5, length.out = 10), labels = seq(1,10))
# box(which = "figure")
lines(c(par("usr")[1],par("usr")[2]), rep(par("usr")[3], 2))
lines(c(par("usr")[1],par("usr")[1]), c(par("usr")[3], par("usr")[4]))
#
par(mar = c(4,4,4,4), mgp = c(2, 0.6, 0), fomt.lab = 2, cex.lab = 1.4, cex.axis = 1.25, 
    lend = "square", ljoin = "mitre")
palette(c("black","red3","dodgerblue", "purple4", "gold3","green3", "darkorange", "gray50","white","transparent"))
sf0 <- 1.
biplot(pca_sv18, scale = sf0, 
       col = c(10,1,2), cex = c(0.65, 1.4), 
       xlab = "", ylab = "")
mtext("PC1 Variable Weightings", 1, 2, font = 2, cex = 1.6)
mtext("PC2 Variable Weightings", 2, 2, font = 2, cex = 1.6)
mtext("PC1 Observation Scores", 3, 2, font = 2, cex = 1.6)
mtext("PC2 Observation Scores", 4, 2, font = 2, cex = 1.6)
sfx <- 1.35
sfy <- 1.58
points(pca_sv18$x[,1]*sfx, pca_sv18$x[,2]*sfy, 
       col = c(5, 3, 4)[pcadata$Type],
       pch = c(15, 1, 17)[pcadata$Type],
       lwd = c(1, 4, 1)[pcadata$Type], 
       cex = c(1.80, 2.16, 1.80)[pcadata$Type])
legend("topleft", legend = levels(pcadata$Type),
       cex = 1.4, bty = "n", inset = 0.0,
       col = c(5, 3, 4),
       pch = c(15, 1, 17),
       pt.lwd = c(1, 4, 1), 
       pt.cex = c(1.80, 2.16, 1.80))
