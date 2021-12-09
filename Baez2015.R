# Baez2015 <- read.csv("Baez2015.csv")
# Baez2015$Type <- factor(Baez2015$Type, 
#                         levels = c("Soil","Property","Contaminant"))
par(mfrow = c(1, 2),
  mar = c(3, 3, 1, 1), mgp = c(1.5, 0.2, 0), 
    tcl = 0.3, font.lab = 2, 
    lend = "square", ljoin = "mitre")

plot(Baez2015$PC2a ~ Baez2015$PC1a,
     type = "n",
     xaxt = "n", xaxs="i", xlim = c(-3,5),
     yaxt = "n", yaxs="i", ylim = c(-3,3),
     xlab = "Component 1",
     ylab = "Component 2")
axis(1, at = seq(-3,5,1), labels = seq(-3,5,1))
axis(2, at = seq(-3,3,1), labels = seq(-3,3,1))
abline(h=0, col = "grey50")
abline(v=0, col = "grey50")
points(Baez2015$PC2a ~ Baez2015$PC1a,
     pch = c(22,15,15)[Baez2015$Type],
     col = c("black","sienna","red")[Baez2015$Type],
     cex = 1.5, lwd = 2, bg = "white")
text(Baez2015$PC1a[1:10], Baez2015$PC2a[1:10],
     labels = seq(1,10), 
     pos = c(4,4,1,4,4,4,1,1,1,4))
text(Baez2015$PC1a[11], Baez2015$PC2a[11],
     labels = expression("Al"["Ox"]), 
     pos = 4)
text(Baez2015$PC1a[c(12:13,16:17)], Baez2015$PC2a[c(12:13,16:17)],
     labels = c("CEC","Clay %","OC","pH"), 
     pos = c(4,4,2,4,4,4,4))
text(Baez2015$PC1a[14], Baez2015$PC2a[14],
     labels = expression("Fe"["Ox"]), 
     pos = 4)
text(Baez2015$PC1a[15], Baez2015$PC2a[15],
     labels = expression(paste("H",scriptstyle(atop("+  ","ext")))), 
     pos = 4, cex = 1.2)
box(lwd = 2)
# > as.character(Baez2015$Value)
# [1] "1"      "2"      "3"      "4"      "5"      "6"      "7"      "8"      "9"     
# [10] "10"     "AlOx"   "CEC"    "Clay %" "FeOx"   "Hext"   "OC"     "pH"     "MSM"   
# [19] "IMHP"   "TCP"
plot(Baez2015$PC2d ~ Baez2015$PC1d,
     type = "n",
     xaxt = "n", xaxs="i", xlim = c(-3,6),
     yaxt = "n", yaxs="i", ylim = c(-3,3),
     xlab = "Component 1",
     ylab = "Component 2")
axis(1, at = seq(-3,6,1), labels = seq(-3,6,1))
axis(2, at = seq(-3,3,1), labels = seq(-3,3,1))
abline(h=0, col = "grey50")
abline(v=0, col = "grey50")
points(Baez2015$PC2d ~ Baez2015$PC1d,
     pch = c(22,15,15)[Baez2015$Type],
     col = c("black","sienna","red")[Baez2015$Type],
     cex = 1.5, lwd = 2, bg = "white")
points(Baez2015$PC2d ~ Baez2015$PC1d,
       subset = Baez2015$Type == "Contaminant",
       pch = 4, col = "white", lwd = 2, cex = 0.9)
text(Baez2015$PC1d[1:10], Baez2015$PC2d[1:10],
     labels = seq(1,10), 
     pos = c(2,4,1,4,4,4,1,1,1,4))
text(Baez2015$PC1d[11], Baez2015$PC2d[11],
     labels = expression("Al"["Ox"]), 
     pos = 4)
text(Baez2015$PC1d[c(12:13,16:17)], Baez2015$PC2d[c(12:13,16:17)],
     labels = c("CEC","Clay %","OC","pH"), 
     pos = c(4,4,2,4))
text(Baez2015$PC1d[14], Baez2015$PC2d[14],
     labels = expression("Fe"["Ox"]), 
     pos = 2)
text(Baez2015$PC1d[15], Baez2015$PC2d[15],
     labels = expression(paste("H",scriptstyle(atop("+  ","ext")))), 
     pos = 4, cex = 1.2)
text(Baez2015$PC1d[c(18:20)], Baez2015$PC2d[c(18:20)],
     labels = Baez2015$Value[18:20], 
     pos = c(4,3,4))
box(lwd = 2)
