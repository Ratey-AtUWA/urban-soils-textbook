# Ma2008 <- read.csv("Ma2008.csv")
# Ma2008$C_type <- as.factor(Ma2008$C_type)

palette(c("black","chocolate","blue3","purple",
          "darkcyan","red3","darkorange","gray"))
par(mfrow = c(1,2), mar = c(4,4,1,1), mgp = c(1.7, 0.3, 0), 
    tcl = 0.3, font.lab = 2, cex.lab = 1.2, cex.axis=1.2, 
    lwd=2, lend="square", ljoin="mitre", las = 1)
plot(Ma2008$BaA_228 ~ Ma2008$Flu_202, type = "n", 
     xlim = c(0.46, 0.74), ylim = c(0.1, 0.4),
     xlab = "FLU/(FLU + PYR)", ylab = "", xaxt = "n", yaxt = "n")
axis(1, at = seq(0.5, 0.7, 0.1), labels = seq(0.5, 0.7, 0.1))
axis(2, at = seq(0.1, 0.4, 0.1), labels = seq(0.1, 0.4, 0.1))
mtext("BaA/(BaA + CHR)",2,2.2, font = 2, cex = 1.2, las = 0, col = "blue3")
abline(v = 0.5, col = "grey", lty = 3)
abline(h = 0.2, col = "grey", lty = 3)
points(Ma2008$BaA_228 ~ Ma2008$Flu_202, pch = 15)
arrows(0.7,0.38,0.7,0.4, col = "blue3", length = 0.1)
arrows(0.7,0.355,0.7,0.2, col = "blue3", length = 0.1)
arrows(0.71,0.165,0.71,0.2, col = "blue3", length = 0.1)
arrows(0.71,0.14,0.71,0.1, col = "blue3", length = 0.1)
arrows(0.52,0.12,0.57,0.12, col = "grey", length = 0.1)
text(0.66, 0.37, labels = "petroleum & combustion", col = "blue3")
text(0.705, 0.155, labels = "petroleum", col = "blue3")
text(0.508, 0.22, labels = "wood and coal combustion", srt = 90)

plot(Ma2008$IP_276 ~ Ma2008$Flu_202, type = "n", 
     xlim = c(0.46, 0.74), ylim = c(0.3, 0.7),
     xlab = "FLU/(FLU + PYR)", ylab = "", xaxt = "n")
axis(1, at = seq(0.5, 0.7, 0.1), labels = seq(0.5, 0.7, 0.1))
mtext("IcdP/(IcdP + BghiP)",2,2.2, font = 2, cex = 1.2, las = 0, col = "red3")
abline(v = 0.5, col = "grey", lty = 3)
abline(h = 0.45, col = "grey", lty = 3)
points(Ma2008$IP_276 ~ Ma2008$Flu_202, pch = 15)
arrows(0.7,0.63,0.7,0.7, col = "red3", length = 0.1)
arrows(0.7,0.6,0.7,0.45, col = "red3", length = 0.1)
arrows(0.72,0.35,0.72,0.45, col = "red3", length = 0.1)
arrows(0.72,0.32,0.72,0.3, col = "red3", length = 0.1)
arrows(0.52,0.57,0.57,0.57, col = "grey", length = 0.1)
text(0.65, 0.62, labels = "wood and coal combustion", col = "red3")
text(0.74, 0.34, pos = 2, labels = "combustion of liquid fossil fuel", col = "red3")
text(0.508, 0.47, labels = "wood and coal combustion", srt = 90)
