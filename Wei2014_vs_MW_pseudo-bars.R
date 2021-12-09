windowsFonts(nar = windowsFont("Arial Narrow"),
             tah = windowsFont("Tahoma"))
wei2014 <- read.table("clipboard", header = T, sep = "\t")
wei2014
wei2014$Hb..Pa.m3.mol. <- NULL
names(wei2014)[2] <- "MW"
plot(wei2014$Flux ~ wei2014$MW)
wei2014$logNeg <- log10(-1*wei2014$Flux)
wei2014$PAHs <- c("ACE(3)","ACY(3)","FLU(3)",
                  "PHE(3)","ANT(3)","FLA(4)","PYR(4)",
          "BaA(4)","CHR(4)","BbF(5)","BkF(5)",
          "BaP(5)","DahA(6)","IcdP(6)","BghiP(6)")

barplot(wei2014$logPos ~ wei2014$Analyte, 
        xaxt="n", yaxt="n")
barplot(wei2014$logNeg ~ wei2014$Analyte, 
        ylim = c(2,-2), yaxt="n")


wei <- wei2014[order(wei2014$MW),] 

par(mfrow=c(2,1), mar = c(0,4,0.1,1), oma = c(4,0,1,0), 
    mgp = c(1.6,0.3, 0), tcl = 0.3, las = 1, font.lab = 2,
    lend = "square", ljoin = "mitre")
plot(wei$logPos ~ wei$MW, 
     ylab="",
     type = "n", 
     xlim = c(130,300), 
     xaxt="n", yaxt="n", 
     bty = "n")
arrows(wei$MW, rep(-2.1,15), wei$MW, wei$logPos, 
       col  = "darkred", length = 0, lwd = 8)
points(wei$MW+1, wei$logPos,
     col = "darkred", pch = 150)
text(wei$MW, wei$logPos, labels = wei$PAHs, 
     pos = c(2,2,3,4,4,2,4,3,3,3,3,3,3,3,3), 
     col = "darkred", family = "tah")
mtext("(μg m⁻² yr⁻¹)", 2, 2.2, font = 2, adj = 0, las = 0)
mtext("Positive fluxes (soil \u2192 air)", 3, -2.5, 
      font = 2, col = "darkred", adj = 0.95)
axis(2, at = seq(-2,3,1), 
     labels = c(0.01, 0.1, 1, 10, 100, 1000), 
     col = "darkred", col.ticks = "darkred", 
     col.axis = "darkred", lwd.ticks = 2, lwd = 2)
axis(2, at = c(log10(seq(0.02,0.09,0.02)), 
               log10(seq(0.2,0.9,0.2)), 
               log10(seq(2,9,2)), 
               log10(seq(20,90,20)), 
               log10(seq(200,900,200))), 
     labels = rep("",20), tcl = 0.15, 
     col = "darkred", col.ticks = "darkred", 
     col.axis = "darkred")
abline(h = par("usr")[3], col = "grey")
plot(wei$logNeg ~ wei$MW,
     type = "p", 
     xlim = c(130,300), ylim = c(1,-2), 
     yaxt="n", bty = "n",
     xlab = expression(logK[OW]),
     ylab = "",
     col = "darkblue", bg = "white", pch = 22)
arrows(wei$MW, rep(-2.1,15), wei$MW, wei$logNeg, 
       col  = "darkblue", length = 0, lwd = 8)
points(wei$MW-1, wei$logNeg,
       col = "darkblue", pch = 150)
# points(wei$logNeg ~ wei$MW,
#      col = "darkblue", bg = "white", pch = 22, cex = 1.4, lwd = 2)
text(wei$MW, wei$logNeg, labels = wei$PAHs, 
     pos = c(1,4,1,4,4,1,4,2,2,3,2,2,2,2,4), 
     col = "darkblue", family = "tah")
mtext("Soil to air flux ", 2, 2.2, font = 2, adj = 1, las = 0)
mtext("Negative fluxes (air \u2192 soil)", 1, -2.5, 
      font = 2, col = "darkblue", adj = 0.05)
axis(2, at = seq(-2,1,1), labels = c(-0.01, -0.1, -1, -10), 
     col = "darkblue", col.ticks = "darkblue", 
     col.axis = "darkblue", lwd.ticks = 2, lwd = 2)
axis(2, at = c(log10(seq(0.02,0.09,0.02)), 
               log10(seq(0.2,0.9,0.2)), log10(seq(2,9,2))), 
     labels = rep("",12), tcl = 0.15, 
     col = "darkblue", col.ticks = "darkblue", 
     col.axis = "darkblue")
abline(h = par("usr")[4], col = "grey")
axis(1, lwd.ticks = 2, lwd = 2, at = c(130,150,200, 250,300), labels = rep("",5))
mtext("Molecular weight of PAH compound (g/mole)", 1,1.6, font=2)
