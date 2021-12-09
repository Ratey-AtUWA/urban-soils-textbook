wei2014 <- read.table("clipboard", header = T, sep = "\t")
wei2014
wei2014$Hb..Pa.m3.mol. <- NULL
names(wei2014)[2] <- "MW"
plot(wei2014$Flux ~ wei2014$log.Kow)
wei2014$logNeg <- log10(-1*wei2014$Flux)

barplot(wei2014$logPos ~ wei2014$Analyte, xaxt="n", yaxt="n")
barplot(wei2014$logNeg ~ wei2014$Analyte, ylim = c(2,-2), yaxt="n")


wei <- wei2014[order(wei2014$log.Kow),] 

par(mfrow=c(2,1), mar = c(0,4,0.1,1), oma = c(4,0,1,0), 
    mgp = c(1.6,0.3, 0), tcl = 0.3, las = 1, font.lab = 2)
plot(wei$logPos ~ wei$log.Kow, 
     ylab="",
     type = "p", 
     xlim = c(3.8,6.9), 
     xaxt="n", yaxt="n", 
     bty = "n",
     col = "darkred", pch = 15)
arrows(wei$log.Kow, rep(-2.1,15), wei$log.Kow, wei$logPos, 
       col  = "darkred", length = 0)
text(wei$log.Kow, wei$logPos, labels = wei$Analyte, 
     pos = c(2,4,4,4,4,2,4,3,3,3,3,3,3,3,3), col = "darkred")
mtext("(μg m⁻² yr⁻¹)", 2, 2.2, font = 2, adj = 0, las = 0)
mtext("Positive fluxes (soil = source)", 3, -1.5, font = 2, col = "darkred", adj = 0.95)
axis(2, at = seq(-2,3,1), labels = c(0.01, 0.1, 1, 10, 100, 1000), 
     col = "darkred", col.ticks = "darkred", col.axis = "darkred")
axis(2, at = c(log10(seq(0.2,0.9,0.1)), log10(seq(2,9,1))), 
     labels = rep("",16), tcl = 0.15, 
     col = "darkred", col.ticks = "darkred", col.axis = "darkred")
abline(h = par("usr")[3], col = "grey")
plot(wei$logNeg ~ wei$log.Kow,
     type = "p", 
     xlim = c(3.8,6.9), ylim = c(1,-2), 
     yaxt="n", bty = "n",
     xlab = expression(logK[OW]),
     ylab = "",
     col = "darkblue", pch = 15)
arrows(wei$log.Kow, rep(-2.1,15), wei$log.Kow, wei$logNeg, 
       col  = "darkblue", length = 0)
text(wei$log.Kow, wei$logNeg, labels = wei$Analyte, 
     pos = c(1,4,1,4,4,1,4,2,1,3,2,1,1,4,1), col = "darkblue")
mtext("Soil to air flux ", 2, 2.2, font = 2, adj = 1, las = 0)
mtext("Negative fluxes (soil = sink)", 1, -1.5, font = 2, col = "darkblue", adj = 0.05)
axis(2, at = seq(-2,1,1), labels = c(-0.01, -0.1, -1, -10), 
     col = "darkblue", col.ticks = "darkblue", col.axis = "darkblue")
axis(2, at = c(log10(seq(0.02,0.09,0.01)), log10(seq(0.2,0.9,0.1)), log10(seq(2,9,1))), 
     labels = rep("",24), tcl = 0.15, 
     col = "darkblue", col.ticks = "darkblue", col.axis = "darkblue")
abline(h = par("usr")[4], col = "grey")
mtext(expression(paste(bold(log[10]),bolditalic(K[OW]))), 1,1.6, font=2)
