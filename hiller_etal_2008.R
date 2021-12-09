# hiller2008 <- read.csv("hiller2008.csv")
# str(hiller2008)
# hiller2008$logKD <- log10(hiller2008$KD)
par(mar = c(3,4,1,1), mgp = c(1.6,0.3,0), tcl = 0.3,
    cex.lab = 1.4, cex.axis = 1.4, font.lab = 2, 
    lend = "square", ljoin = "mitre")
palette(c("black", "blue2", "purple","red3", "gold4", "sienna", "grey50"))
with(hiller2008, 
     plot(logKD ~ SOC,
          pch = c(1,0,2,19,15,17)[Pesticide],
          col = c(2,3,4,5,6,7)[Pesticide],
          cex = c(1.8,1.4,1.4,1.6,1.4,1.4)[Pesticide],
          lwd = c(3,3,3,1,1,1)[Pesticide],
          lty = c(1,2,2,1,1,2),
          xlab = "Soil organic carbon (%)",
          xlim = c(0.4, 2.5),
          yaxt = "n", 
          ylab = "",
          ylim = c(-1, 3.2))
     )
mtext(expression(bolditalic(" K")[D]), 2, 2.5, cex = 1.4)
abline(h = log10(c(0.2, 0.5, 2,5,20,50,200)), col = "grey88", lty=3)
axis(2, at = log10(c(0.2, 0.5, 2,5,20,50,200)), 
     labels = c(0.2, 0.5, 2,5,20,50,200), las = 1)
linez <- c(1,2,2,1,1,2)
for (i in 1:nlevels(hiller2008$Pesticide)) {
  data <-
    subset(hiller2008,
           hiller2008$Pesticide == levels(hiller2008$Pesticide)[i])
  lm0 <- lm(data$logKD ~ data$SOC)
  cat(i, lm0$coefficients,"\n")
  x1 <- min(data$SOC, na.rm = T)
  x2 <- max(data$SOC, na.rm = T)
  a <- as.numeric(lm0$coefficients[1])
  b <- as.numeric(lm0$coefficients[2])
  lines(c(x1, x2), c(a + (b * x1), a + (b * x2)), col = i + 1, lty = linez[i])
}
legend("topleft", bty = "0", inset = 0.003, 
       box.col = "grey", ncol = 2,
       title = expression(italic("Pesticide")),
       legend = levels(hiller2008$Pesticide),
       pch = c(1,0,2,19,15,17),
       col = c(2,3,4,5,6,7),
       pt.cex = c(1.8,1.4,1.4,1.6,1.4,1.4),
       pt.lwd = c(3,3,3,1,1,1),
       lty = c(1,2,2,1,1,2),
       cex = 1.3, x.intersp = 0.6, y.intersp = 1.)
rm(list = c("data","linez", "lm0", "x1", "x2", "a", "b"))
