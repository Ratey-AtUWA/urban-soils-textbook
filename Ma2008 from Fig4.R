# Ma2008 <- read.csv("Ma2008.csv")
# Ma2008$C_type <- as.factor(Ma2008$C_type)

temp <- Ma2008[,3:4]
colnames(temp) <- colnames(Ma2008)[1:2]
data0 <- rbind(Ma2008[,1:2], temp)
data0$Ctype <- factor(c(rep("TOC",24),rep("BC",24)), 
                      levels = c("TOC","BC"))
require (car)
carPalette(palette = c("black","chocolate","blue3","purple",
             "darkcyan","red3","darkorange","gray"))
palette(c("black","chocolate","blue3","purple",
          "darkcyan","red3","darkorange","gray"))
par(mar = c(4,4,1,1), mgp = c(1.7, 0.3, 0), tcl = 0.3, 
    font.lab = 2, cex.lab = 1.2, cex.axis=1.2, lwd=2,
    lend="square", ljoin="mitre", 
    las = 0, xaxs = "i", yaxs = "i")
scatterplot(Ma2008$Sum_PAH ~ Ma2008$C_mgkg | Ma2008$C_type,
     xlim = c(0,23), ylim = c(0,400), smooth = FALSE,
     pch = c(15,1), col = c(1, "chocolate4"),
     xlab = "Carbon fraction content", cex = 1.4, 
     ylab = expression(bold(paste(Sigma[16],"PAH"))),
     legend = list(coords="bottomright", title = "Carbon Type",
                   cex =1.3, legend = c("Total carbon", "Black carbon")))
summBC <- summary(lm(Sum_PAH ~ C_mgkg, data = Ma2008, subset = Ma2008$C_type == "Black carbon"))
summTOC <- summary(lm(Sum_PAH ~ C_mgkg, data = Ma2008, subset = Ma2008$C_type == "Total carbon"))
text(6.2,180, pos = 2, cex = 1.2,
     labels = paste("R\u00B2 =", round(summBC$r.squared, 3)))
text(17.5,280, pos = 4, col = "chocolate4", cex = 1.2,
     labels = paste("R\u00B2 =", signif(summTOC$r.squared, 3)))
#
rm(list = c("summTOC","summBC"))
