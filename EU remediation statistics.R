# EUremed <- read.table("clipboard", header = TRUE, sep = "\t", stringsAsFactors = F)
str(EUremed)
par(mar = c(4,12,1,1), las=1)
barplot(EUremed$InSituBiol, names.arg = EUremed$Country, horiz = T)

palette(c("black", "#400060","#922424","#48489A","#A16D6D",
          "#9191A9","#B0B6B6","#d0d0d0","#FFFFC0"))
par(mar = c(4,12,1,1), las=1, cex.axis = 1.8)
layout(matrix(c(1,1,2), byrow = T, nrow = 1, ncol = 3))
layout.show(2)
barplot(t(as.matrix(EUremed[,5:12])), 
        names.arg = EUremed$Country, 
        horiz = T, 
        col = seq(2,9),
        cex.axis = 1.2)
par(mar = c(4,4,1,1), las=1, cex.axis = 1.8)
plot(c(0,1), c(0,1), ann = F, xaxt = "n", yaxt = "n", bty = "n", type = "n")
legend("left", legend = colnames(EUremed[,5:12]), 
       pch = 22, cex = 2,
       pt.bg = c("#400060","#922424","#48489A","#A16D6D",
                 "#9191A9","#B0B6B6","#d0d0d0","#FFFFC0"), 
       pt.cex = 5, y.intersp = 1.4, bty = "n")

layout(matrix(c(1), byrow = T, nrow = 1, ncol = 1))
layout.show()
