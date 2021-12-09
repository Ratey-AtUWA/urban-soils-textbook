Du2014 <- read.csv("Du_etal_2014.csv")
str(Du2014)
Du2014$Landuse <-
  factor( Du2014$Landuse,
    levels = c("Built-up", "Idle land", "Water", "Forest",
               "Farmland", "Fishpond", "Orchard", "Grass"))
par(mar = c(6.5,6,1,1), mgp = c(5.5,0.3,0), tcl = 0.3, font.lab = 2,
    ljoin = "mitre", lend = "square", 
        cex.lab = 1.5, cex.axis = 1.4)
barplot(t(as.matrix(Du2014[,2:4])), 
        names.arg = Du2014$Landuse,
        beside = TRUE, las = 2, log = "y",
        ylim = c(100,20000), col = "transparent",
        xlab = "Land Use")
mtext(expression(bold("Area / km"^2)), 2, 4, cex = 1.5)
abline(h = c(100,200,500,1000,2000,5000,10000,20000), 
       col = "grey", lty = 2)
barplot(t(as.matrix(Du2014[,2:4])), 
        xaxt = "n", yaxt = "n", 
        beside = TRUE, las = 2, log = "y",
        ylim = c(100,20000), add = TRUE, 
        col = c("blue3", "lemonchiffon", "tan3"))
box()
legend(5,19000, bty = "o", cex = 1.5,
       border = "white", box.col = "white",
       legend = c("1990", "2000", "2010"),
       pch = 22, pt.cex = 3, y.intersp = 1.25, 
       pt.bg = c("blue3", "lemonchiffon", "tan3"))
