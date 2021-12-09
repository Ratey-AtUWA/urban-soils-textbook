par(mar = c(3,3,1,3), mgp = c(1.4,0.2,0), tcl = 0.3, font.lab = 2,
    xaxs = "i", yaxs = "i", cex = 1.2)
plot(evans2000$pH ~ evans2000$Days, 
     type = "b",
     pch=15,
     xlab = "Time (days)",
     ylab = "Dredge spoil pH",
     xlim = c(0, 58),
     ylim = c(2,8),
     xaxs = "i")
lines((evans2000$EC / 2) +1 ~ evans2000$Days, 
      type = "b",
      pch=1)
grid()
axis(4, at = (seq(2, 14, 2) / 2) + 1, labels=seq(2, 14, 2))
mtext(4,1.4,text="Dredge spoil EC (dS/m)", cex=1.2, font=2)
legend("top", ncol = 2, legend = c("pH", "EC"), pch = c(15, 1),
       pt.cex=1.2, cex=1.2, bty="n")
box()
