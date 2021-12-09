win.metafile(file = "soiltemp.wmf", height=6, width=15)
par(mfrow = c(2, 1),
    mar = c(2.8, 3, 1, 1),
    mgp = c(1.6, 0.3, 0), 
    tcl = 0.3,
    cex.lab = 1.5,
    cex.axis = 1.4,
    font.lab = 2)
palette(c("black", "red2", "sienna3", "navy", "white"))
with(murrum[2017:2497,], 
     plot(smooth.spline(days, Temp_4cm, 
                        spar = 0.4),
          xaxs = "i",
          xlim = c(41.9,53.9), 
          type="l", 
          col=2,
          xlab = "Days from 1 December 2008",
          ylab = "Soil temperature (\u00B0C)",
          lwd = 2, lty = 5)
     )
with(murrum[2017:2497,], 
       lines(smooth.spline(days, Temp_15cm, 
                           spar = 0.4), 
             col=3,
             lwd = 2, lty = 2)
     )
abline(v = seq(40,52,1), col = "grey", lty = 2)
with(murrum[2017:2497,], 
       lines(smooth.spline(days, Temp_45cm, 
                            spar = 0.4), 
             col=4,
             lwd = 2, lty = 1)
     )
legend("right",
       cex = 1.5,
       title = expression(bold("Summer")),
       legend = c("5 cm", "15 cm", "45 cm"),
       col = c(2, 3, 4),
       lwd = c(2, 2, 2),
       lty = c(5, 2, 1),
       bty = "o", box.col = 5,
       inset = 0.02)
#
# winter plot
with(murrum_W[2017:2497,], 
     plot(smooth.spline(days, Temp_4cm, 
                        spar = 0.4),
          xaxs = "i",
          xlim = c(41.9,53.9), 
          type="l", 
          col=2,
          xlab = "Days from 1 June 2009",
          ylab = "Soil temperature (\u00B0C)",
          lwd = 2, lty = 5)
)
with(murrum_W[2017:2497,], 
     lines(smooth.spline(days, Temp_15cm, 
                         spar = 0.4), 
           col=3,
           lwd = 2, lty = 2)
)
abline(v = seq(40,52,1), col = "grey", lty = 2)
with(murrum_W[2017:2497,], 
     lines(smooth.spline(days, Temp_45cm, 
                         spar = 0.4), 
           col=4,
           lwd = 2, lty = 1)
)
legend("right",
       cex = 1.5,
       title = expression(bold("Winter")),
       legend = c("5 cm", "15 cm", "45 cm"),
       col = c(2, 3, 4),
       lwd = c(2, 2, 2),
       lty = c(5, 2, 1),
       bty = "o", box.col = 5,
       inset = 0.02)
dev.off()
#