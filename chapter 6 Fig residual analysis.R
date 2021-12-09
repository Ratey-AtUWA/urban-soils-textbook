# plot observed vs. predicted ####
par(mfrow=c(1,1), mar=c(4,4,1,3), mgp = c(2,0.4.0), tcl = 0.5)
plot(minreg$fitted.values, mregdata$As.log, pch = 15, cex = 1.5, 
     xaxt = "n", yaxt = "n", ann = F)
axis(1, at = log10(c(1, 2, 5, 10)), labels = c(1, 2, 5, 10), 
     cex.axis = 1.5, mgp = c(2.5,0.4,0))
mtext(expression(bold(paste("Model log"[10]," As"))), 1, 2.5, cex=1.5)
axis(2, at = log10(c(1, 2, 5, 10)), labels = c(1, 2, 5, 10), 
     cex.axis = 1.5, mgp = c(2.5,0.4,0))
mtext(expression(bold(paste("Measured log"[10]," As"))), 2, 2.4, cex=1.5)

require(data.table)
plotvars <- data.table(X = minreg$fitted.values, Y= scale(minreg$residuals))
setorder(plotvars, X)
plotvars <- as.data.frame(plotvars)
colnames(plotvars) <- c("X", "Y")
points(plotvars[,2]/10 ~ plotvars[,1], type="b", col="dodgerblue3")
points(plotvars[,2]/10 ~ plotvars[,1], col="dodgerblue3", lwd = 3, 
       subset = plotvars[,2] > 2)
axis(4, mgp = c(2, 0.3, 0), at=seq(-0.3,0.3,0.1), labels=seq(-3,3,1), 
     col="dodgerblue3")
mtext(side=4, line=1.6, text="Standardised residual", 
      font=2, cex = 1.4, adj=0.0, col="dodgerblue3")
abline(h = c(0,0.2), col = "lightsteelblue3", lty = 3)
#
abline(0,1, col="sienna", lwd = 2, lty = 2)
text(0.92,0.8,labels="1:1 line", col="sienna", cex = 1.5)
points(minreg$fitted.values, mregdata$As.log, pch = 15, 
       cex = 1.5) # replot points
#
ellipse(center=c(0.48,0.85), 
        shape = matrix(c(1.2,0.5,0.5,1.2), nrow=2, ncol=2), 
        radius = 0.1,
        center.pch = NA)
# ellipse(center=c(0.28,0.55), 
#         shape = matrix(c(.3,0,0,.3), nrow=2, ncol=2), 
#         radius = 0.1,
#         center.pch = NA)
ellipse(center=c(0,0.8), 
        shape = matrix(c(4,0,0,4), nrow=2, ncol=2), 
        radius = 0.1,
        center.pch = NA)
text(0,0.8,labels="Samples with\nstandardised\nresidual > 2", 
     col="blue", cex = 1.4)
#
