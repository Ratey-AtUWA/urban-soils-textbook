summary(mregAs)
regAsFe <- lm(As.log ~ Fe.pow, data = mregdata)
summary(regAsFe)
stripchart(scale(regAsFe$residuals), method = "jitter")
fx.reg.As<-allEffects(regAsFe, xlevels=25, confidence.level=0.99)
# fx.reg.As$Fe.pow$x
plot(As.log~Fe.pow, data=mregdata, cex.axis=1.4, cex.lab=1.4, font.lab=2)
lines(fx.reg.As$Fe.pow$fit~fx.reg.As$Fe.pow$x[,1], lwd=2, col="gray50")
lines(fx.reg.As$Fe.pow$upper~fx.reg.As$Fe.pow$x[,1], lty=2, col="gray50")
lines(fx.reg.As$Fe.pow$lower~fx.reg.As$Fe.pow$x[,1], lty=2, col="gray50")
#
#
require(data.table)
plotvars <- data.table(X = regAsFe$fitted.values, Y= scale(regAsFe$residuals))
setorder(plotvars, X)
plotvars <- as.data.frame(plotvars)

par(mar=c(3,3,1,3), mgp=c(1.6,0.3,0), tcl=0.3)
plot(mregdata$As.log ~ regAsFe$fitted.values, pch=15,
     xlab = expression(bold(paste("Model log"[10], "As"))),
     ylab = expression(bold(paste("Measured log"[10], "As"))))
abline(h = 0, lty = 2, col = "grey")
lines(c(min(regAsFe$fitted.values), max(regAsFe$fitted.values)), c(0.2, 0.2),
      col="grey", lty=2)
points(plotvars[,2]/10 ~ plotvars[,1], type="b", col="dodgerblue3")
points(mregdata$As.log ~ regAsFe$fitted.values, pch=15)
axis(4, at=seq(-0.3,0.3,0.1), labels=seq(-3,3,1), col="dodgerblue3")
mtext(side=4, line=1.6, text="Standardised residual", 
      font=2, adj=0.0, col="dodgerblue3")
abline(0,1, col="sienna")
text(0.92,0.8,labels="1:1 line", col="sienna")
ellipse(center=c(0.45,0.85), 
        shape = matrix(c(1.2,0.5,0.5,1.2), nrow=2, ncol=2), 
        radius = 0.1,
        center.pch = NA)
text(0.25,0.975,labels="Samples with standardised\nresidual > 2", col="blue")

