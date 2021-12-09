---
title: "R Notebook"
output: html_notebook
---

# [43] ________  "ph.pow", "ec.pow”, 
# [45] "cr.pow", "cu.pow", "fe.pow", "al.pow”, 
# [49] "as.log", "ba.log", "ca.pow", "cd.pow”, 
# [53] "ce.log", "gd.log", "k.log", "la.log”, 
# [57] "mg.pow", "mn.pow", "mo.pow", "na.log”, 
# [61] "nd.log", "ni.pow", "p.pow", "pb.log”, 
# [65] "s.pow", "sr.pow", "th.log", "v.pow", 
# [69] "y.pow", "zn.log", "ph.log", "ec.log”, 
# [73] "al.log", "as.pow", "ba.pow", "ca.log”, 
# [77] "cd.log", "ce.pow", "cr.log", "cu.log”, 
# [81] "fe.log", "gd.pow", "k.pow", "la.pow”, 
# [85] "mg.log", "mn.log", "mo.log", "na.pow”, 
# [89] "nd.pow", "ni.log", "p.log", "pb.pow”, 
# [93] "s.log", "sr.log", "th.pow", "v.log", 
# [97] "y.log", "zn.pow", "depth_mean"

```{r}
require(car)
require(RcmdrMisc)
require(effects)
require(gvlma)
sv18 <- read.csv("C:/Users/00028958/LocalData/Dropbox (Ratey at UWA)/R Projects/Smiths-Veryard-2018/sv18.csv")
sv18soil <- subset(sv18, subset = sv18$Type == "Soil")
sv18soil$Depth_mean <- (sv18soil$Depth_upper + sv18soil$Depth_lower)/2
mregdata <- na.omit(sv18soil[,c("Group","Sample","As.log","pH","EC.log",
                                "Al.log","Ca.log","Fe.log","K.log","Mn.log",
                                "Na.log","P.log","S.log","Depth_mean")])
rownames(mregdata) <- NULL
# mregdata <- mregdata[order(mregdata[,3]),]
# maximal regression model and checks ####
```

```{r}
attach(mregdata, warn=F)
maxreg <- lm(As.log ~ pH+ EC.log+ Al.log+ Ca.log+ Fe.log+ K.log+
                 Mn.log+ Na.log+ P.log+ S.log) # 
summary(maxreg)

cor(mregdata[,c("pH","EC.log", "Al.log",
                         "Ca.log","Fe.log","K.log", "Mn.log",
                         "Na.log","P.log","S.log")], 
             use = "pairwise.complete.obs") # "As.log", 
vif(maxreg)
```

# forward regression from dummy model with no predictors ####
```{r}
minreg = lm(As.log ~ Fe.log)
fwd_reg = step(minreg, direction='forward', scope=(~ pH+ EC.log+ Ca.log+ K.log+
                                                     Na.log+ P.log+ S.log),
               trace = -1) # Al.log+ Mn.log+ 
summary(fwd_reg)
vif(fwd_reg)
gvlma(fwd_reg)
```

# stepwise (backward-forward) refinement of maximal regression model and checks ####
```{r}
mregAs <- step(maxreg, trace = -1)
summary(mregAs)
vif(mregAs)
shapiro.test(mregAs$residuals)
# check assumptions using gvlma package
assumpt.mregAs <- gvlma(mregAs)
print(assumpt.mregAs)
summary(assumpt.mregAs)
loocv.mregAs <- deletion.gvlma(assumpt.mregAs)
display.delstats(loocv.mregAs$DeltaGlobalStat,loocv.mregAs$GStatpvalue)
```

# plot deletion (leave-one-out) stats for each predictor
```{r}
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.,0.5,0), tcl=-0.2, lend="square", ljoin="mitre", font.lab=2)
  display.delstats(loocv.mregAs$DeltaStat1,loocv.mregAs$Stat1pvalue)
  mtext(names(assumpt.mregAs$effects)[2], side = 3, line = -1.1, adj = 0.9)
  display.delstats(loocv.mregAs$DeltaStat2,loocv.mregAs$Stat2pvalue)
  mtext(names(assumpt.mregAs$effects)[3], side = 3, line = -1.1, adj = 0.9)
  display.delstats(loocv.mregAs$DeltaStat3,loocv.mregAs$Stat3pvalue)
  mtext(names(assumpt.mregAs$effects)[4], side = 3, line = -1.1, adj = 0.9)
  display.delstats(loocv.mregAs$DeltaStat4,loocv.mregAs$Stat4pvalue)
  mtext(names(assumpt.mregAs$effects)[5], side = 3, line = -1.1, adj = 0.9)
  par(mfrow = c(1, 1))
```

# plot model diagnostics and residual analysis ####
```{r}
par(mfrow=c(3,2), mar=c(4,4,2,1), mgp=c(2.,0.5,0), tcl=-0.2, lend="square", ljoin="mitre", font.lab=2)
plot(mregAs)
Boxplot(scale(mregAs$residuals), ylim=c(-3.3,3.3), cex=1.2, 
        ylab="Standardised residual for\nmultiReg predicting As.log")
abline(h=2,lty=2,col="grey")
stripchart(scale(mregAs$residuals), vertical=T, col="#0000FF40", add=T, 
           method="jitter", jitter=0.1, pch=2, lwd=2, lend="square", ljoin="mitre")
hist(scale(mregAs$residuals), vertical=T, col="#b000c040", breaks=14,
           main="", xlab=expression(bold(zAs[resid])), cex.lab=1.5)
abline(v=2,lty=2,col="grey")
```

# residual autocorrelation ####
```{r}
par(mfrow=c(1,1))
acf(mregAs$residuals)$acf
#
# test if predictors correlated with residuals (they shouldn't be) ####
rcorr.adjust(cbind(mregdata[,c("pH", "EC.log", "Fe.log", "S.log")], mregAs$residuals), 
    use = "pairwise.complete.obs")
spm(cbind(mregdata[,c("pH", "EC.log", "Fe.log", "S.log")], mregAs$residuals), smooth = F, regLine = F)
#
# calculate cooks distance (e.g. bottom right diagnostic plot)
summary(cooks.distance(mregAs))
par(mfrow=c(1,1))
barplot(cooks.distance(mregAs), ylab = "Cook's Distance")
#
# plot observed vs. predicted ####
par(mfrow=c(1,1), mar=c(4,4,1,3), tcl = 0.5)
plot(mregAs$fitted.values, mregdata$As.log, pch = 15, cex = 1.5, 
     xaxt = "n", yaxt = "n", ann = F)
axis(1, at = log10(c(1, 2, 5, 10)), labels = c(1, 2, 5, 10), cex.axis = 1.5)
mtext(expression(bold(paste("Model log"[10]," As"))), 1, 2, cex=1.5)
axis(2, at = log10(c(1, 2, 5, 10)), labels = c(1, 2, 5, 10), cex.axis = 1.5)
mtext(expression(bold(paste("Measured log"[10]," As"))), 2, 2, cex=1.5)

require(data.table)
plotvars <- data.table(X = mregAs$fitted.values, Y= scale(mregAs$residuals))
setorder(plotvars, X)
plotvars <- as.data.frame(plotvars)
colnames(plotvars) <- c("X", "Y")
points(plotvars[,2]/10 ~ plotvars[,1], type="b", col="dodgerblue3")
points(plotvars[,2]/10 ~ plotvars[,1], col="dodgerblue3", lwd = 3, 
       subset = plotvars[,2] > 2)
axis(4, mgp = c(2, 0.3, 0), at=seq(-0.3,0.3,0.1), labels=seq(-3,3,1), col="dodgerblue3")
mtext(side=4, line=1.6, text="Standardised residual", 
      font=2, cex = 1.4, adj=0.0, col="dodgerblue3")
abline(h = c(0,0.2), col = "lightsteelblue3", lty = 3)
#
abline(0,1, col="sienna", lwd = 2, lty = 2)
text(0.92,0.8,labels="1:1 line", col="sienna", cex = 1.5)
points(mregAs$fitted.values, mregdata$As.log, pch = 15, cex = 1.5) # replot points
#
ellipse(center=c(0.55,0.85), 
        shape = matrix(c(1.2,0.5,0.5,1.2), nrow=2, ncol=2), 
        radius = 0.1,
        center.pch = NA)
ellipse(center=c(0.28,0.55), 
        shape = matrix(c(.3,0,0,.3), nrow=2, ncol=2), 
        radius = 0.1,
        center.pch = NA)
ellipse(center=c(0,0.8), 
        shape = matrix(c(4,0,0,4), nrow=2, ncol=2), 
        radius = 0.1,
        center.pch = NA)
text(0,0.8,labels="Samples with\nstandardised\nresidual > 2", col="blue", cex = 1.4)
#
# alternative ovserved vs. fitrted plot ####
sp(mregdata$As.log ~ mregAs$fitted.values, 
   ellipse = list(levels=c(.75, .95), robust=TRUE, fill=TRUE, fill.alpha=0.1), 
   smooth = F, 
   cex = 1.5, cex.lab = 1.5, boxplots=F, 
   reset.par = TRUE)
#
#
#
#
detach(mregdata, warn=F)
rm(list=c("mregdata","maxreg","minreg","fwd_reg"))
summary(mregAs)
vif(mregAs)
sort(scale(mregAs$residuals))
#
# newx <- as.data.frame(seq(from=min(As.log), to=max(As.log), length.out=nrow(mregdata)))
# newx <- data.frame(seq(from=min(As.log), to=max(As.log), length.out=nrow(mregdata)),
#                    seq(from=min(As.log), to=max(As.log), length.out=nrow(mregdata)))
# colnames(newx)[1] <- "As.log"
# ci0 <- predict(mregAs, interval="confidence", newdata=data.frame(Fe.log=newx)) # calcs CIs for fitted values
# ci1 <- ci0[order(ci0[,1]),]
# plot(As.log ~ mregAs$fit)
# abline(0,1, col="red3")
# lines(ci1[,1], ci1[,2],lty=1, lwd=2, col="blue", cex=0.2, pch=16)
# lines(ci1[,1], ci1[,3], lty=1, lwd=2, col="red", cex=0.2, pch=16)

```{r}
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
