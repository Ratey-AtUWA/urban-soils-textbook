LiRan2012 <- read.csv("LiRan2012.csv")
str(LiRan2012)
summary(lm0)
#
par(mfrow=c(1,1), mar = c(3,3,1,1), oma = c(0,0,0,0), tcl = 0.3, bty="o", 
    mgp=c(1.6,0.3,0), font.lab =2, cex.lab = 1.4, cex.axis = 1.4)
lm0 <- lm(LiRan2012$logKoc ~ LiRan2012$logKow)
temp_var <- predict(lm0, interval="prediction")

new_df <- cbind(LiRan2012, temp_var)
new_df <- new_df[order(new_df$logKow),]

with(LiRan2012,
     plot(logKoc ~ logKow, 
          xaxs = "i", xlim = c(3, 7),
          xlab = expression(bold(paste("log ",bolditalic("K")[ow]))),
          yaxs = "i", ylim = c(4.9, 8.4),
          ylab = expression(bold(paste("log ",bolditalic("K")[oc]," (mL/g)"))),
          type = "n")
)

polygon(c(new_df$logKow,rev(new_df$logKow)),
        c(new_df$lwr,rev(new_df$upr)),
        border = "transparent", col = "grey88")

temp_var <- predict(lm0, interval="confidence")

new_df <- cbind(LiRan2012, temp_var)
new_df <- new_df[order(new_df$logKow),]

# lines(new_df$logKow, new_df$lwr, col="blue2", lty = 2, lwd = 1)
# 
# lines(new_df$logKow, new_df$upr, col="blue2", lty = 2, lwd = 1)

polygon(c(new_df$logKow,rev(new_df$logKow)),
        c(new_df$lwr,rev(new_df$upr)),
        border = "transparent", col = "lightskyblue3")
lines(new_df$logKow[c(1,12)], new_df$fit[c(1,12)], col=1, lty = 1, lwd = 1)


for (i in 1:NROW(new_df)){
  arrows(new_df$logKow[i], new_df$logKoc[i],
         new_df$logKow[i], new_df$logKoc[i]+new_df$errorbar[i],
         length = 0.05, angle = 90, lwd = 3, col = "white")
  arrows(new_df$logKow[i], new_df$logKoc[i],
         new_df$logKow[i], new_df$logKoc[i]-new_df$errorbar[i],
         length = 0.05, angle = 90, lwd = 3, col = "white")
}
for (i in 1:NROW(new_df)){
  arrows(new_df$logKow[i], new_df$logKoc[i],
         new_df$logKow[i], new_df$logKoc[i]+new_df$errorbar[i],
         length = 0.05, angle = 90)
  arrows(new_df$logKow[i], new_df$logKoc[i],
         new_df$logKow[i], new_df$logKoc[i]-new_df$errorbar[i],
         length = 0.05, angle = 90)
}
with(LiRan2012,
     points(logKoc ~ logKow, pch = 1, cex = 1.3, col = "white"))
with(LiRan2012,
     points(logKoc ~ logKow, pch = 19))

legend(3.1, 8.45, bty = "n", inset = 0.02,
       legend = c("Prediction interval", "95% confidence interval"),
       pch = c(15, 15), col = c("grey88", "lightskyblue3"), pt.cex = 6,
       x.intersp = 1.4, y.intersp = 1.8, cex = 1.4)

mtext(expression(paste(italic("y")," = 0.5076",italic("x")," + 4.0276")),
      1, -3, adj = 0.95, family = "serif", cex = 1.4)
mtext(expression(paste(italic("r")," = 0.897  ",italic("P")," < 0.001")),
      1, -1.5, adj = 0.95, family = "serif", cex = 1.4)
