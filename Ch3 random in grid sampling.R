require(OpenStreetMap)
require(prettymapr)
# apiKey <- paste0("?access_token=",
#                  "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
# baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/outdoors-v11/tiles/256/{z}/{x}/{y}"
# SV.mapbox.osm <- openmap(c(-31.9296, 115.8472), c(-31.9348, 115.852),
#                   type=paste0(baseUrl,apiKey), zoom=17.5)
# SV.mapboxO.utm <- openproj(SV.mapbox.osm, projection="+proj=utm +zone=50 +south")
# rm(SV.mapbox.osm)
# random-in-grid sampling ####
palette(c("black",rainbow(8,start=0,end=0.75,s=0.9,v=0.7),"#E0E0E0",
          "white","#B6E59E","#75CFF0"))
par(mar=c(3,0,1,0), oma=c(0,3,0,1.5), 
    mfrow=c(1,2), lend=2, ljoin=1)
plot(SV.mapboxO.utm, removeMargin=FALSE)
axis(1, tcl=0.4, mgp=c(2,0.3,0))
mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)",
      font=2, cex=1.4)
axis(2, tcl=0.4, mgp=c(2,0.3,0))
mtext(side=2, line=1.6, text="Northing (UTM Zone 50, m)",
      font=2, cex=1.4)
mtext(side=3, line=-1.2, text="(a)",
      adj=0.02, font=2, cex=1.4)
abline(v=seq(391060,391450,52), lty=3, col="purple")
abline(h=seq(6466240,6466800,52), lty=3, col="purple")
polygon(c(391380,391380,391423,391423), c(6466638,6466685,6466685,6466638),
        col=11, border=11)
lines(c(391420,391380,391380,391424,391424),c(6466636,6466636,6466685,6466685,6466646), 
      col="grey92", lwd=2, lend="round", ljoin="round")
text(391405,6466660, labels="P",col="cornflowerblue", cex=1.25, font=2)
text(391224,6466564, labels="P",col="cornflowerblue", cex=1.25, font=2)
text(seq(391060,391400,52)+26,rep(6466760,7),labels=seq(1,7),
     col="red", font=2, pos=1, offset=0.2)
text(seq(391060,391400,52)+26,rep(6466708,7),labels=seq(8,14),
     col="red", font=2, pos=1, offset=0.2)
text(seq(391060,391380,52)+26,rep(6466656,7),labels=seq(15,21),
     col="red", font=2, pos=1, offset=0.2)
text(c(seq(391216,391400,52)+26,seq(391320,391400,52)+26,seq(391320,391400,52)+26,
       seq(391268,391400,52)+26,seq(391268,391400,52)+26,
       seq(391320,391400,52)+26,seq(391320,391400,52)+26),
     c(rep(6466604,4),rep(6466552,2),rep(6466500,2),rep(6466448,3),rep(6466396,3),
       rep(6466344,2),rep(6466292,2)),
labels=seq(22,39),
     col="red", font=2, pos=1, offset=0.2)
box()
text(391370,6466358,labels="Smith's Lake\nReserve",col=6, font=4, cex=0.8)
text(391355,6466464,labels="Smith's\nLake",col="skyblue4", font=4, cex=0.8)
text(391274,6466495,labels="Electrical\nsubstation",col="grey33", font=3, cex=0.8)
# polygon(c(391350,391350,391420,391420), c(6466280,6466320,6466320,6466280),
#         col="#CCFACC", border="#CCFACC")
addnortharrow(border=1, text.col=1, padin=c(0.25,0.5))
addscalebar(linecol=1, label.col=1, pos="bottomleft", padin=c(0.25,0.5), 
            plotepsg=28350, label.cex=1.2, htin=0.16)
seq(391060,391400,52)
rev(seq(6466240,6466800,52))
points(SVplan$RandomN~SVplan$RandomE, subset=SVplan$Rep==1, pch=19, lwd=2)
points(SVplan$RandomN~SVplan$RandomE, subset=SVplan$Rep==2, pch=19, lwd=2)
box(lwd=2)
#
# ~randomised sampling ####
plot(SV.mapboxO.utm, removeMargin=FALSE)
axis(1, tcl=0.4, mgp=c(2,0.3,0))
mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)",
      font=2, cex=1.4)
axis(4, tcl=0.4, mgp=c(2,0.3,0))
mtext(side=3, line=-1.2, text="(b)",
      adj=0.02, font=2, cex=1.4)
polygon(c(391380,391380,391423,391423), c(6466638,6466685,6466685,6466638),
        col=11, border=11)
lines(c(391420,391380,391380,391424,391424),c(6466636,6466636,6466685,6466685,6466646), 
      col="grey92", lwd=2, lend="round", ljoin="round")
text(391405,6466660, labels="P",col="cornflowerblue", cex=1.25, font=2)
text(391224,6466564, labels="P",col="cornflowerblue", cex=1.25, font=2)
box()
text(391370,6466358,labels="Smith's Lake\nReserve",col=6, font=4, cex=0.8)
text(391355,6466464,labels="Smith's\nLake",col="skyblue4", font=4, cex=0.8)
text(391274,6466495,labels="Electrical\nsubstation",col="grey33", font=3, cex=0.8)
addnortharrow(border=1, text.col=1, padin=c(0.25,0.5))
addscalebar(linecol=1, label.col=1, pos="bottomleft", padin=c(0.25,0.5), 
            plotepsg=28350, label.cex=1.2, htin=0.16)
# cvr1-21 44, cvr22-27a 8, slr26b-39 26
x0 <- rep(NA,44)
y0 <- rep(NA,length(x0))
for (i in 1:length(x0)) {
  x0[i] <- sample(seq(391060,391424,1),1)
  y0[i] <- sample(seq(6466604,6466760,1),1)
}
points(y0 ~ x0, pch=19, lwd=2)
#
x0 <- rep(NA,8)
y0 <- rep(NA,length(x0))
for (i in 1:length(x0)) {
  x0[i] <- sample(seq(391216,391424,1),1)
  y0[i] <- sample(seq(6466552,6466604,1),1)
}
points(y0 ~ x0, pch=19, lwd=2)
#
x0 <- rep(NA,21)
y0 <- rep(NA,length(x0))
for (i in 1:length(x0)) {
  x0[i] <- sample(seq(391320,391424,1),1)
  y0[i] <- sample(seq(6466292,6466535,1),1)
}
points(y0 ~ x0, pch=19, lwd=2)
#
x0 <- rep(NA,2)
y0 <- rep(NA,length(x0))
for (i in 1:length(x0)) {
  x0[i] <- sample(seq(391300,391320,1),1)
  y0[i] <- sample(seq(6466344,6466448,1),1)
}
points(y0 ~ x0, pch=19, lwd=2)
#
x0 <- rep(NA,3)
y0 <- rep(NA,length(x0))
for (i in 1:length(x0)) {
  x0[i] <- sample(seq(391346,391424,1),1)
  y0[i] <- sample(seq(6466248,6466292,1),1)
}
points(y0 ~ x0, pch=19, lwd=2)
#
box(lwd=2)
# end code