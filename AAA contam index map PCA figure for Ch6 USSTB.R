require(OpenStreetMap)
require(prettymapr)
#
palette(c("white","purple","blue3","green3","olivedrab4",
          "khaki4","gold2","tomato","red3","grey87",
          "black"))
par(mar=c(3,3,1,1), lend=2, ljoin=1)
plot(SV.mapboxL.utm, removeMargin=FALSE)
axis(1, mgp=c(1.6,0.4,0), tcl=0.33)
mtext(side=1, line=1.6, text="Easting (UTM Zone 50, m)",
      font=2, cex=1.4)
axis(2, at = seq(6466300,6466700,100), labels = seq(6466300,6466700,100), 
     mgp=c(1.6,0.4,0), tcl=0.33)
mtext(side=2, line=1.6, text="Northing (UTM Zone 50, m)",
      font=2, cex=1.4)
text(391242,6466652,labels="Charles Veryard\nReserve",col="darkgreen", font=3, cex=0.85)
text(391370,6466388,labels="Smith's Lake\nReserve",col="darkgreen", font=3, cex=0.85)
text(391355,6466460,labels="Smith's\nLake",col="skyblue4", font=3, cex=0.85)
text(391274,6466495,labels="Electrical\nsubstation",col="grey33", font=3, cex=0.85)
text(391405,6466660, labels="P",col="cornflowerblue", cex=1.25, font=2)
text(391225,6466560, labels="P",col="cornflowerblue", cex=1.25, font=2)
addnortharrow(pos="topright", border=11, text.col=11, padin=c(0.2,0.3), scale=1.4)
addscalebar(linecol=11, label.col=11, pos="bottomright", padin=c(0.2,0.2), 
            plotepsg=28350, label.cex=1.8, htin=0.16)
rect(391040, 6466490, 391130, 6466532, col = 1, border = 1, lwd = 10, ljoin = "round")
text(c(391037,391035),c(6466509,6466510), pos = 4, labels=rep("PC1",2),
     cex = 3, font = 2, col = c(1,11))
box(lwd=2)
#
# make a vector of calculated quantiles for selected variable ####
q0 <- as.numeric(signif(quantile(clrdata$PC1, 
                                 probs=c(0.02,0.05,0.25,0.5,0.75,0.95,0.98),
                                 na.rm=TRUE),3))
# make a vector of symbol sizes
cex0 <- c(0.7,0.8,0.9,0.75,0.85,1.4,1.7,2)*2
#
# plot the points according to the calculated percentiles ####
points(clrdata$Northing ~ clrdata$Easting, pch=22, bg=2, cex=cex0[1], lwd=1,
       subset=clrdata$PC1<q0[1])
points(clrdata$Northing ~ clrdata$Easting, pch=22, bg=3, cex=cex0[2], lwd=1,
       subset=clrdata$PC1>q0[1]&clrdata$PC1<q0[2])
points(clrdata$Northing ~ clrdata$Easting, pch=22, bg=4, cex=cex0[3], lwd=1,
       subset=clrdata$PC1>q0[2]&clrdata$PC1<q0[3])
points(clrdata$Northing ~ clrdata$Easting, pch=3, col=5, cex=cex0[4], lwd=2,
       subset=clrdata$PC1>q0[3]&clrdata$PC1<q0[4])
points(clrdata$Northing ~ clrdata$Easting, pch=4, col=6, cex=cex0[5], lwd=2,
       subset=clrdata$PC1>q0[4]&clrdata$PC1<q0[5])
points(clrdata$Northing ~ clrdata$Easting, pch=21, bg=7, cex=cex0[6], lwd=1,
       subset=clrdata$PC1>q0[5]&clrdata$PC1<q0[6])
points(clrdata$Northing ~ clrdata$Easting, pch=21, bg=8, cex=cex0[7], lwd=1,
       subset=clrdata$PC1>q0[6]&clrdata$PC1<q0[7])
points(clrdata$Northing ~ clrdata$Easting, pch=21, bg=9, cex=cex0[8], lwd=1,
       subset=clrdata$PC1>q0[7])
#
# add a legend taking care to match symbols & colours ####
legend(391035, 6466465, bty="o", pch=c(22,22,22,3,4,21,21,21), inset=0.025, 
       cex=cex0[1], pt.cex=cex0, pt.lwd=c(1,1,1,2,2,1,1,1), 
       text.col = 11, box.col = 10, bg = 10, y.intersp=1.1, 
       title=expression(bold("PC1 percentiles:")), 
       legend=c(paste0("< 2% (",signif(min(clrdata$PC1, na.rm=T),3),
                       " - ",q0[1],")"),
                paste0("2-5% (",q0[1]," - ",q0[2],")"),
                paste0("5-25% (",q0[2]," - ",q0[3],")"),
                paste0("25-50% (",q0[3]," - ",q0[4],")"),
                paste0("50-75% (",q0[4]," - ",q0[5],")"),
                paste0("75-95% (",q0[5]," - ",q0[6],")"),
                paste0("95-98% (",q0[6]," - ",q0[7],")"),
                paste0("> 98% (",q0[7]," - ",
                       signif(max(clrdata$PC1, na.rm=T),3),")")), 
       pt.bg=seq(2,9), col=c(11,11,11,5,6,11,11,11))
# end code ####













points(clrdata$Northing ~ clrdata$Easting,
       pch=seq(1,11)[clrdata$Group],
       col=c(seq(1,10),"pink")[clrdata$Group],
       lwd=c(3,3,3,3,3,3,2,2,2,2,2)[clrdata$Group], 
       cex=c(1.8,rep(1.6,10))[clrdata$Group])
summary(clrdata[,6:7])
# points(as.numeric(tapply(clrdata$Easting, clrdata$Group, mean)),
#      as.numeric(tapply(clrdata$Northing, clrdata$Group, mean)),
#      pch=16, col="#FFFFFFA0", cex=1.6)
text(as.numeric(tapply(clrdata$Easting, clrdata$Group, mean)) +
       as.numeric(tapply(clrdata$Easting, clrdata$Group, sd)),
     as.numeric(tapply(clrdata$Northing, clrdata$Group, mean)),
     labels=names(tapply(clrdata$Easting, clrdata$Group, mean)),
     pos=4, col=seq(1,9), offset=0.65, cex=0.75) # 
legend(391035, 6466500, legend=levels(clrdata$Group), title="Group",
       pch=seq(1,9), col=seq(1,9),
       pt.cex=c(1.8,rep(1.6,8)), 
       pt.lwd=c(3,3,3,3,3,3,2,2,2), box.col="grey67",
       box.lwd=2, bg=10, inset=0.03, 
       text.col=seq(1,9), title.col=1)
# set up mapbox api key
# get your own: available for free at https://www.mapbox.com/pricing/
# styles are: streets-v10, outdoors-v10, light-v9, dark-v9
# satellite-v9, satellite-streets-v10, navigation-preview-day-v2
# navigation-preview-night-v2, navigation-guidance-day-v2
# navigation-guidance-night-v2
# apiKey <- paste0("?access_token=",
# "pk.eyJ1IjoicmF0ZXlhdHV3YSIsImEiOiJjamk5cnlta2cwenM4M29tcjl1NXBhYjhsIn0.sqWHsDFsZNvKjnVZTSrqRg")
# baseUrl <- "https://api.mapbox.com/styles/v1/mapbox/satellite-streets-v10/tiles/256/{z}/{x}/{y}"
# # baseUrl <- "https://api.mapbox.com/styles/v1/rateyatuwa/cji9tr9711lny2so9pocqmzxk/tiles/256/{z}/{x}/{y}"
# SV999TEMP.osm <- openmap(c(-31.9296, 115.8472), c(-31.9348, 115.853),
#                   type=paste0(baseUrl,apiKey), zoom=17.5)
# SV.mapboxA.utm <- openproj(SV999TEMP.osm, projection="+proj=utm +zone=50 +south")
# "purple","blue3","green3","olivedrab","khaki","yellow","orange","tomato","red2"
#
# end code