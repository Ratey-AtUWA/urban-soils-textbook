sv17soil <- read.csv("sv2017soil.csv")
sv17soil$PLI <- ((sv17soil$As/1.4)*
                   (sv17soil$Ba/6.6)*
                   (sv17soil$Cr/3.6)*
                   (sv17soil$Cu/0.2)*
                   (sv17soil$Mn/8.0)*
                   (sv17soil$Pb/1.1)*
                   (sv17soil$Zn/0.6))^(1/7)
sv17soil$IPI <- ((sv17soil$As/1.4)+
                   (sv17soil$Ba/6.6)+
                   (sv17soil$Cr/3.6)+
                   (sv17soil$Cu/0.2)+
                   (sv17soil$Mn/8.0)+
                   (sv17soil$Pb/1.1)+
                   (sv17soil$Zn/0.6))/7
#
pcadata <- na.omit(sv17soil[c(1:58,64:73),c(5,6,15:36,38:40)])
summary(pcadata)
ncol(pcadata)
require(rgr)
clrdata <- as.data.frame(clr(pcadata[,3:27]))
clrdata <- cbind(pcadata[,1:2],clrdata)
str(clrdata)
pca0 <- prcomp(~As+ Ba+ Cd+ Ce+ Cr+ Cu+ Gd+ K+ La+ 
                 Mn+ Mo+ Nd+ Ni+ Pb+ Sr+ V+ Y+ Zn, 
               data = clrdata,
               scale. = TRUE)
# ls(pca0)
# [1] "call"     "center"   "rotation" "scale"    "sdev"     "x" 
summary(pca0)
{cat("==== EIGENVALUES ====\n")
pca0$sdev}
{cat("==== ROTATIONS ====\n")
pca0$rotation[,1:6]}
biplot(pca0, col=c(2,3,4))
clrdata$PC1 <- pca0$x[,1]
clrdata$PC2 <- pca0$x[,2]
clrdata$PC3 <- pca0$x[,3]
clrdata$PC4 <- pca0$x[,4]
clrdata$PC5 <- pca0$x[,5]
