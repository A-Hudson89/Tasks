setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Code")
RawData<-read.csv("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Data\\SVMP_Dryad.csv")

MetalloActivity<-RawData[,11]
MetalloSerum<-RawData[,12]
SquirrelElevation<-RawData[,6]
SnakeElevation<-RawData[,5]
SnakeElevation
MetalloActivity
A<- RawData[1,] RawData[2,]
?filter
Nsim <- 1
RandomSnakeElevation <- matrix(0, nrow=length(SnakeElevation), ncol=Nsim)
RandomSquirrelElevation <- matrix(0, nrow=length(SquirrelElevation), ncol=Nsim)

for(i in 1:Nsim) {
  RandomSnakeElevation[,i]<-sample(c(SquirrelElevation,SnakeElevation), length(SnakeElevation), replace=T)
  RandomSquirrelElevation[,i]<-sample(c(SquirrelElevation,SnakeElevation), length(SquirrelElevation), replace=T)
}


PearTestSnake<-cor.test(MetalloActivity,SnakeElevation)
PearTestSquirrel<-cor.test(MetalloSerum,SquirrelElevation)
PearTestRandomSnake<-cor.test(MetalloActivity,RandomSnakeElevation)
PearTestRandomsquirrel<-cor.test(MetalloSerum,RandomSquirrelElevation)
print(PearTestSnake)
print(PearTestSquirrel)
print(PearTestRandomSnake)
print(PearTestRandomsquirrel)

CorTestS<-data.frame(cbind(MetalloActivity, SnakeElevation))
CorTestSQ<-data.frame(cbind(MetalloSerum, SquirrelElevation))

jpeg("Figure3.jpg",height=250,width=400)
ggscatter(CorTestS,"SnakeElevation", "MetalloActivity", color="blue", fill="white", 
          add = "reg.line", conf.int = TRUE, title="Venom Strength vs. Snake Elevation", 
          cor.coef = TRUE, cor.method = "pearson", shape=2,
          xlab = "Elevation", ylab = "Venom Strenght")+border("black")
dev.off()
jpeg("Figure4.jpg",height=250,width=400)
ggscatter(CorTestSQ,"SquirrelElevation","MetalloSerum", color="red", fill="white",
                 add = "reg.line", conf.int = TRUE, title="Venom Resistance vs. Squirrel Elevation ",
                 cor.coef = TRUE, cor.method = "pearson", shape=1,
                 xlab = "Elevation", ylab = "Venom Resistance")+border("black")
dev.off()
?jpeg
AnovaSnake<-aov(MetalloActivity~SnakeElevation)
AnovaSquirrel<-aov(MetalloSerum~SquirrelElevation)
AnovaRandomSnake<-aov(MetalloActivity~RandomSnakeElevation)
AnovaRandomSquirrel<-aov(MetalloSerum~RandomSquirrelElevation)
summary(AnovaSnake)
summary(AnovaSquirrel)
summary(AnovaRandomSnake)
summary(AnovaRandomSquirrel)

Model1<-lm(MetalloSerum~SquirrelElevation)
Model2<-lm(MetalloActivity~SnakeElevation)
Model3<-lm(MetalloSerum~RandomSquirrelElevation)
Model4<-lm(MetalloActivity~RandomSnakeElevation)
summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)

jpeg("Figure1.jpg",height=400,width=600)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(SquirrelElevation, MetalloSerum, col='red', pch=1, cex=1.5, xlab="Elevation", ylab="Venom Resistance")
title(main="Squirrel Resistance vs. Elevation")
abline(Model1,col='black')
plot(SnakeElevation, MetalloActivity, col='blue', pch=2, cex=1.5, xlab="Elevation", ylab="Venom Strength")
title(main="Venom Strength vs. Elevation")
abline(Model2,col='black')
dev.off()

jpeg("Figure2.jpg",height=400,width=700)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(RandomSquirrelElevation[,1], MetalloSerum, col='red', pch=1, cex=1.5, xlab="Elevation", ylab="Venom Resistance")
title(main="Squirrel Resistance vs. Random Elevation")
abline(Model3,col='black',lwd=3)
plot(RandomSnakeElevation[,1], MetalloActivity, col='blue', pch=2, cex=1.5, xlab="Elevation", ylab="Venom Strength")
title(main="Venom Strength vs. Random Elevation")
abline(Model4,col='black',lwd=3)
dev.off()

?abline
?jpeg
?plot
?ggplot
install.packages("ggplot2")
library(ggpubr)
?ggscatter
?dplyr