setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Code")
RawData<-read.csv("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Data\\SVMP_Dryad.csv")
head(RawData)
nrow(RawData)
ncol(RawData)
library(phytools)
RawData
DistanceBetween<-RawData[,7]
MetalloActivity<-RawData[,11]
MetalloSerum<-RawData[,12]
SquirrleResist<-RawData[,13]
Diff <- MetalloActivity - MetalloSerum
MetalloActivity[1,11]
?dim
print(MetalloActivity)
MeanElevation <- (RawData$Snake_Elevation + RawData$Squirrel_Elevation) / 2

Model <- lm(Diff~MeanElevation)

#
Model1<-lm(MetalloSerum~RandomElevSquirl)
Model2<-lm(MetalloActivity~RandomElevSnake)
Model3<-lm(MetalloSerum~RawData$Squirrel_Elevation)
Model4<-lm(MetalloActivity~RawData$Snake_Elevation)

Model5<-lm()

par(mar=c(4,4,1,1), las=1, mgp=c(1.25, 0.5, 0), tck=-0.01)
plot(MeanElevation, Diff, pch=16, xlab="mean elevation", ylab="diff. in metallo")
abline(Model, col='red')

plot(RawData$Snake_Elevation, MetalloActivity, col='red', pch=16, cex=1.5, xlab="elevation", ylab="dunno")
points(RawData$Squirrel_Elevation, MetalloSerum, col='blue', pch=17)
abline(Model3,Model4, col='black')

#
plot(RandomElevSnake, MetalloActivity, col='red', pch=16, cex=1.5, xlab="elevation", ylab="dunno")
points(RandomElevSquirl, MetalloSerum, col='blue', pch=17)
abline(Model1,Model2, col='black')

par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(RawData$Snake_Elevation, MetalloActivity, col='red', pch=16, cex=1.5, xlab="elevation", ylab="dunno")
abline(Model4,col='black')
plot(RawData$Squirrel_Elevation, MetalloSerum, col='blue', pch=17)
abline(Model3,col='black')

snakeel<-lm(MetalloActivity ~ RawData$Snake_Elevation)
summary(snakeel)

histosnake<-hist(RawData$Snake_Elevation)
histosquirrel<-hist(RawData$Squirrel_Elevation)

#
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(RandomElevSnake, MetalloActivity, col='red', pch=16, cex=1.5, xlab="elevation", ylab="Metallo Activity")
abline(Model2,col='black')
plot(RandomElevSquirl, MetalloSerum, col='blue', pch=17, xlab='elevation',ylab='Serum Present')
abline(Model1,col='black')

RandomElevSnake<-vector()
RandomElevSquirl<-vector()

for(i in 1:1440) {
  RandomElevSnake<-runif(1440,0,810)
  RandomElevSquirl<-runif(1440,0,763)
}
max(RandomElevSnake)
RandomElevSnake
RandomElevSqirl
warnings()
RandomElev

?runif