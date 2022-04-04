setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_10")
library(phytools)
trees<-list()
births<-vector()
Fractions<-vector()
netdiversification<-vector()
speciationrate<-vector()
avgbranchlength<-vector()
for(i in 1:100) {
  births[i]<-runif(1,0,1)
  Fractions[i]<-runif(1,0,1)
  trees[[i]]<-pbtree(n=100, b=births[i], d=Fractions[i] * births[i])
  netdiversification[i] <- (births[i]-Fractions[i]*births[i])
  speciationrate[i] <- births[i]
  avgbranchlength[[i]] <- mean(trees[[i]]$edge.length)
}
print(trees)
print(trees[[i]])
?rnorm
?sample
sample(births)
#4 As net diversification increases so does the log of total tree tips
netdiversification<-(births-Fractions*births)
totaltreetips<-log(sapply(trees,Ntip))
plot(netdiversification, totaltreetips)
Q4plot<-plot(netdiversification, totaltreetips)
line<-lm(totaltreetips ~ netdiversification)
abline(line)
#5 speciation rate increases with decreasng avg. branch length
plot(speciationrate, avgbranchlength)
#6 
cor(speciationrate, avgbranchlength)
#7
trees
trees[90]
Tree<-trees[[90]]
rates<-vector()
traits<-list()
plot(Tree)
for(i in 1:100){
  rates[i]<-runif(1)
  traits[[i]]<-fastBM(Tree, sig2=rates[i])
}
MeanTrait<-sapply(traits,mean)
CorRateTrait<-cor(MeanTrait,rates)
plot(MeanTrait,rates)
dev.off()
#8 there is no correlation between trait means and rates
Vtraits<-sapply(traits,var)
CorVrates<-cor(Vtraits,rates)
plot(Vtraits,rates)
dev.off()
#9 The variance of traits and the rates show a positive correlation.
E1<-sapply(traits,"[[",1)
E2<-sapply(traits,"[[",2)
ElementCor<-cor(E1,E2)
plot(E1,E2)
dev.off()
#10 A slight positive correlation with no significance is observed which is in 
#part due to the pylo tree relatedness.
# You have to account for phylogeny because things considered living, can not have 
#independent variables. This shows that all traits are correlated in some fashion
#but does not make the correlation meaningful.

#Extra Credit
ExCrdTree<-pbtree(n=100)
X<-fastBM(ExCrdTree,nsim=2)
ExCreditPlot<-phylomorphospace(ExCrdTree,X,xlab="E1",ylab="E2")
pdf("ExCreditPlot.pdf")
?pdf
?write.csv
head(ExCreditPlot)
?write.csv