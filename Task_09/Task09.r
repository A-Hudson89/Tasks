setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_09")
library(phytools)
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col= 'black', border= 'white', main= "", xlab= "edge lengths for the anolis tree", ylim= c(0, 50), xlim= c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, type="fan")
print(AnolisTree)

#1 It does not have branch lenghts present and there are 82 tips.
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
dim(data)
typeof(data)
nrow(data)
#2 It is a list in svl format. 82 rows and 1 column.
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(AnolisTree, svl, vars= TRUE, CI= TRUE)
?fastAnc
#3Estimated node values. CI95 represents the variance with a 95% confidence interval.
#4State compound of root node is equal to the root node. Also, it assumes that all the data being tested is within a 95% confidence interval
par(mar=c(0.1,0.1,0.1,0.1))
plot(AnolisTree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[AnolisTree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(AnolisTree,svl,plot=F)
plot(obj, type="fan", legend= 0.7*max(nodeHeights(AnolisTree)), sig= 2, fsize= c(0.7,0.9))
fossilData <- data.frame(svl = log(c(25.4,23.2,17.7,19.7,24,31)), tip1 = c("Anolis_aliniger","Anolis_aliniger","Anolis_occultus","Anolis_ricordii","Anolis_cristatellus","Anolis_occultus"), tip2 = c("Anolis_chlorocyanus","Anolis_coelestinus","Anolis_hendersoni","Anolis_cybotes","Anolis_angusticeps","Anolis_angusticeps"))
#5
fossilNodes<-c()
nodeN<-c()
for(i in 1:6){
Node<-fastMRCA(AnolisTree, fossilData[i,"tip1"], fossilData[i,"tip2"])
fossilNodes[i]<-fossilData[i,"svl"]
nodeN[i]<-Node
}
#7 it increased the overall size
names(fossilNodes) <- nodeN
Ancestors_withFossils<- fastAnc(AnolisTree, svl, anc.states = fossilNodes, CI=TRUE, var=TRUE)
plot(Ancestors$ace,Ancestors_withFossils$ace, col= "blue", xlab= "Ancestral State w/ Fossils", ylab= "Ancestral state")
?fastAnc
print(fossilData)
#8
install.packages("geiger")
library(geiger)
lambdaFit <- fitContinuous(AnolisTree,data,model="lambda")
#AIC=-4.51
brownianFit <- fitContinuous(AnolisTree,data,model="BM")
#AIC=-6.51
ouFit <- fitContinuous(AnolisTree,data,model="OU")
#AIC=-4.51
ebFit <- fitContinuous(AnolisTree,data,model="EB")
#AIC=-7.24
ratetrendFit <- fitContinuous(AnolisTree,data,model="rate_trend")
#AIC=-6.98
kappaFit <- fitContinuous(AnolisTree,data,model="kappa")
#AIC=-4.51
deltaFit <- fitContinuous(AnolisTree,data,model="delta")
#AIC=-6.11
meantrendFit <- fitContinuous(AnolisTree,data,model="mean_trend")
#AIC=-4.51
whiteFit <- fitContinuous(AnolisTree,data,model="white")
#AIC=91.39
#8 ebFit is the best model displaying the lowest AIC. fastAnc uses estimates of current data and fitcontinuous uses continuous data to compare.
#both assume the same confidence intervals and assume continuous traits