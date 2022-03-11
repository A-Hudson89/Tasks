setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_08")
text.string <-
  "(((((((cow, pig), whale), (bat, (lemur, human))),(robin, iguana)), coelacanth),
(gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree,edge.width=2)
nodelabels(frame= "circle", bg= 'white', cex= 1)
#1 A shark
vert.tree
#2 There are no branch lengths in this tree
str(vert.tree)
tree <- read.tree(text= "(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col= 'black', border= 'white', main= "", xlab= "edge lengths for the anolis tree", ylim= c(0, 50), xlim= c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

?plot.phylo
install.packages("base")
library(base)
#3
plot(AnolisTree, cex=0.25, show.tip.label= FALSE)
#4
plot(AnolisTree, type= "radial", cex=0.25, show.tip.label= FALSE)
#5
plot(AnolisTree, type= "radial", cex=0.25, tip.color= "red")
?which
plot(AnolisTree, cex)
#6-8
which.min(AnolisTree$edge.length)
AnolisTree2 <- drop.tip(AnolisTree, 82)
plot(AnolisTree2, cex=0.25)

ltt(AnolisTree)
abline(0,1,lwd=2,col='red',lty=2)
#9
#a. It increases somewhat exponentially
#b. No
#c. It can never go down because the past lineages still exist. A massive extinction event could stop it from increasing.
#d. No the slope varies over time and appears to be stabilizing and it can never be negative.
#e. The diversity increases over time with periods of stabilization.

?fit.bd
bd.fitAnolis <- fit.bd(AnolisTree, rho=0.2)
print(bd.fitAnolis)

#10.
#a. b= lambda-mu = 0.8031
#b. d= mu/lambda = 0


#ExtraCredit
?treebase
install.packages("rphylotastic")
install.packages("treeBase")
dolphins <- search_treebase('"Delphinus"', by="taxon", max_trees=1, branch_lengths=TRUE, exact_match=TRUE)
?search_treebase
install.packages("diversitree")
library(diversitree)
?diversitree
install.packages("rvertnet")
library(rvertnet)
?rvertnet
searchbyterm(class="Aves")
install.packages("rphylotastic")
PrimateTree <- read.nexus("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Extra credit")
par(las=1)

setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Extra credit")
LemurTree <- read.nexus("TreeBlock_10kTrees_Lemurs_Version3.nex")
par(las=1)
plot(LemurTree, cex=0.25)
bd.fitLemurTree <- fit.bd(LemurTree, rho=0.2)
print(bd.fitLemurTree)
which(LemurTree$1)
LemurTree
head(LemurTree)
LemurTree[0:1]
LemurTree1 <- LemurTree[0:1]
LemurTree1
plot(LemurTree1, cex=0.25)
bd.fitLemurTree1 <- force.ultrametric(fit.bd(LemurTree1, rho=0.2))
print(bd.fitLemurTree1)