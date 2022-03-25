setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Data")
DataRate <- read.csv('C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Data\\Rate.csv', header = TRUE, stringsAsFactors = FALSE)
DataDuration <- read.csv('C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Data\\Duration.csv', header = TRUE, stringsAsFactors = FALSE)
DataProbability <- read.csv('C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Data\\Probability.csv', header = TRUE, stringsAsFactors = FALSE)
DataDuration

par(mfrow=c(1,3), mar=c(4,4,1,1), pch=16)
plot(DataProbability[,2], DataProbability[,3], xlab="relatedness to rattlesnake", ylab="probability")
plot(DataDuration[,2], DataDuration[,3], xlab="relatedness to rattlesnake", ylab="duration")
plot(DataRate[,2], DataRate[,3], xlab="relatedness to rattlesnake", ylab="rate")


head(DataRate)
SquamateTree <- read.tree("Pyron.tre")

Relatedness <- vcv(SquamateTree)

Rattlesnake <- grep("Crotalus_horridus", rownames(Relatedness))
PrunedRelated <- Relatedness[Rattlesnake,]

ChosenRelated <- PrunedRelated[DataRate$Species]

Missing <- setdiff(DataRate$Species, names(PrunedRelated))

names(PrunedRelated)[grep("mattogrossensis", names(PrunedRelated))]

par(las=1)

Phylo <- which(DataRate$Species)
DataPhylo <- DataRate[Phylo,]
DataPhylo
SpeciesNames <- DataRate["Species"]
DropFromTree <- setdiff(SquamateTree$tip.label, SpeciesNames$Species)
InSpeciesNames <- intersect(SpeciesNames$Species, SquamateTree$tip.label)
Tree2 <- drop.tip(SquamateTree, DropFromTree)
Tree2
plot(Tree2, cex=0.25)
?plot.phylo
which?