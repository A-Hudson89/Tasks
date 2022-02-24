setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_05")
library(coala)
library(phytools)
library(MASS)
learnPopGen::coalescent.plot()
?coalescent.plot()
coalescentPlot1 <- learnPopGen::coalescent.plot(n = 10, ngen = 20, colors = NULL )
coalescentPlot2 <- learnPopGen::coalescent.plot(n = 20, ngen = 30, colors = NULL )
coalescentPlot3 <- learnPopGen::coalescent.plot(n = 30, ngen = 40, colors = NULL )
coalescentPlot4 <- learnPopGen::coalescent.plot(n = 10, ngen = 5, colors = NULL )
coalescentPlot5 <- learnPopGen::coalescent.plot(n = 5, ngen = 5, colors = NULL )
coalescentPlot5
pdf("10coalescentplot1.pdf", height = 4, width = 4)
plot(coalescentPlot1)
dev.off()
pdf("20coalescentplot2.pdf", height = 4, width = 4)
plot(coalescentPlot2)
dev.off()
pdf("30coalescentplot3.pdf", height = 4, width = 4)
plot(coalescentPlot3)
dev.off()
pdf("coalescentplot4.pdf", height = 4, width = 4)
plot(coalescentPlot4)
dev.off()
pdf("coalescentplot5.pdf", height = 4, width = 4)
plot(coalescentPlot5)
dev.off()
meanFix <- c(1,1,1,2,5)
mean(meanFix)
fixGen <- c(1,1,1,2,5)
var(fixGen)
meanKid <- c(0,0,0,3,2)
mean(meanKid)
varKid <- c(0,0,0,3,2)
var(varKid)
#1. Each simulation begins with 10, 20, or 30 alleles. You can modify it by changing n in 
  #the coalescent.plot().
#2.Avg. time to fixation= 2 generation (for n=5 ngen=5)
  #Variance= 3 (for n=5 ngen=5)
#3.Avg. offspring of each individual= 1 (for n=5 ngen=5)
  #Variance= 2 (for n=5 ngen=5)
#4.It does not because the selection is at random.
#5.No
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, 
  ploidy = 2) + feat_mutation(10) +
  feat_recombination(15) +
  sumstat_trees() +
  sumstat_nucleotide_div()
model
stats <- simulate(model, nsim = 1)
stats
Diversity <- stats$pi
Diversity
#All of the numbers are different. Mutation and recombination could be effecting
  #diversity.
Nloci <- length(stats$trees)
Nloci
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#6.Each of the numbers possibly share a common ancestor. 
Agel <- max(nodeHeights(t1))
Agel
t2 <- read.tree(text=stats$trees [[2]][1])
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
Agel2 <- max(nodeHeights(t2))
Agel2
#The most recent common ancestor for t2 is much younger than t1.
#7. No they do not match. the most recent ancestor for t2 is much younger that t1. 
for(locus in 1:Nloci){
  ntrees<- length(stats$trees[[locus]])
  for(n in 1:ntrees) {
    if(locus == 1 && n == 1){
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
AllSNP1 <- densityTree(outPhy)
#prediction: If I increase the rate of recombination, it should increase 
  #diversity in loci over time and increase the average age in the results.
#results: It seems to be as I predicted
model3 <- coal_model(10,50)+
  feat_mutation(par_prior("theta", sample.int(100,1)))+
  sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi, theta, type = "p", xlab="mean_pi", ylab="theta")
LineA <- rlm(mean_pi~theta)
abline(LineA)


?feat_migration
?feat_mutation
?coala
help(coala)
?coal_model
  

#Extra Credit!!

modelA <- coal_model(c(4,9),15) + 
  feat_mutation(par_prior("theta", sample.int(100,1))) + 
  feat_migration(0.5, 1, 2) +
  feat_migration(1.0, 2, 1) +
  feat_growth(5, time=0) + 
  feat_growth(8, time=1) + 
  feat_selection(strength_A = 1, population = 2, time = 1, locus_group = "all") +
  feat_selection(strength_Aa = 0.2, population = 1, time = 0, locus_group = "all") +
  sumstat_nucleotide_div()
output <- simulate(modelA, nsim= 100)
Pis <- sapply(output, function(x) x$pi)
Pis  
  
  
  
  check_model(modelA)
activate_msms(jar = NULL, java = NULL, priority = 200, download = TRUE)