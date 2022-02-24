trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2)
size <- 50
sample1 <- sample(population1, size)
sample2 <- sample(population2, size)
sample1
sample2
population1
population2
The samples and the populations were both different
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_dad")
PatGrandpa <- makeFounder("grandpa_dad")
head(MatGrandma)
head(MatGrandpa)
nrow(MatGrandma)
head(PatGrandma)
nrow(PatGrandma)
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
Maybe half of the genes are shared
ToMom <- length(grep("mom", Focus))/length(Focus)
ToMom
ToMomMom <- length(grep("grandma_mom", Focus))/length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)
ToMomMom == ToMomDad
ToDadDad <- length(grep("grandma_dad", Focus))/length(Focus)
ToDadMom <- length(grep("grandpa_dad", Focus))/length(Focus)
ToDadDad == ToDadMom
AvgRelateTotal <- length(grep("grandma_mom", "grandpa_mom", "grandma_dad", "grandpa_dad", Focus))/length(Focus)
AvgRelateTotal
Focus is not equally related to either maternal or paternal grandparents.It is very close to half however so it close to what I was expecting. The average relatedness of Focus to all grandparents is 100%.
Sibling_01 <- makeBaby(Brenda, Alan)
I would expect Focus and their sibling to share half DNA. It is actually a litte more than half.
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
ToSib
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
ManySiblings
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main = "", xlab = "proportion shared genes")
The plot represents the % relatedness of each of the 1000 children concieved by Brenda and Alan. The peak towards the center shows the decendents most related and moving towards either end (left or right) represnets the decendents least related to Brenda and Alan
HWE <- function(p) {
  aa <- p^2
  ab <- 2 * p * (1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p,HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
Time nor geographic space are considered in this graph. As the frequency of allele a increases so does the genotypic frequency. As it decreases, so does the genotypic frequency
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
It is close to the Hardy-Weinburg equation but not equal.
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")

install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=2000, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x,500)))
Line <- lm(tExt ~ Samples)
LineA <- rlm(tExt ~ Samples)
Line2 <- lm(tExt ~ Samples + 0)
summary(Line)
Line$coef
Line2$coef
plot(Samples, tExt)
abline(Line)
abline(LineA)
abline(Line2)
the + 0 removes the intercept and only shows the Samples
The points seem to get further away from the line as the population increases. It seems to loose consistency with increased sampling which would mean that the occurence increases with the number of samples 
install.packages("lmtest")
library(lmtest)
bptest(Line)
bptest(Line2)
bptest(LineA)
install.packages("MASS")
library(MASS)
LineA <- rlm(tExt ~ Samples)
abline(LineA)
LineA$coef
summary(LineA)
summary(Line)