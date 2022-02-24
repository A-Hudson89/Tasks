source("http://jonsmitchell.com/code/fxn05.R")
setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_04")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1 : nrow(Pop1), Pop1[,1], ylim = c(0,1), type = "l", xlab = "generation", ylab = "allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col="red")
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h =1, s = 0)
Expectation <- c(10,10,10,10)
Observed <- c(15,15,5,5)
chisq <- sum(((Expectation - Observed)^2)/Expectation)
chisq
Expectation <- c(10,10,10,10)
Observed <- c(5,0,0,35)
chisq <- sum(((Expectation - Observed)^2)/Expectation)
chisq
Expectation <- c(10,10,10,10)
Observed <- c(2,3,10,30)
chisq <- sum(((Expectation - Observed)^2)/Expectation)
chisq
Expectation <- c(10,10,10,10)
Observed <- c(9,7,11,12)
chisq <- sum(((Expectation - Observed)^2)/Expectation)
chisq
Expectation <- c(10,10,10,10)
Observed <- c(10,10,10,10)
chisq <- sum(((Expectation - Observed)^2)/Expectation)
chisq
#The more different the observed become, the further the chi value gets from 0.
results<- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)
length(results)
nrow(results)
ncol(results)
colnames(results)
head(results)
counts <- results[,c("yellow","red","green","blue","black","tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
#The bars become more uneven with increasing chi squared value. The higher the chi squared value observed, the more differnt the to populations are.
Avg <- mean(Chisqs)
Avg
#That all of the populations are significatnly different. The average chi-squared value is significantlly greater than the critical value found in the packet (11.70)
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
backgroundAvgs
#The chi squared value stays close to 60 except for one red background at 15.6
propSig <- length(which(Chisqs > 11.70))/length(Chisqs)
percSig <- round(100 * propSig)
propSig
percSig
#The number was not suprising. Selection is probably not the only thing acting on the chi squared value.
pdf("plots.pdf")
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis = 1)
hist(Chisqs, main = "", xlab = "chi-squared values", ylab = "frequency")
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis = 1)
plot(1, 1, xlim = c(0, 400), ylim = c(1, 8.5), xlab = "", ylab = "", type = "n", yaxt = "n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side = 1, expression(chi^2), cex = 1.75, line = 2.5)
counter <- 1
for(i in backgrounds) {
  Data<- Chisqs[which(results[,3] == i)]
  addHist(Y = counter, Dat = Data, Color = backgroundCol[counter])
  counter <- counter + 1
}
abline(v = 11.70, lty = 2, lwd = 2, col = 'black')
#I would say yes there are meaningful differences in background color.
Simulation <- simDraws(10000)
addHist(Y = 7, Dat = Simulation, Color = "lightgray")
mtext(side = 2, at = 7, line = 0, "simulated")
abline(v = 11.70, lty = 2, lwd = 2)


dev.off()
propSig2 <- length(which(Simulation > 11.70))/length(Simulation)
propSig2
percSig2 <- round(100 * propSig2)
percSig2
#The percentage of time that the selection-free simulation found a meaningful 
  #relationship (>11.70) was 89%. Overall, since there is more than a 5% chance
  #of obtaining proportions different from what we observed, so there is no 
  #meaningful relationship. These numbers could be effected because randomness 
  #is the only thing acting on the population.
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation2, Color = rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation3, Color = rgb(0, 0, 0, 0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation4, Color = rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation5, Color = rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y = 8, Dat = Simulation6, Color = rgb(0, 0, 0, 0.25))
mtext(side = 2, at = 8, line = 0, "sel.sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y = 8, Dat = Simulation7, Color = rgb(0, 0, 1, 0.25))
#The mixture of simulations appears more similar to the student data, although 
  #the peaks at the extreme are more exaggerated.
#Most of the student groups seemed to have evidence of selection with a few stronger
  #instances towards the higher end of the Chi-square values. Natural selection may
  #have been involved in certain colors being favored while other colors were ignored.
#The red and green backgrounds seemed to have the largest effect on selection.
#Natural selection and possible other unknown factors are the evolutionary factors
  #acting on the student lab. Unknown factors could be due to improper conduction
  #and understanding of the lab directions.      
#Natural selection is the evolutionary process for the simulation.
#The graphs tell us that selection is not very strong in the student group data.
#I believe that the comparison between graphs, rather than comparing to a critical
  #value, provides a better understanding of the processes occurring.
#Unexpected mutations would possibly be observed so the chi-square value would increase.