setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_11")
#1
x<-rnorm(100,mean=5,sd=2)
y<-(x*5)+2+runif(100,0,0.1)
MODELxy<-lm(y~x)
plot(x,y)
MODELxy
coef(lm(y~x))
#Intercept= 2.059153, Slope= 4.998577
#Why? the slope is positive due to they y values being created by x being multiplied and added to. 

z <- c()
for(i in 1:100) {
  z[i] <- runif(1, 0, 1)
  y <- ((x*z[i])+2)+runif(100, 0, 0.1)
  mb <- coef(lm(y~z[1:100]))
}
mb
plot(z, z[1:100])
# That there is not much difference between this plot and the first plot.

#2
n <- 10000
prize <- sample(c("A", "B", "C"), size=n, replace=TRUE)
doorOpened <- ifelse(prize=="A", sample(c("B", "C"), size=n, replace=TRUE), ifelse(prize=="B", "C", "B"))
doorUnopened <- ifelse(doorOpened=="B", "C", "B")
StayWinChance <- sum(prize=="A")/n
SwitchWinChance <- sum(prize==doorUnopened)/n
?barplot
WinChance <- c(StayWinChance,SwitchWinChance)
barplot(WinChance, names.arg = c("StayWin", "SwitchWin"), ylab="Chance of Winning", main="Monty Hall Odds", col="Yellow")


#3
library(meme)

Meme<-"https://i.imgflip.com/n967c.jpg"
Meme2<-meme(Meme,size=1.5, "I don't always mate\noutside of my breeding pool","But when I do,\nI increase my effective population size")
plot(Meme2)
