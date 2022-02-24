Question 1= Hypothesis 1 tries to test data that does not exist (how much Beren eats) so it is unusable. Hypothesis 2 is too vauge in the wording with ammount, and with the relationship. Hypothesis 2 could be changed to work. 
  The best hypothesis is Hypothesis 3. While it could be more descriptive, it clearly states two variables that are being tested against eachother.
setwd('C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_02')
data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(data, 'rawdata.csv', quote = F)
data
Feed <- which(data[,9] == "bottle")
berenmilk <- data[Feed,]
Feeds <- which(data[, 'event'] == "bottle")
Feeds <- which(data$event == "bottle")
BerenMilk <- data[Feeds,]
berenMilk <- data[Feeds,]
dayID <- apply(data, 1, function(x) paste(x[1:3], collapse = '-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
data$age <- dateID - dateID[which(data$event == 'birth')]
head(data)
beren2 <- data
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote = F, row.names = FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk
avgMilk units are 'oz'.
the ammount of milk fed in oz.
It is important. It specifies which vector we are using.
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
head(beren3$value)
beren3
head(beren3$age)
beren3[257,]
beren3[180,]
head(beren3$value[Feeds])
head(beren3$age[Feeds])
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
berenANOVA
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par
las=style of x axis, mar=# lines of margin on 4 sides, mpg= margin lines for axis title, axis labels, and axis line, tck= length of tick marks as a fraction of width/height of plotting area.
par(las= 1, mar= c(5,5,1,1), mgp= c(2,0.5,0), tck= -0,01)
plot(as.numeric(names(totalFeed)), totalFeed, type= "b", pch= 16, xlab= "age in days", ylab= "ounces of milk")
abline(h=mean(totalFeed), lty= 2, col= "red")
pdf("r02b-totalMilkByDay.pdf", height= 4, width= 4)
par(las= 1, mar= c(5,5,1,1), mgp= c(2,0.5,0), tck= -0,01)
plot(as.numeric(names(totalFeed)), totalFeed, type= "b", pch= 16, xlab= "age in days", ylab= "ounces of milk")
abline(h=mean(totalFeed), lty= 2, col= "red")
dev.off()
Question 2= towards a certian age, the child starts to rely less on milk for nutrients and consumes other foods/drinks. The graph needs to include other foods or be scaled for just milk. It also needs a title.
beren3
pdf("r02b-cummulativeMilkByTime.pdf", height= 4, width= 4)
unique(beren3$event)
Hypothesis= The circumference of Berens head increases with his age.
traitHeadCircum <- which(beren3$event == "trait_head_circum")
traitHeadCircum
avgHeadCircum <- mean(beren3$value[traitHeadCircum])
avgHeadCircum
cor.test(beren3$value[traitHeadCircum], beren3$age[traitHeadCircum])
?cor.test
t.test(beren3$value[traitHeadCircum], beren3$age[traitHeadCircum])
naps <- which(beren3$event == "nap")
beren4 <- beren3[naps,]
beren4
beren4[,8] + beren4[,7]
napStart <- tapply(beren4$start_hour, beren4$start_minute)
time1 <- "2019-08-22 12:30"
time2 <- "2019-08-22 13:30"
Aug22napTime <- difftime(time2, time1, units = "hours")
Aug22napTime
time3 <- "2019-08-23 8:55"
time4 <- "2019-08-23 9:36"
Aug23napTime <- difftime(time4, time3, units = "hours")
time5 <- "2019-08-23 11:52"
time6 <- "2019-08-23 12:46"
Aug23napTime2 <- difftime(time6, time5, units = "hours")
time7 <- "2019-08-26 8:40"
time8 <- "2019-08-26 10:10"
Aug26napTime <- difftime(time8, time7, units = "hours")
time9 <- "2019-08-26 12:45"
time10 <- "2019-08-26 13:20"
Aug26napTime2 <- difftime(time10, time9, units = "hours")
time11 <- "2019-08-27 11:10"
time12 <- "2019-08-27 12:40"
Aug27napTime <- difftime(time12, time11, units = "hours")
time13 <- "2019-08-27 14:10"
time14 <- "2019-08-27 14:45"
Aug27napTime2 <- difftime(time14, time13, units = "hours")
time15 <- "2019-08-30 7:30"
time16 <- "2019-08-30 8:42"
Aug30napTime <- difftime(time16, time15, units = "hours")
time17 <- "2019-08-30 10:21"
time18 <- "2019-08-30 11:34"
Aug30napTime2 <- difftime(time18, time17, units = "hours")
time19 <- "2019-08-30 14:27"
time20 <- "2019-08-30 14:37"
Aug30napTime3 <- difftime(time20, time19, units = "hours")
time21 <- "2019-09-04 8:04"
time22 <- "2019-09-04 9:19"
Sept4napTime <- difftime(time22, time21, units = "hours")
time23 <- "2019-09-04 11:07"
time24 <- "2019-09-04 12:46"
Sept4napTime2 <- difftime(time24, time23, units = "hours")
time25 <- "2019-09-05 08:00"
time26 <- "2019-09-05 09:19"
Sept5napTime <- difftime(time26, time25, units = "hours")
time27 <- "2019-09-05 12:15"
time28 <- "2019-09-05 12:33"
Sept5napTime2 <- difftime(time28, time27, units = "hours")
time29 <- "2019-09-05 12:55"
time30 <- "2019-09-05 14:17"
Sept5napTime3 <- difftime(time30, time29, units = "hours")
time31 <- "2019-09-06 08:48"
time32 <- "2019-09-06 09:42"
Sept6napTime <- difftime(time32, time31, units = "hours")
time33 <- "2019-09-06 11:34"
time34 <- "2019-09-06 12:10"
Sept6napTime2 <- difftime(time34, time33, units = "hours")
time35 <- "2019-09-06 14:10"
time36 <- "2019-09-06 14:40"
Sept6napTime3 <- difftime(time36, time35, units = "hours")
time37 <- "2019-09-09 08:46"
time38 <- "2019-09-09 09:43"
Sept9napTime <- difftime(time38, time37, units = "hours")
time39 <- "2019-09-09 12:38"
time40 <- "2019-09-09 13:23"
Sept9napTime2 <- difftime(time40, time39, units = "hours")
time41 <- "2019-09-10 09:00"
time42 <- "2019-09-10 09:40"
Sept10napTime <- difftime(time42, time41, units = "hours")
time43 <- "2019-09-10 12:00"
time44 <- "2019-09-10 12:35"
Sept10napTime2 <- difftime(time44, time43, units = "hours")
time45 <- "2019-09-11 09:49"
time46 <- "2019-09-11 10:36"
Sept11napTime <- difftime(time46, time45, units = "hours")
time47 <- "2019-09-11 11:49"
time48 <- "2019-09-11 12:05"
Sept11napTime2 <- difftime(time48, time47, units = "hours")
time49 <- "2019-09-11 14:16"
time50 <- "2019-09-11 14:45"
Sept11napTime3 <- difftime(time50, time49, units = "hours")
time51 <- "2019-09-12 09:08"
time52 <- "2019-09-12 09:46"
Sept12napTime <- difftime(time52, time51, units = "hours")
time53 <- "2019-09-12 12:07"
time54 <- "2019-09-12 13:05"
Sept12napTime2 <- difftime(time54, time53, units = "hours")
time55 <- "2019-09-13 08:22"
time56 <- "2019-09-13 09:17"
Sept13napTime <- difftime(time56, time55, units = "hours")
time57 <- "2019-09-13 11:45"
time58 <- "2019-09-13 12:15"
Sept13napTime2 <- difftime(time58, time57, units = "hours")
time59 <- "2019-09-16 12:15"
time60 <- "2019-09-16 12:53"
Sept16napTime <- difftime(time60, time59, units = "hours")
time61 <- "2019-09-18 10:40"
time62 <- "2019-09-18 11:02"
Sept18napTime <- difftime(time62, time61, units = "hours")
time63 <- "2019-09-18 14:35"
time64 <- "2019-09-18 14:45"
Sept18napTime2 <- difftime(time64, time63, units = "hours")
time65 <- "2019-09-20 09:40"
time66 <- "2019-09-20 10:23"
Sept20napTime <- difftime(time66, time65, units = "hours")
time67 <- "2019-09-20 13:21"
time68 <- "2019-09-20 14:14"
Sept20napTime2 <- difftime(time68, time67, units = "hours")
time69 <- "2019-09-23 11:17"
time70 <- "2019-09-23 11:32"
Sept23napTime <- difftime(time70, time69, units = "hours")
time71 <- "2019-09-24 10:35"
time72 <- "2019-09-24 11:20"
Sept24napTime <- difftime(time72, time71, units = "hours")
time73 <- "2019-09-24 14:21"
time74 <- "2019-09-24 15:03"
Sept24napTime2 <- difftime(time74, time73, units = "hours")
time75 <- "2019-09-25 08:40"
time76 <- "2019-09-25 09:30"
Sept25napTime <- difftime(time76, time75, units = "hours")
time77 <- "2019-09-30 09:36"
time78 <- "2019-09-30 10:10"
Sept30napTime <- difftime(time78, time77, units = "hours")
time79 <- "2019-10-02 08:57"
time80 <- "2019-10-02 10:14"
Oct2napTime <- difftime(time80, time79, units = "hours")
time81 <- "2019-10-02 13:23"
time82 <- "2019-10-02 14:00"
Oct2napTime2 <- difftime(time82, time81, units = "hours")
time83 <- "2019-10-03 09:10"
time84 <- "2019-10-03 10:25"
Oct3napTime <- difftime(time84, time83, units = "hours")
time85 <- "2019-10-23 07:52"
time86 <- "2019-10-23 09:10"
Oct4napTime <- difftime(time86, time85, units = "hours")
time87 <- "2019-10-04 14:09"
time88 <- "2019-10-04 15:20"
Oct4napTime2 <- difftime(time88, time87, units = "hours")
time89 <- "2019-10-07 09:10"
time90 <- "2019-10-07 09:43"
Oct7napTime <- difftime(time90, time89, units = "hours")
time91 <- "2019-10-07 12:28"
time92 <- "2019-10-07 13:22"
Oct7napTime2 <- difftime(time92, time91, units = "hours")
time93 <- "2019-10-08 09:23"
time94 <- "2019-10-08 10:00"
Oct8napTime <- difftime(time94, time93, units = "hours")
time95 <- "2019-10-08 12:15"
time96 <- "2019-10-08 12:40"
Oct8napTime2 <- difftime(time96, time95, units = "hours")
time97 <- "2019-10-08 14:45"
time98 <- "2019-10-08 15:10"
Oct8napTime3 <- difftime(time98, time97, units = "hours")
time99 <- "2019-10-09 09:20"
time100 <- "2019-10-09 10:55"
Oct9napTime <- difftime(time100, time99, units = "hours")
time101 <- "2019-10-10 09:18"
time102 <- "2019-10-10 09:40"
Oct10napTime <- difftime(time102, time101, units = "hours")
time103 <- "2019-10-15 09:30"
time104 <- "2019-10-15 10:00"
Oct15napTime <- difftime(time104, time103, units = "hours")
time105 <- "2019-10-15 11:35"
time106 <- "2019-10-15 12:15"
Oct15napTime2 <- difftime(time106, time105, units = "hours")
time107 <- "2019-10-16 09:05"
time108 <- "2019-10-16 09:45"
Oct16napTime <- difftime(time108, time107, units = "hours")
time109 <- "2019-10-16 13:30"
time110 <- "2019-10-16 14:10"
Oct16napTime2 <- difftime(time110, time109, units = "hours")
time111 <- "2019-10-17 09:21"
time112 <- "2019-10-17 09:52"
Oct17napTime <- difftime(time112, time111, units = "hours")
time113 <- "2019-10-17 14:12"
time114 <- "2019-10-17 14:45"
Oct17napTime2 <- difftime(time114, time113, units = "hours")
time115 <- "2019-10-18 09:10"
time116 <- "2019-10-18 09:58"
Oct18napTime <- difftime(time116, time115, units = "hours")
time117 <- "2019-10-18 13:01"
time118 <- "2019-10-18 13:30"
Oct18napTime2 <- difftime(time118, time117, units = "hours")
time119 <- "2019-10-21 09:27"
time120 <- "2019-10-21 10:00"
Oct21napTime <- difftime(time120, time119, units = "hours")
time121 <- "2019-10-21 13:00"
time122 <- "2019-10-21 13:30"
Oct21napTime2 <- difftime(time122, time121, units = "hours")
time123 <- "2019-10-22 09:17"
time124 <- "2019-10-22 10:30"
Oct22napTime <- difftime(time124, time123, units = "hours")
time125 <- "2019-10-22 13:10"
time126 <- "2019-10-22 13:25"
Oct22napTime2 <- difftime(time126, time125, units = "hours")
time127 <- "2019-10-23 09:00"
time128 <- "2019-10-23 09:45"
Oct23napTime <- difftime(time128, time127, units = "hours")
time129 <- "2019-10-23 11:32"
time130 <- "2019-10-23 12:55"
Oct23napTime2 <- difftime(time130, time129, units = "hours")
