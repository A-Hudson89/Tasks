setwd('C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_02')
data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(data, 'rawdata.csv', quote = F)
data
length(data)
nrow(data)
ncol(data)
head(data)
data[1,]
data[2,]
data[1:3,]
data[1:3, 4]
data[1:5, 1:3]
  Each subset of data above shows [start:end, column#start:column#end].
data[257,]
  data[257,] would yeild the 257th observation.
Feed <- which(data[,9] == "bottle")
berenmilk <- data[Feed,]
head(berenmilk)
Feeds <- which(data[, 'event'] == "bottle")
Feeds <- which(data$event == "bottle")
BerenMilk <- data[Feeds,]
head(BerenMilk)
berenMilk <- data[Feeds,]
head(berenMilk)
  They all pull up the same specified data when I opened them. 'event' specifies which column (9) and 'bottle' specifies which specific event we are looking for in that column.
dayID <- apply(data, 1, function(x) paste(x[1:3], collapse = '-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
data$age <- dateID - dateID[which(data$event == 'birth')]
head(data)
beren2 <- data
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote = F, row.names = FALSE)