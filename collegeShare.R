library(ggplot2)#loads a graphing library to produce charts
library(plyr)
library(grid)
library(rpart)
library(e1071)
library(kknn)
library(ROCR)
#Nicole: hello

setwd("https://github.com/patrickdec/198.git")
deptOfEdu <- read.csv("Cleaned-Most-Recent-Cohorts-Treasury-Elements.csv",header = TRUE, sep = ",", dec = ".")
deptOfEdu <- deptOfEdu[2:3]
deptOfEdu <- deptOfEdu[deptOfEdu$unemp_rate != 'NULL',]
deptOfEdu$unemp_rate <- as.numeric(levels(deptOfEdu$unemp_rate))[deptOfEdu$unemp_rate]
deptOfEdu <- deptOfEdu[!is.na(deptOfEdu$unemp_rate),]

TimesRank <- read.csv("timesData.csv",header = TRUE, sep = ",", dec = ".")
TimesRank <- TimesRank[c("university_name", "total_score")]
TimesRank <- TimesRank[TimesRank$total_score != '-',]
TimesRank$total_score <- as.numeric(levels(TimesRank$total_score))[TimesRank$total_score]
TimesRank <- ddply(TimesRank, .(university_name), summarize, total_score = mean(total_score, na.rm = TRUE))
colnames(TimesRank)[2] <- "TimeRankScore"

ShanghaiRank <- read.csv("shanghaiData.csv",header = TRUE, sep = ",", dec = ".")
ShanghaiRank <- ShanghaiRank[c("university_name", "total_score")]
ShanghaiRank <- ddply(ShanghaiRank, .(university_name), summarize, total_score = mean(total_score, na.rm = TRUE))
colnames(ShanghaiRank)[2] <- "ShanghaiRankScore"
ShanghaiRank <- ShanghaiRank[!is.na(ShanghaiRank$ShanghaiRankScore),]

CWURRank <- read.csv("cwurData.csv",header = TRUE, sep = ",", dec = ".")
CWURRank <- CWURRank[c("institution", "score")]
CWURRank <- ddply(CWURRank, .(institution), summarize, score = mean(score, na.rm = TRUE))
colnames(CWURRank)[2] <- "CWURRankScore"
CWURRank <- CWURRank[!is.na(CWURRank$CWURRankScore),]

FullCollegeData <- merge(deptOfEdu, TimesRank, by.x = "INSTNM", by.y = "university_name", all.x = TRUE)
FullCollegeData <- merge(FullCollegeData, ShanghaiRank, by.x = "INSTNM", by.y = "university_name", all.x = TRUE)
FullCollegeData <- merge(FullCollegeData, CWURRank, by.x = "INSTNM", by.y = "institution", all.x = TRUE)
FullCollegeData$unemploymentFlag <- ifelse(FullCollegeData$unemp_rate < 3.58, 0, 1)
FullCollegeData[is.na(FullCollegeData)] <- -1
FullCollegeData$unemploymentFlag <- factor(FullCollegeData$unemploymentFlag)

FullCollegeData$TimeRankScore <- as.numeric(FullCollegeData$TimeRankScore)
#classifier <- naiveBayes(FullCollegeData[,3:5], FullCollegeData[,6])
#table(predict(classifier, FullCollegeData[,3:5]), FullCollegeData[,6])
#prediction <- predict(classifier, FullCollegeData[,3:5])
FullCollegeData2 <- FullCollegeData[!(FullCollegeData$TimeRankScore==-1 & FullCollegeData$ShanghaiRankScore==-1 & FullCollegeData$CWURRankScore==-1),]

FCD2 <- FullCollegeData2 #create new data frame form purpose of randomization
FCD2$rand <- sample(1:144, 144) #generate numbers in range and randomly assign them

#randomly split up training and test data
FCD.train <- FCD2[FCD2$rand < (144/2), ] 
FCD.test <- FCD2[FCD2$rand >= (144/2), ]

train.model <- rpart(unemploymentFlag ~ TimeRankScore + ShanghaiRankScore + CWURRankScore, method="class", data=FCD.train)
tab <- table(data.frame(FCD.train$unemploymentFlag, pred=predict(train.model, type="class")))
sum(diag(tab)/sum(tab))

test.model <- rpart(unemploymentFlag ~ TimeRankScore + ShanghaiRankScore + CWURRankScore, method="class", data=FCD.test)
tab2 <- table(data.frame(FCD.test$unemploymentFlag, pred=predict(test.model, type="class")))
sum(diag(tab2)/sum(tab2))

meanRankedandUnranked <- mean(FullCollegeData$unemp_rate)
CollegeThrowAway <- FullCollegeData[rowSums(is.na(FullCollegeData[,3:5]))==3,]
PartiallyRankedCollegeData <- FullCollegeData[!rowSums(is.na(FullCollegeData[,3:5]))==3,]
RankedAllCollegeData <- FullCollegeData[!rowSums(is.na(FullCollegeData[,3:5]))>0,]
meanRankedonly <- mean(FullCollegeData$unemp_rate)
meanThrowAway <- mean(CollegeThrowAway$unemp_rate)

summary(FullCollegeData$unemp_rate)
summary(PartiallyRankedCollegeData$unemp_rate)
summary(RankedAllCollegeData$unemp_rate)
summary(CollegeThrowAway$unemp_rate)

RatedEmpRates <- data.frame(PartiallyRankedCollegeData$unemp_rate)
UnratedEmpRates <- data.frame(CollegeThrowAway$unemp_rate)
FullRatedEmpRates <- data.frame(RankedAllCollegeData$unemp_rate)
RatedEmpRates$type <- 'Partial Ranking'
UnratedEmpRates$type <- 'Unranked'
FullRatedEmpRates$type <- 'Fully Ranked'
colnames(RatedEmpRates)[1] <- 'unemp'
colnames(UnratedEmpRates)[1] <- 'unemp'
colnames(FullRatedEmpRates)[1] <- 'unemp'


#k nearest neighbor
college <- FullCollegeData
college <- subset(college, TimeRankScore != -1 | ShanghaiRankScore != -1 | CWURRankScore != -1)

college<-college[sample(nrow(college)),]
college <- college[-2]

fold10 <- cut(seq(1,nrow(college)),breaks=5,labels=FALSE)

Precisions<-list() #create lists to hold all of the attributes
Recalls<-list()
Fmeasures<-list()
Accuracies<-list()

for(i in 1:5){ #for loop to go through each fold
  Indexes <- which(fold10==i,arr.ind=TRUE) #split up into test and train data
  college.test <- college[Indexes, ]
  college.train <- college[-Indexes, ]
  
  kknnmodel2 = train.kknn(unemploymentFlag~., college.train)
  
  kknncontpredict <- predict(kknnmodel2,college.test[,-5])
  PredictTable <- table(kknncontpredict,college.test[,5])
  
  #calculates each of the attribute using the Prediction Table
  #precision = D / B + D
  Prec<-PredictTable[4]/(PredictTable[2] + PredictTable[4])
  Precisions[i] = c(Prec)
  
  #recall = D / C + D
  Rec<-PredictTable[4]/(PredictTable[3] + PredictTable[4])
  Recalls[i] = c(Rec)
  
  #F-measure = 2 * precision * recall / (precision + recall)
  Fmeasures[i]<-(2*Prec*Rec)/(Prec + Rec)
  
  #accuracy = A + D / A + B + C + D
  Accuracies[i]<-(PredictTable[1] + PredictTable[4])/(PredictTable[1] + PredictTable[2] + PredictTable[3] + PredictTable[4])
  
}

averagePrecision<-Reduce("+", Precisions)/5 #gets the average for each attribute
averageRecall<-Reduce("+", Recalls)/5
averageFmeasure<-Reduce("+", Fmeasures)/5
averageAccuracy<-Reduce("+", Accuracies)/5

print("For 5 fold cross validation using k nearest neighbor") #prints out the desired info
print(c("The average Precision is", averagePrecision))
print(c("The Average Recall is", averageRecall))
print(c("The Average Fmeasure is", averageFmeasure))
print(c("The Average Accuracy is", averageAccuracy))


EmpRates <- rbind(RatedEmpRates,UnratedEmpRates,FullRatedEmpRates)

ggplot(EmpRates, aes(unemp, fill = type)) + geom_density(alpha = 0.45)

chaid(~FullCollegeData$unemploymentFlag, FullCollegeData, na.action = na.pass)
