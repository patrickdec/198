library(ggplot2)#loads a graphing library to produce charts
library(plyr)
library(partykit)
library(grid)
library(CHAID)

#Nicole: hello

setwd("C:/Users/JDec/Documents/CSE198/Final Proj/Data")
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

EmpRates <- rbind(RatedEmpRates,UnratedEmpRates,FullRatedEmpRates)

ggplot(EmpRates, aes(unemp, fill = type)) + geom_density(alpha = 0.45)

chaid(~FullCollegeData$unemploymentFlag, FullCollegeData, na.action = na.pass)
