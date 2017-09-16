library(caret)
library(corrplot)
library(e1071)
library(lattice)
library(AppliedPredictiveModeling)
data(segmentationOriginal)
structure(segmentationOriginal)
summary(segmentationOriginal)
segData= subset(segmentationOriginal, Case=="Train")
names(segmentationOriginal)
dim(segmentationOriginal)

Cell <-(segData$Cell)
Class <- segData$Class

Case <- segData$Case

segData <- segData[,-(1:3)]

statusColNum <-grep("Status", names(segData))

segData <- segData[,-statusColNum]

dim(segData)
skewness(segData$AreaCh1)
skewValues<- apply(segData,2, skewness)
head(skewValues)

hist(segData$AvgIntenCh1, breaks = 100, main = "Histogram of Segmentation Original")
hist(segData$AngleCh1, breaks = 100, main = "Histogram of Segmentation Original")
ch1ArearTrans <-BoxCoxTrans(segData$AreaCh1)
ch1ArearTrans$skewness
ch1ArearTrans$summary
ch1ArearTrans$lambda
head(segData$AreaCh1)
myA= predict(ch1ArearTrans, head(segData$AreaCh1))
myA
skewness(myA)

pcaobj<- prcomp()




