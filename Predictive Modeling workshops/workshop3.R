---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
library(caret)
library(corrplot)
library(e1071)
library(lattice)
library(AppliedPredictiveModeling)

```

```{r}
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
```

```{r}
pcaobj<- prcomp(segData,center = TRUE, scale. = TRUE)
percentVariance <- pcaobj$sdev ^2/sum(pcaobj$sdev^2)*100
percentVariance[1:3]

```

```{r}
head([,1:5])
```

```{r}
pcaobj$x
```

```{r}
head(pcaobj$x[,1:3])
```

```{r}
trans <- preProcess(segData,method = c("BoxCox","center", "scale", "pca"))
trans
```

```{r}
transformed <- predict(trans, segData)
```

```{r}
head(transformed[,1:5])
```


```{r}
nearZeroVar(segData)
```

```{r}
corealtions <-cor(segData)
dim(corealtions)
corealtions[1:5,1:5]
```

```{r}
corrplot(corealtions, order = 'hclust')
```

```{r}
  highCorr <-  findCorrelation(corealtions,cutoff = 0.75)
  length(highCorr)
```

```{r}
head(highCorr)
```

```{r}
filteredSegData <- segData[, -highCorr]
filteredSegData
```

