---
title: 'Peer-graded Assignment: Prediction Assignment Writeup'
author: "Richard Yeow"
date: "April 18, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
```

#Synoposis

People regularly quantify how much of a particular activity they do, not how well they do it. This project will use the data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.

The goal is to predict the manner in which they did the exercise. 

This report should include:
-description on how the model is built
-how cross validation is used
-what is the expected out of sample error
-reason for the choices made

The prediction model will also used to predict 20 different test cases.

#Data Processing
##Loading the data
```{r}
set.seed(123) #for the purpose of reproducible

# Set the URL for the download
TrainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TestURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Download and load the data
OriTrain <- read.csv(url(TrainURL))
OriTest <- read.csv(url(TestURL))
```
##Data partition
In order to estimate out-of-sample error, we split the original training dataset into 70% for prediction, 30% for validation.
```{r}
library(caret)
inTrain <- createDataPartition(OriTrain$classe, p = 0.7, list = FALSE)
PredData <- OriTrain[inTrain, ]
ValData <- OriTrain[-inTrain, ]
```
##Clean the data
Reduce the number of features by removing variables with nearly zero variance, variables that are almost always NA, and variables that don’t make intuitive sense for prediction. Since PredData and ValData are from the same source, we will perform identifcal removal for PredData and ValData 
```{r}
#Removing variables with nearly zero variance
nzv <- nearZeroVar(PredData)
PredData <- PredData[,-nzv]
ValData <- ValData[,-nzv]

#Removing variables that are almost always NA
MostlyNA <- sapply(PredData, function(x) mean(is.na(x))) > 0.95
PredData <- PredData[, MostlyNA == FALSE]
ValData <- ValData[, MostlyNA == FALSE]

#Removing variable that don’t make intuitive sense for prediction - the first five column
PredData <- PredData[, -c(1:5)]
ValData <- ValData[, -c(1:5)]
```
#Prediction Algorithms (Model Building)
##Random Forest
```{r}
#Using 3-fold cross validation to select optimal tuning parameter
control <- trainControl(method = "cv", number = 3) 

#Fit model on Prediction Data
fit <- train(classe ~ ., data = PredData, method = "rf", trControl = control)

#Print final model to see tuning parameters it chose
fit$finalModel
```
##Model Evaluation and Selection
Using the fitted model to predict the label (“classe”) in Validation Data (ValData), and show the confusion matrix to compare the predicted versus the actual labels

```{R}
#Use model to predict classe in Validation Data (ValData)
PredictTrain <- predict(fit, newdata = ValData)

#Show confusion matrix to get estimate of out-of-sample error
confusionMatrix(ValData$classe, PredictTrain)
```
The accuracy is 99.81%, thus the predicted accuracy for the out-of-sample error is 0.19%.

With the accuracy above, we will use Random Forests to predict on the test set data.

##Predicting Test Set Data
```{r}
PredictTest <- predict(fit, newdata = OriTest)
PredictTest
```
