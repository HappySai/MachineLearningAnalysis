library(ROCR)
library(caret)
library(ggplot2)
library(aod)
library(kernlab)
#---------------------------------------------------------------------------------
setwd("/Users/spusar877/Documents/Business Analytics/Assignment/TestJune/Set5/")
myClassDF <- read.csv("CreditCardClassificationFinal.csv")
head(myClassDF)
summary(myClassDF)
str(myClassDF)

sum(is.na(myClassDF))

set.seed(1234)
TrainingIndex <- createDataPartition(myClassDF$def_flag, p=0.7, list = FALSE)
TrainingData <- myClassDF[TrainingIndex,]
TestingData <- myClassDF[-TrainingIndex,]

#Building Training Model
#
Model <- train(def_flag ~ ., data = myClassDF,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "none"),
               tuneGrid = data.frame(degree = 1, scale = 1, C=1))

ModelCV <- train(def_flag ~ ., data = myClassDF,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = data.frame(degree = 1, scale = 1, C=1))



