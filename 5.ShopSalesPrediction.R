library(tidyr)
setwd("/Users/spusar877/Documents/Business Analytics/Assignment/TestJune/Set5/")
myData <- read.csv("Shop_Sales_Regression.csv")
head(myData)
summary(myData)
str(myData)

unique(myData$Product_Weight)
min(myData$Product_Weight)
max(myData$Product_Weight)

apply(is.na(myData), 2, which)
sum(is.na(myData)) 

sum(is.na(myData$Product_Weight))

#Both Product Weight NA value is matching the na value of entire dataframe
#Convert NA values of Product Weight into mean of Product Waight

mean(myData$Product_Weight)

myUpdData <- myData %>% replace_na(list(Product_Weight = mean(.$Product_Weight, na.rm = TRUE)))
myUpdData


sum(is.na(myUpdData$Product_Weight))
sum(is.na(myUpdData)) 
#Now I have all the date which is clean as there is no NA values in entire dataframe
#
#----------------------------------------------------------------------------------------
#Now lets break the dataset into Test and Training dataset
#----------------------------------------------------------------------------------------
#
set.seed(1234)
TrainingIndex <- createDataPartition(myUpdData$Shop_Outlet_Sales, p=0.7, list = FALSE)
TrainingData <- myUpdData[TrainingIndex,]
TestingData <- myUpdData[-TrainingIndex,]

#Now we have both Training and Test Dataset.
#Let's create model using the Training Dataset and then will apply on Testing Dataset
#
myModel <- lm(Shop_Outlet_Sales ~ Product_Weight + Product_Visibility + Product_MRP,
              data = myUpdData)
summary(myModel)
AIC(myModel)

#Apply this model into Test Dataset to see how prdiction looks like
#
salesPred <- predict(myModel, TestingData)
summary(salesPred)
str(salesPred)
salesPred

actPred <- data.frame(cbind(actuals=myUpdData$Shop_Outlet_Sales, predict= salesPred))
corAccuracy <- cor(actPred)
head(actPred)

minMaxAcc <- mean(apply(actPred, 1, min)/ apply(actPred, 1, max))
minMaxAcc

mape <- mean(abs((actPred$predict - actPred$actuals)) / actPred$actuals)
mape

summary(myModel)$coef

R2(salesPred, TestingData$Shop_Outlet_Sales)# 0.32

#----------------------------------------------------------------------------------------
myUpdModel <- lm(Shop_Outlet_Sales ~ Product_Visibility + Product_MRP,
              data = myUpdData)
summary(myUpdModel)
salesUpdPred <- predict(myUpdModel, TestingData)
R2(salesUpdPred, TestingData$Shop_Outlet_Sales) # 0.32


#----------------------------------------------------------------------------------------
myNewModel <- lm(Shop_Outlet_Sales ~ Product_Visibility,
                 data = myUpdData)
summary(myNewModel)
salesNewPred <- predict(myNewModel, TestingData)
R2(salesNewPred, TestingData$Shop_Outlet_Sales) # 0.014

#----------------------------------------------------------------------------------------
myNewModelMrp <- lm(Shop_Outlet_Sales ~ Product_MRP,
                 data = myUpdData)
summary(myNewModelMrp)
salesNewPredMrp <- predict(myNewModel, TestingData)
R2(salesNewPredMrp, TestingData$Shop_Outlet_Sales) # 0.014

#----------------------------------------------------------------------------------------