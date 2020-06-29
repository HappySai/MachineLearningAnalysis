install.packages("aod")
install.packages("caret")
install.packages("ROCR")
install.packages("caTools")
library(ROCR)
library(caret)
library(ggplot2)
library(aod)
library(caTools)
#---------------------------------------------------------------------------------
setwd("/Users/spusar877/Documents/Business Analytics/Assignment/TestJune/Set5/")
myData <- read.csv("CreditCardClassificationFinal.csv")
head(myData)
summary(myData)
str(myData)

#---------------------------------------------------------------------------------
#1. Lets see how bill_amount has an impact on prediction on def_flag
#---------------------------------------------------------------------------------
ggplot(myData, aes(bill_amount, def_flag)) +
    geom_point() +
    geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))

billModel <- glm(def_flag ~ bill_amount, data = myData, family = "binomial")
summary(billModel)
coef(billModel)
exp(coef(billModel))
exp(confint(billModel))

crossValSettings <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)


billVal <- train(as.factor(def_flag) ~ bill_amount,
                 data = myData,family = "binomial", 
                 method = "glm", trControl = crossValSettings)
billPred <- predict(billVal, newdata = myData)
myData$def_flag = as.factor(myData$def_flag)
confusionMatrix(data = billPred, myData$def_flag) # 78.33%

#---------------------------------------------------------------------------------
#2. Here we are going to consider following 3 parameter to predict the def_flag
#bill_amount
#monthly_salary
#Monthly_int_Rate
#---------------------------------------------------------------------------------
myData$Monthly_int_Rate <- factor(myData$Monthly_int_Rate)
defModel <- glm(def_flag ~ bill_amount + monthly_salary + Monthly_int_Rate, 
                data = myData,
                family = "binomial")

summary(defModel)
coef(defModel)
confint(defModel)

exp(coef(defModel))
exp(confint(defModel))
cbind(OR = exp(coef(defModel)), exp(confint(defModel)))


#Now let's see how this individual independent variable has an impact to overall model
wald.test(b = coef(defModel), Sigma = vcov(defModel), Terms = 4:10)
wald.test(b = coef(defModel), Sigma = vcov(defModel), Terms = 2)
wald.test(b = coef(defModel), Sigma = vcov(defModel), Terms = 3)

#Conclusion: Hence we can understand above all 4 having an impact on depdent variable

#Assessing the model using the cross validation

crossVal <- train(as.factor(def_flag) ~ bill_amount + monthly_salary + Monthly_int_Rate, 
                  data = myData,
                  family = "binomial",
                  method = "glm",
                  trControl = crossValSettings)

pred <- predict(crossVal, newdata = myData)
myData$def_flag = as.factor(myData$def_flag)
confusionMatrix(data = pred, myData$def_flag) # 79.00%


#---------------------------------------------------------------------------------
#3. Let's add Purpose_Loan to see how this has any impact on oveall prediction
#---------------------------------------------------------------------------------
defModelUpd <- glm(def_flag ~ bill_amount + monthly_salary + Monthly_int_Rate + Purpose_Loan, 
                data = myData,
                family = "binomial")

summary(defModelUpd)
coef(defModelUpd)
confint(defModelUpd)

exp(coef(defModelUpd))
exp(confint(defModelUpd))
cbind(OR = exp(coef(defModelUpd)), exp(confint(defModelUpd)))

#Assessing the model using the cross validation
crossValUpd <- train(as.factor(def_flag) ~ bill_amount + monthly_salary + Monthly_int_Rate + Purpose_Loan, 
                  data = myData,
                  family = "binomial",
                  method = "glm",
                  trControl = crossValSettings)

predUpd <- predict(crossValUpd, newdata = myData)
confusionMatrix(data = predUpd, myData$def_flag) # 79%

#---------------------------------------------------------------------------------
#4. Let's add Purpose_Loan to see how this has any impact on oveall prediction
#---------------------------------------------------------------------------------
defModelNew <- glm(def_flag ~ bill_amount + monthly_salary + Purpose_Loan, 
                   data = myData,
                   family = "binomial")

summary(defModelNew)
coef(defModelNew)
confint(defModelNew)

exp(coef(defModelNew))
exp(confint(defModelNew))
cbind(OR = exp(coef(defModelNew)), exp(confint(defModelNew)))

#Assessing the model using the cross validation
crossValNew <- train(as.factor(def_flag) ~ bill_amount + monthly_salary + Purpose_Loan, 
                     data = myData,
                     family = "binomial",
                     method = "glm",
                     trControl = crossValSettings)

predNew <- predict(crossValNew, newdata = myData)
confusionMatrix(data = predNew, myData$def_flag) # 78.6%


#---------------------------------------------------------------------------------
probModel <- predict(defModel, type = "response")
predictModel <- prediction(probModel, myData$def_flag)
perfModel <- performance(predictModel, measure = "tpr", x.measure = "fpr")


probBillModel <- predict(billModel, type = "response")
predictBillModel <- prediction(probBillModel, myData$def_flag)
perfBillModel <- performance(predictBillModel, measure = "tpr", x.measure = "fpr")

probModelUpd <- predict(defModelUpd, type = "response")
predictModelUpd <- prediction(probModelUpd, myData$def_flag)
perfModelUpd <- performance(predictModelUpd, measure = "tpr", x.measure = "fpr")


probModelNew <- predict(defModelNew, type = "response")
predictModelNew <- prediction(probModelNew, myData$def_flag)
perfModelNew <- performance(predictModelNew, measure = "tpr", x.measure = "fpr")


plot(perfModel, col="blue")
plot(perfBillModel, add = TRUE, col="red")
plot(perfModelUpd, add = TRUE, col="black")
plot(perfModelNew, add = TRUE, col="green")

AucBill <- performance(predictBillModel, measure = "auc")
AucModel <- performance(predictModel, measure = "auc")
AucModelUpd <- performance(predictModelUpd, measure = "auc")
AucModelNew <- performance(predictModelNew, measure = "auc")

AucBill@y.values #0.6537948
AucModel@y.values #0.7096965
AucModelUpd@y.values #0.7099728
AucModelNew@y.values #0.6942502


#--------------------------------------------------------------
#Option 2
myData
set.seed(1234)
TrainingIndex <- createDataPartition(myData$def_flag, p=0.7, list = FALSE)
TrainingData <- myData[TrainingIndex,]
TestingData <- myData[-TrainingIndex,]

myModel <- glm(def_flag ~ bill_amount + monthly_salary + Purpose_Loan,
               data = TrainingData, 
               family = "binomial")
summary(myModel)

resTest <- predict(myModel, TestingData, type = "response")
resTest <- predict(myModel, TrainingData, type = "response")

conMatrix <- table(ActualValue = TrainingData$def_flag, PredictedValue = resTest > 0.5 )
conMatrix

(conMatrix[[1,1]] + conMatrix[[2,2]]) / sum(conMatrix) # 78.5

#--------------------------------------------------------------
#Option 3: Using all the features

myModelUpd <- glm(def_flag ~ ., data = TrainingData, family = "binomial")
summary(myModelUpd)

resTestUpd <- predict(myModelUpd, TestingData, type = "response")
resTestUpd <- predict(myModelUpd, TrainingData, type = "response")

conMatrixUpd <- table(ActualValue = TrainingData$def_flag, PredictedValue = resTestUpd > 0.5 )
conMatrixUpd

(conMatrixUpd[[1,1]] + conMatrixUpd[[2,2]]) / sum(conMatrixUpd) # 78.4

#--------------------------------------------------------------------------------------

#Additional check to see if removal of any insignificant feature have any impact on AIC value
myModelUpd <- glm(def_flag ~ . -age, data = TrainingData, family = "binomial")
summary(myModelUpd)

myModelUpd <- glm(def_flag ~ . -linkedin_status, data = TrainingData, family = "binomial")
summary(myModelUpd)

myModelUpd <- glm(def_flag ~ . -designation_id, data = TrainingData, family = "binomial")
summary(myModelUpd)

#--------------------------------------------------------------------------------------
