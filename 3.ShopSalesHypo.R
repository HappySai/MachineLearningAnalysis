setwd("/Users/spusar877/Documents/Business Analytics/Assignment/TestJune/Set5/")
myData <- read.csv("Shop_Sales_Regression.csv")
head(myData)
summary(myData)
str(myData)

names(myData)
levels(myData$Product_Fat_Content)
table(myData$Product_Fat_Content)

myUpdData <- myData %>% replace_na(list(Product_Weight = mean(.$Product_Weight, na.rm = TRUE)))

library(dplyr)
myUpdData <- myUpdData %>% 
    mutate(Product_Fat_Content = ifelse(as.character(Product_Fat_Content) == "low fat", "Low Fat", as.character(Product_Fat_Content)))

myUpdData <- myUpdData %>% 
    mutate(Product_Fat_Content = ifelse(as.character(Product_Fat_Content) == "reg", "Regular", as.character(Product_Fat_Content)))

table(myUpdData$Product_Fat_Content)

boxplot(myUpdData$Product_Weight ~ myUpdData$Product_Fat_Content,
        las = 1, ylab = "Product Weight", xlab = "Product Fat Content", 
        main = "Product Weight by Fat Content")

mean(myUpdData$Product_Weight[myUpdData$Product_Fat_Content == "LF"]) # 12.82
mean(myUpdData$Product_Weight[myUpdData$Product_Fat_Content == "Low Fat"]) # 12.93
mean(myUpdData$Product_Weight[myUpdData$Product_Fat_Content == "Regular"]) # 12.73

with(myUpdData, tapply(Product_Weight, Product_Fat_Content, mean))
#LF         Low Fat         Regular 
#12.81286   12.93037        12.73652 
#

stat1 <- abs(mean(myUpdData$Product_Weight[myUpdData$Product_Fat_Content == "LF"]) - 
                 mean(myUpdData$Product_Weight[myUpdData$Product_Fat_Content == "Regular"]))
stat1 #0.07633794

with(myUpdData, tapply(Product_Weight, Product_Fat_Content, median))
#LF             Low Fat         Regular 
#12.85765       12.85765        12.85765 
#
stat2 <- abs(median(myUpdData$Product_Weight[myUpdData$Product_Fat_Content == "LF"]) - 
                 median(myUpdData$Product_Weight[myUpdData$Product_Fat_Content == "Regular"]))
stat2 # 0

names(myUpdData)
str(myUpdData)
levels(myUpdData$Product_Fat_Content)
table(myUpdData$Product_Fat_Content)

#t.test(myUpdData$Product_Weight ~ myUpdData$Product_Fat_Content, paired = FALSE, var.eq = FALSE)
aovResult <- aov(Product_Weight ~ Product_Fat_Content, data=myUpdData)
aovResult

#                               Product_Fat_Content     Residuals
#Sum of Squares                     72.25               152131.71
#Deg. of Freedom                    2                   8520
#Residual standard error: 4.225616

aovResult$coefficients
#(Intercept)            Product_Fat_ContentLow Fat Product_Fat_ContentRegular 
#12.81285801                 0.11751451                -0.07633794 

summary(aovResult)
#                       Df      Sum Sq      Mean Sq     F value     Pr(>F)
#Product_Fat_Content    2       72          36.12       2.023       0.132
#Residuals           8520 152132   17.86  