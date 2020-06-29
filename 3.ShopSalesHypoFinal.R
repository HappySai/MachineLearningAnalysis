#---------------------------------------------------------------------
#Hypothesis: Product Fat Content doesn't have any impact on Product Weight
#
#---------------------------------------------------------------------
library(dplyr)

setwd("/Users/spusar877/Documents/Business Analytics/Assignment/TestJune/Set5/")
myData <- read.csv("Shop_Sales_Regression.csv")
head(myData)
summary(myData)
str(myData)

names(myData)
levels(myData$Product_Fat_Content)
table(myData$Product_Fat_Content)

myUpdData <- myData %>% replace_na(list(Product_Weight = mean(.$Product_Weight, na.rm = TRUE)))
myUpdData <- myUpdData %>% 
    mutate(Product_Fat_Content = ifelse(as.character(Product_Fat_Content) == "low fat", "Low Fat", 
                                        as.character(Product_Fat_Content)))
myUpdData <- myUpdData %>% 
    mutate(Product_Fat_Content = ifelse(as.character(Product_Fat_Content) == "reg", "Regular", 
                                        as.character(Product_Fat_Content)))

table(myUpdData$Product_Fat_Content)
boxplot(myUpdData$Product_Weight ~ myUpdData$Product_Fat_Content,
        las = 1, ylab = "Product Weight", xlab = "Product Fat Content", 
        main = "Product Weight by Fat Content")

aovResult <- aov(Product_Weight ~ Product_Fat_Content, data=myUpdData)
aovResult

#                               Product_Fat_Content     Residuals
#Sum of Squares                    72.25               152131.71
#Deg. of Freedom                    2                   8520
#Residual standard error: 4.225616

aovResult$coefficients
#(Intercept)            Product_Fat_ContentLow Fat Product_Fat_ContentRegular 
#12.81285801                 0.11751451                -0.07633794 

summary(aovResult)
#                       Df      Sum Sq      Mean Sq     F value     Pr(>F)
#Product_Fat_Content    2       72          36.12       2.023       0.132
#Residuals           8520 152132   17.86  
#
#----------------------------------------------------------------------------------------------
#We can see that p value is greater than 0.05, hence reject the null hypothesis
#----------------------------------------------------------------------------------------------
#


#----------------------------------------------------------------------------------------------\
#
#
#Hypothesis 2: Product Type and Product Weight
#
#
#----------------------------------------------------------------------------------------------
str(myData)
names(myData)
levels(myData$Product_Type)
table(myData$Product_Type)

levels(myData$Product_MRP)
table(myData$Product_MRP)

apply(is.na(myData), 2, which)
sum(is.na(myData)) 

boxplot(myData$Product_MRP ~ myData$Product_Type,
        las = 1, ylab = "Product MRP", xlab = "Product Type", 
        main = "Product Type on Product MRP")

aovResultUpd <- aov(Product_MRP ~ Product_Type, data=myData)
aovResultUpd

#                      Product_Type         Residuals
#Sum of Squares         412151              32637732
#Deg. of Freedom           15               8507
#Residual standard error: 61.94008
#

aovResultUpd$coefficients
#(Intercept)                Product_TypeBreads             Product_TypeBreakfast                Product_TypeCanned 
#126.380766                         14.571902                         15.407385                         13.383066 
#Product_TypeDairy          Product_TypeFrozen Foods Product_TypeFruits and Vegetables           Product_TypeHard Drinks 
#22.118442                         12.122600                         18.200469                         10.697162 
#Product_TypeHealth and Hygiene             Product_TypeHousehold                  Product_TypeMeat                Product_TypeOthers 
#4.438155                         23.043987                         13.501266                          6.470664 
#Product_TypeSeafood           Product_TypeSnack Foods           Product_TypeSoft Drinks         Product_TypeStarchy Foods 
#15.460953                         19.814168                          5.111740                         21.457257 

summary(aovResultUpd)
#               Df   Sum Sq         Mean Sq     F value         Pr(>F)    
#Product_Type   15   412151         27477       7.162           6.51e-16 ***
#    Residuals    8507 32637732    3837                     

#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#----------------------------------------------------------------------------------------------
#We can see that p value is less than 0.05, hence accept the null hypothesis
#----------------------------------------------------------------------------------------------
#