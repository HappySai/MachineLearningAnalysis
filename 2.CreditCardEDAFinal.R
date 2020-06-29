#-------------------------------------------------------------------------------------------------------
#Following are the Analysis based on the data available
#1. Relationship between Age and Facebook status: To understsnd which age group has more active in facebook
#2. Relationship between Age and Linkedin Status: To understsnd which age group has more active in linked
#3. Relationship between Age and Monthly Salary: To show which age group is getting higher overall salary
#4. Relationship between Age and Number of Contacts: To understand which age group has more number of contacts
#5. Relationship of Defaulter flag with Loan Purpose: To understand more on which loan types has dafaulter
#6. Relationship between Age and Bill Amount: To understand spread of Age with Billing Amount (may be more consumption)
#7. Grouping of Education ID
#8. Grouping of Loan Purpose
#9. Grouping of Facebook Status
#10. Grouping of Linkedin Status
#11. Grouping of Designation ID
#12. Grouping of Defaulter flag
#13. Grouping of Monthly Interest Rate
#14. To understand Monthly Interest Rate with Bill Amount
#15. Box plot of Monthly Salary to see how much range
#16. To understand Monthly Interest Rate with Loan Purpose
#17. Do we have any relationship between Monthly Salary to Education Type
#-------------------------------------------------------------------------------------------------------
# Load all the libraries
library(readr)
library(dplyr)
library(ggplot2, warn.conflicts = FALSE)
install.packages("expss")
library(expss)
library(scales)
library(ggpubr)
#Loading the CSV
setwd("/Users/spusar877/Documents/Business Analytics/Assignment/TestJune/Set5/")
CreditCardDF <- read_csv("CreditCardDefault_Classification.csv")
View(CreditCardDF)
length(CreditCardDF)
str(CreditCardDF)
class(CreditCardDF)

# Checking various data and cleanup
#1. Check for Null and less than 0 values
apply(is.na(CreditCardDF), 2, which)
sum(is.na(CreditCardDF)) 

#2. Now check range of variables and get the count of unique values. This will help us to understand odd one out

#Cleansing Age column
unique(CreditCardDF$age)
min(CreditCardDF$age)
max(CreditCardDF$age)
length(unique(CreditCardDF$age))
CreditCardDF$age <- floor(CreditCardDF$age)
CreditCardDF %>% distinct(distinct_values = age)
ageCount <- CreditCardDF %>%
    count(age) %>%
    rename(age_count = n)

ageCount

CreditCardDF <- filter(CreditCardDF, age != -10)
CreditCardDF %>% distinct(distinct_values = age)

#Renaming of Number of Contracts
CreditCardDF$NumberOfContacts <- CreditCardDF$`No Of Contats`

##Cleansing Designation ID
unique(CreditCardDF$designation_id)
min(CreditCardDF$designation_id)
max(CreditCardDF$designation_id)
length(unique(CreditCardDF$designation_id))
CreditCardDF %>% distinct(distinct_values = designation_id)
designationCount <- CreditCardDF %>%
    count(designation_id) %>%
    rename(designation_count = n)

designationCount
CreditCardDF <- filter(CreditCardDF, designation_id != 0)


# Final Dataframe to run varios EDA
CreditCardDF
str(CreditCardDF)
length(CreditCardDF)
class(CreditCardDF)
object.size(CreditCardDF)
summary(CreditCardDF)
dim(CreditCardDF)
nrow(CreditCardDF)
ncol(CreditCardDF)
colnames(CreditCardDF)

#Exporting the final dataframe to CSV
#
write.csv(CreditCardDF, "Documents/Business Analytics/Assignment/TestJune/Set5/CreditCardClassificationFinal.csv", row.names = FALSE)

#[1] "Monthly_int_Rate"  "bill_amount"       "age"               "monthly_salary"    "education_type_id" "Purpose_Loan"     
#[7] "facebook_status"   "linkedin_status"   "designation_id"    "No Of Contats"     "def_flag"          "NumberOfContacts" 


fancy_scientific <- function(l) {
    # turn in to character string in scientific notation
    l <- format(l, scientific = TRUE)
    # quote the part before the exponent to keep all the digits
    l <- gsub("^(.*)e", "'\\1'e", l)
    # turn the 'e+' into plotmath format
    l <- gsub("e", "%*%10^", l)
    # return this as an expression
    parse(text=l)
}

#-------------------------------------------------------------------------------------------------------
#1. Relationship between Age and Facebook status: To understsnd which age group has more active in facebook
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(y=facebook_status, x=age)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(title = "Age Vs Facebook Status", x="Age", y = "Facebook Status", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))


#-------------------------------------------------------------------------------------------------------
#2. Relationship between Age and Linkedin Status: To understsnd which age group has more active in linked
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(y=linkedin_status, x=age)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(title = "Age Vs Linkedin Status", x="Age", y = "Linkedin Status", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#3. Relationship between Age and Monthly Salary: To show which age group is getting higher overall salary
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(y=monthly_salary, x=age)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(title = "Age Vs Monthly Salary", x="Age", y = "Monthly Salary", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#4. Relationship between Age and Number of Contacts: To understand which age group has more number of contacts
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x=age, y=CreditCardDF$NumberOfContacts)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(title = "Age Vs Number of Contacts", x="Age", y = "Number of Contacts", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#5. Relationship of Defaulter flag with Loan Purpose: To understand more on which loan types has dafaulter
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x=Purpose_Loan, y=def_flag)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(title = "Loan Purpose Vs Defaulter Flag", x="Loan Purpose", y = "Number of Dafaluter", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#6. Relationship between Age and Bill Amount: To understand spread of Age with Billing Amount (may be more consumption)
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x=age, y=bill_amount)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(title = "Age Vs Total Bill Amount", x="Age", y = "Bill Amount", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red")) + 
    scale_y_continuous(labels=fancy_scientific)

#-------------------------------------------------------------------------------------------------------
#7. Grouping of Education ID
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x = education_type_id)) + 
    geom_bar() + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top") + 
    labs(title = "Education ID (In Numbers)", x="Education Type", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))
#-------------------------------------------------------------------------------------------------------
#8. Grouping of Loan Purpose
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x = Purpose_Loan)) + 
    geom_bar() + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top") + 
    labs(title = "Loan Purpose(In Numbers)", x="Loan Purpose", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#9. Grouping of Facebook Status
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x = facebook_status)) + 
    geom_bar() + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top") + 
    labs(title = "Facebook Status (In Numbers)", x="Facebook Type", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#10. Grouping of Linkedin Status
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x = linkedin_status)) + 
    geom_bar() + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top") + 
    labs(title = "Linkedin Status (In Numbers)", x="LinkedIn Type", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#11. Grouping of Designation ID
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x = designation_id)) + 
    geom_bar(aes(fill = factor(designation_id))) + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top") + 
    labs(title = "Designation ID (In Numbers)", x="Designation Id", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red")) + 
    theme(legend.position="none")

#-------------------------------------------------------------------------------------------------------
#12. Grouping of Defaulter flag
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x = def_flag)) + 
    geom_bar(aes(fill = factor(def_flag))) + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top") + 
    labs(title = "Def Flag (In Numbers)", x="Defaulter Flag", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red")) +
    theme(legend.position="right")

#-------------------------------------------------------------------------------------------------------
#13. Grouping of Monthly Interest Rate
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(x = education_type_id)) + 
    geom_bar() + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top") + 
    labs(title = "Education ID (In Numbers)", x="Education Type", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="green"),
          axis.title.x = element_text(colour = "red"))

#-------------------------------------------------------------------------------------------------------
#14. To understand Monthly Interest Rate with Bill Amount
#-------------------------------------------------------------------------------------------------------
ggplot(CreditCardDF, aes(Monthly_int_Rate,bill_amount, label = Monthly_int_Rate)) + 
    geom_bar(stat="identity") + 
    facet_wrap(~grouping, scales="free")


#-------------------------------------------------------------------------------------------------------
#15. Box plot of Monthly Salary to see how much range
#-------------------------------------------------------------------------------------------------------

ggplot(CreditCardDF, aes(x = monthly_salary)) + 
    geom_boxplot()
    
df <- CreditCardDF %>%
    group_by(Monthly_int_Rate) %>%
    summarise(counts = n())
df

ggplot(df, aes(x = Monthly_int_Rate, y = counts)) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) + 
    theme_pubclean()
