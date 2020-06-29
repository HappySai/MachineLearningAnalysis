library(readr)
library(plyr)
library(dplyr)
library(ggplot2, warn.conflicts = FALSE)
install.packages("expss")
library(expss)
CreditCardDF <- read_csv("Documents/Business Analytics/Assignment/TestJune/Set5/CreditCardDefault_Classification.csv")
View(CreditCardDF)

length(CreditCardDF)
str(CreditCardDF)
class(CreditCardDF)

apply(is.na(CreditCardDF), 2, which)
sum(is.na(CreditCardDF)) 
sum(loanDataFrame[CreditCardDF < 0])
unique(loanDataFrame$Year)
unique(loanDataFrame$BranchID)
unique(loanDataFrame$Week)

colnames(CreditCardDF)
plot(CreditCardDF)
length(unique(CreditCardDF[["Monthly_int_Rate"]]))
unique(CreditCardDF[["Monthly_int_Rate"]])
unique(CreditCardDF$`No Of Contats`)

#Cleansing Age column
unique(CreditCardDF$age)
min(CreditCardDF$age)
max(CreditCardDF$age)
length(unique(CreditCardDF$age))
CreditCardDF %>% distinct(distinct_values = age)
CreditCardDF <- filter(CreditCardDF, age != -10)

ageCount <- CreditCardDF %>%
    count(age) %>%
    rename(age_count = n)

ageCount


#Cleansing Designation ID
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


min(CreditCardDF$facebook_status)
max(CreditCardDF$facebook_status)


#> colnames(CreditCardDF)
#[1] "Monthly_int_Rate"  "bill_amount"       "age"               "monthly_salary"    "education_type_id"
#[6] "Purpose_Loan"      "facebook_status"   "linkedin_status"   "designation_id"    "No Of Contats"    
#[11] "def_flag"  
#

CreditCardDF["age"] = floor(CreditCardDF["age"])
CreditCardDF["NoOfContats"] = CreditCardDF$`No Of Contats`

# Plot between Age and Facebook Status
ggplot(CreditCardDF, aes(y=facebook_status, x=age)) + 
    geom_bar(position="stack", stat="identity")

#Age Vs Linkedin Status
ggplot(CreditCardDF, aes(x = age, y=linkedin_status)) + 
    geom_bar(position="stack", stat="identity")

#Age Vs Salary
ggplot(CreditCardDF, aes(x = age, y=monthly_salary)) + 
    geom_bar(position="stack", stat="identity")

#Age Vs Number of Contacts
ggplot(CreditCardDF, aes(x = age, y=NoOfContats)) + 
    geom_bar(position="dodge", stat="identity")

ggplot(CreditCardDF, aes(fill = Monthly_int_Rate, x = age, y=NoOfContats)) + 
    geom_bar(position="dodge", stat="identity")


#Based on Designation ID, Monthly salary
ggplot(CreditCardDF, aes(x = designation_id, y=monthly_salary)) + 
    geom_bar(position="dodge", stat="identity")

ggplot(CreditCardDF, aes(x = age, y=education_type_id)) + 
    geom_bar(position="dodge", stat="identity")

ggplot(CreditCardDF) + geom_bar(aes(x=education_type_id))
ggplot(CreditCardDF) + geom_bar(aes(x=Purpose_Loan))
ggplot(CreditCardDF) + geom_bar(aes(x=Monthly_int_Rate))
ggplot(CreditCardDF) + geom_bar(aes(x=age))

ggplot(CreditCardDF, aes(x = Purpose_Loan, y=bill_amount)) + 
    geom_bar(position="dodge", stat="identity")

ggplot(CreditCardDF, aes(fill=Monthly_int_Rate, x = Purpose_Loan, y=bill_amount)) + 
    geom_bar(position="dodge", stat="identity")

ggplot(CreditCardDF, aes(x = age, y=Monthly_int_Rate)) + 
    geom_bar(position="stack", stat="identity")

ggplot(CreditCardDF, aes(x = age, y=Monthly_int_Rate)) + 
    geom_bar(position="stack", stat="identity") 

ggplot(CreditCardDF, aes(x = facebook_status, y=NoOfContats)) + 
    geom_bar(position="dodge", stat="identity") 

ggplot(CreditCardDF, aes(x = linkedin_status, y=NoOfContats)) + 
    geom_bar(position="dodge", stat="identity") 

ggplot(CreditCardDF, aes(x = facebook_status)) + 
    geom_bar() + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top")

ggplot(CreditCardDF, aes(x = linkedin_status)) + 
    geom_bar() + 
    geom_text(stat = "count", aes(label = stat(count)), vjust = -1) + 
    theme(legend.position = "top")



use_labels(CreditCardDF, {
    ggplot(CreditCardDF) +
        geom_point(aes(y = education_type_id, x = designation_id, color = "red")) +
        facet_grid(factor(linkedin_status) ~ factor(facebook_status))
}) 


with(CreditCardDF, 
     barplot(
         table(linkedin_status, facebook_status), 
         beside = TRUE, 
         legend = TRUE)
)

table(CreditCardDF$linkedin_status, CreditCardDF$facebook_status)


facebookContracts <- CreditCardDF %>%
    group_by(facebook_status, age) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N),
           pct = round((freq*100), 0))

facebookContracts

facebookContracts %>% group_by(age) %>%
    summarize(total = sum(pct))

p <- ggplot(facebookContracts, aes(x = age, y = pct))
p + geom_col(position = "dodge2") +
    labs(x = "age",y = "Percent") +
    theme(legend.position = "top")


min(CreditCardDF$Monthly_int_Rate)
max(CreditCardDF$Monthly_int_Rate)
CreditCardDF %>%
    count(Monthly_int_Rate) %>%
    rename(monthly_interest = n)

ggplot(data=CreditCardDF, aes(x=monthly_salary, y=bill_amount)) + 
    geom_point(aes(color = Monthly_int_Rate)) 

ggplot(data=CreditCardDF, aes(x=bill_amount, y=monthly_salary)) + 
    geom_point(aes(color = Monthly_int_Rate), alpha = 0.6)


ggplot(data=CreditCardDF, aes(x=Monthly_int_Rate)) +      # Initialize plot 
    geom_histogram(fill="skyblue",      # Create histogram with blue bars
                   col="black",         # Set bar outline color to black
                   binwidth = 0.9)


min(CreditCardDF$def_flag)
max(CreditCardDF$def_flag)
CreditCardDF %>%
    count(def_flag) %>%
    rename(defFlag = n)

ggplot(data=CreditCardDF, aes(x=bill_amount, y=monthly_salary)) + 
    geom_point(aes(color = def_flag), alpha = 0.6)


ggplot(CreditCardDF, aes(x = age, y=def_flag)) + 
    geom_bar(position="stack", stat="identity") + 
    scale_x_discrete(position = "top") + 
    scale_y_continuous(sec.axis = dup_axis())

ggplot(CreditCardDF, aes(fill=def_flag, x = bill_amount, y=monthly_salary)) + 
    geom_bar(position="dodge", stat="identity")

ggplot(CreditCardDF, aes(x = bill_amount, y = monthly_salary, color = def_flag)) +
    geom_point()

plot(CreditCardDF$age, CreditCardDF$monthly_salary)

p +labs(title="Plot of length \n by dose",
        x ="Dose (mg)", y = "Teeth length")