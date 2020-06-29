#-------------------------------------------------------------------------------------------------------
#Data Exploration
#1)	Choose the data set assigned to you.
#2)	Load the data into a data frame, no data loss is allowed because of conversion. 
#3)	Remove the ID column and load the data into another data frame
#4)	Delete the original Data Frame
#5)	Remove the Rows with  invalid values
#6)	Factor the data High, Medium, Low as 1, 2, 3
#7)	Remove any unwanted symbols, data without data loss
#8)	Convert the Value Data to number. No NAs should be introduced
#9)	Convert Date to valid Date format.  No NAs should be introduced
#10)	Convert Attribute to Character
#11)	Calculate the hourly Average of all the all the attributes
#12)	Which hour has the maximum number of observations in the day
#13)	Which hour has the maximum “High” No2 in the data
#14)	Plot a bar graph showing the hourly count for each hour also show the distribution among the attributes for each hour. Op should look something like this 
#15)	Combine Hour and Day Together to a single column
#16)	Transpose the table in such a way each of the attribute becomes a column name and the rows contain the hourly average for each hour. Output should looks something like this
#17)	Find the highly correlated attribute pair and visually represent it.
#-------------------------------------------------------------------------------------------------------
#
library(plyr)
library(dplyr)
library(ggplot2, warn.conflicts = FALSE)
install.packages("expss")
install.packages('hablar')
install.packages('lubridate')
install.packages('pivottabler')
library(expss)
library(scales)
library(ggpubr)
library(readr)
library(tidyverse)
library(hablar)
library(lubridate)
library(pivottabler)
#-------------------------------------------------------------------------------------------------------
#
dataEDADF <- read_csv("Documents/Business Analytics/Assignment/TestJune/Set5/Data_005.csv")
View(dataEDADF) #50683

#-------------------------------------------------------------------------------------------------------
#3)	Remove the ID column and load the data into another data frame
#-------------------------------------------------------------------------------------------------------
dataDF <- dataEDADF[,c(2,3,4)]
dataDF

#-------------------------------------------------------------------------------------------------------
#4)	Delete the original Data Frame
#-------------------------------------------------------------------------------------------------------
remove(dataEDADF)
View(dataDF) #50683
#-------------------------------------------------------------------------------------------------------
#5)	Remove the Rows with  invalid values
#-------------------------------------------------------------------------------------------------------
length(dataDF)
str(dataDF)
class(dataDF)
str(dataDF)
length(dataDF)
class(dataDF)
object.size(dataDF)
summary(dataDF)
dim(dataDF)
nrow(dataDF)
ncol(dataDF)
colnames(dataDF)
sapply(dataDF, class)

unique(dataDF$Value)
min(dataDF$Value)
max(dataDF$Value)

#Remove rows which has Value = I/O TimeOut, Data Lost and No Signal
#
dataDF <- filter(dataDF, Value != 'I/O TimeOut')
dataDF <- filter(dataDF, Value != 'No Signal')
dataDF <- filter(dataDF, Value != 'Data Lost')

#Total Number of Rows: 48652
#
#6)	Factor the data High, Medium, Low as 1, 2, 3
dataDF$Value[dataDF$Value == 'High'] <- '1'
dataDF$Value[dataDF$Value == 'Medium'] <- '2'
dataDF$Value[dataDF$Value == 'Low'] <- '3'

#8)	Convert the Value Data to number. No NAs should be introduced
typeof(dataDF$Value) #Showing as character
mode(dataDF$Value) # Output as character

#By looking into data I saw that there are rows which has % in Value column. 
#And when we convert this Value column (which is current character), will become NA when we convert into numric
#Hence to avoid invalid characters in specific column value to na, better check which are characters wont support
dataDF$Value<- as.numeric(gsub("%", "", as.character(dataDF$Value)))
#dataDF$Value = as.numeric(dataDF$Value)

attributeCount <- dataDF %>%
    count(Attribute) %>%
    rename(attCount = n)

attributeCount

#-------------------------------------------------------------------------------------------------------
#
nrow(dataDF) #- 48652
nrow(is.null(dataDF$Value))
map(dataDF, ~sum(is.na(.)))

#-------------------------------------------------------------------------------------------------------
#9)	Convert Date to valid Date format.  No NAs should be introduced
dataDF$Date <- parse_date_time(dataDF$Date, orders="d-my HMS")
typeof(dataDF$Date)

#-------------------------------------------------------------------------------------------------------
dataDF$Date <- strftime(dataDF$Date,"%Y-%m-%d %H:%M:%S")
sapply(dataDF, class)
#Attribute        Date       Value 
#"character" "character"   "numeric" 
#
#-------------------------------------------------------------------------------------------------------
#11)	Calculate the hourly Average of all the attributes
#-------------------------------------------------------------------------------------------------------

dataDF$hour = hour(dataDF$Date)
dataDF[,list(avg=mean(hour)),by=Attribute]
aggregate(hour ~ Attribute, dataDF, mean)


#-------------------------------------------------------------------------------------------------------
#12)	Which hour has the maximum number of observations in the day

dataDF %>%
    group_by(Attribute, hour) %>%
    summarise(TotalCount = n()) %>%
    arrange(desc(TotalCount))

#Answer is: 6th hour

#-------------------------------------------------------------------------------------------------------
#13)	Which hour has the maximum “High” No2 in the data
#
dataDF %>%
    filter(dataDF$Attribute == 'No2') %>%
    group_by(Attribute, hour) %>%
    summarise(TotalCount = n()) %>%
    arrange(desc(TotalCount))

#Attribute  hour TotalCount
#<chr>     <int>      <int>
#1 No2          10        871
#2 No2           8        870
#3 No2           6        869
#4 No2           9        866
#5 No2           7        859
#6 No2           5        430
#7 No2          11         72

#Answer is: 10th hour

#-------------------------------------------------------------------------------------------------------
#14)	Plot a bar graph showing the hourly count for each hour also show the distribution among the attributes for each hour.
#

dataDF %>%
    group_by(hour, Attribute) %>%
    tally() %>%
    ungroup() %>%
    arrange(desc(Attribute)) %>%
    group_by(hour) %>%
    mutate(pos = cumsum(n) - n/2) %>%
    ggplot() +
    geom_bar(aes(x=hour, y=n, fill = Attribute),stat = "identity") +
    geom_text(aes(x=hour, y=pos, label = prettyNum(n,big.mark = ",")), vjust = 0,  size = 2) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    labs(title="Attribute (per hour)", x="Hour", y = "Total Count", family="Optima") +
    theme(text = element_text(color = "red", family="Optima", face="bold", size=10), 
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color="black"),
          axis.title.x = element_text(colour = "red"))
    
#-------------------------------------------------------------------------------------------------------
#16)Transpose the table in such a way each of the attribute becomes a column name and the rows contain the hourly average for each hour <TODO>

newDataFrame <- as.data.frame(t(dataDF))

newDataFrame %>%
    group_by(hour)




# first remember the names
n <- dataDF$hour

# transpose all but the first column (name)
newDataFrame <- as.data.frame(t(dataDF[,-4]))
colnames(newDataFrame) <- n
newDataFrame$myfactor <- factor(row.names(newDataFrame))
sapply(newDataFrame, class)

summary(newDataFrame)
dim(newDataFrame)
nrow(newDataFrame)
ncol(newDataFrame)
colnames(newDataFrame)


dataDF %>%
    group_by(hour, Attribute) %>%
    group_by(Attribute)




pt <- PivotTable$new()
pt$addData(dataDF)
pt$addColumnDataGroups('hour')
pt$addRowDataGroups('Attribute')
pt$addRowDataGroups('Date')
pt$addRowDataGroups('Value')

pt$renderPivot()


newDataFrame <- qpvt(dataDF, "hour", "Attribute", "n()")
newDataFrame
type(newDataFrame)
newDataFrame$Total




spread(dataDF, Attribute, Date, Values)

#-------------------------------------------------------------------------------------------------------
#17)	Find the highly correlated attribute pair and visually represent it.

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

ggplot(dataDF, aes(x=Attribute, y=Value, fill=hour)) + geom_bar(stat='identity') + scale_y_continuous(labels=fancy_scientific)


