setwd("C:/Users/e2wwr4o/Documents/R directory/R4DS/gdp.csv")
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
data1 <- read_csv("gdp.csv",skip = 3)# Using the readr helps eliminate the need to use stringasFactors  
head(data1,5)

#Examining the structure of our dataset
str(data1)
View(data1)
# After taking a peek at the data, we get rid of x3 and x6 since they do not contribute to our observbation
data1 <- data1 %>% 
  select(X1,Ranking,Economy,`US dollars)`) 
#The country column does not have a header and us dollars is not well fomartted so we provide one, X1 Column is changed to countries
colnames(data1)[1] <- "Countries"
colnames(data1)[4] <- "millions_of_US_dollars"

#lets read in the second dataset
data3 <- read_csv("education.csv")
#loooking at the structure of the dataset
View(data3)
str(data3)
# Replace all empty spaces within column names with an underscore
colnames(data3) <- str_replace_all(colnames(data3)," ","_")
data3

#Since we are goona use all columns in  Education dataset, we'll select the most relavant for our analysis  
data3 %>% 
  select(CountryCode,Long_Name,Short_Name, Income_Group,Region,Currency_Unit)  

# Now check for NA's in both dataset
#GDP data
for (i in data1){
  length1 <- is.na(i)
  print (sum(length1))
}
#9
#43
#9
#10
View(Combined_dataset)
grep(" ",colnames(data3))
gsub(grep(" ",colnames(data3)),"_",colnames(data3))


data3
data3 %>%  select(CountryCode,Long_Name,Short_Name, Income_Group,Region,Currency_Unit)  


combined_dataset <- merge.data.frame(data2, data3, by.x = "Countries", by.y = "CountryCode")

is.na(combined_dataset$Countries)
View(combined_dataset)



#lets read in the second dataset
data3 <- read_csv("education.csv")
View(data3)
for (i in data3){
  length2 <- is.na(i)
  print (sum(length1))
}

total <- merge(data2,data3 by="ID")
final <- merge.data.frame(data2, data3, by.x = "Countries", by.y = "CountryCode")


# lets count the total NA values by  each variables
for (i in data1){
  length1 <- is.na(i)
  print (sum(length1))
}

data2 <- data1 %>% 
  select(everything()) %>% 
  na.omit()
View(data2)

