setwd("C:/Users/e2wwr4o/Documents/R directory/R4DS/")

### Loading required package:
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)
library(Hmisc)




GDPurl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(GDPurl, "GDP.csv", quiet=TRUE)

EduURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(EduURL, "Edu.csv", quiet=TRUE)

GDP.data <- read_csv("GDP.csv",skip =3)# Using the readr helps eliminate the need to use stringasFactors  
Educ.data <- read_csv("EDU.csv") 

#GDP data has some irrelevant columns: we pick the relevant ones
GDP.data <- GDP.data[, c(1, 2, 4, 5)]
#Rename column
colnames(GDP.data) <- c("Country_Code", "Rank", "Country_Name", "GDP_Value")
GDP.data <- GDP.data[-c(1:2),]

#Examining the structure of our dataset
str(GDP.data)
View(Educ.data)

#Replace all empty spaces within Educ_data columns  with an underscore
colnames(Educ.data) <- str_replace_all(colnames(Educ.data)," ","_")

#Since we are not goona use all columns in  Education dataset, we'll select the most relavant for our analysis  
Educ.data <- Educ.data %>% 
  select(CountryCode,Long_Name,Short_Name, Income_Group,Region,Currency_Unit)  

# Now check for NA's in both dataset
#GDP data
for (i in GDP.data){
  length1 <- is.na(i)
  print (sum(length1))
}

# Now check for NA's in both dataset
#GDP data
for (i in Educ.data){
  length1 <- is.na(i)
  print (sum(length1))
}

#Covert the Character GDP column in the GDP_data to Numeric  and remove the commas
GDP.data$GDP_Value <- as.numeric(str_replace_all(GDP.data$GDP_Value,",",""))
GDP.data$Rank <- as.numeric(GDP.data$Rank)

View(merge_GDP_EDU) <- merge.data.frame(GDP.data,Educ.data, by.x = "Country_Code", by.y = "CountryCode")

nrow(merge_GDP_EDU )

#Question 2
#Sort the data frame in ascending order by GDP (so United States is last). What is the 13th country in the resulting data frame?
merge_GDP_EDU <- merge_GDP_EDU %>% 
  select(everything()) %>% 
  na.omit() %>% 
  arrange(GDP_Value) 

View(merge_GDP_EDU[13,3])

# Question3
Avg_GDP <- merge_GDP_EDU %>% 
  select(everything()) %>% 
  group_by(Income_Group) %>%
  summarise(Average_GDP = mean(Rank))%>%
  filter(Income_Group %in%  c("High income: OECD","High income: nonOECD"))

Avg_GDP


ggplot(merge_GDP_EDU, aes(y = GDP_Value, x=Income_Group,fill=Income_Group)) + 
  scale_y_log10()+ 
  geom_point(pch = 21, size = 8, stat = "identity", position=position_jitter())+
  scale_fill_manual(values = c("red", "orange", "green", "blue","brown"),
                    na.value = "grey50" ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Question 5
merge_GDP_EDU$RankGroups <- cut2(merge_GDP_EDU$Rank, g=5)
table(merge_GDP_EDU$RankGroups, merge_GDP_EDU$Income_Group)

