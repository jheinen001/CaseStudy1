#############################
##CaseStudy1 Project 
#############################
### Loading required package:Please install first if you have them
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)
library(Hmisc)

# Download data from URL
GDPurl <-  "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(GDPurl, "GDP.csv", quiet=TRUE)

EduURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(EduURL, "Edu.csv", quiet=TRUE)

#Read files into dataframe  
# Using the "read_csv" from readr package helps eliminate the need to use stringasFactors= F
GDP.data <- read_csv("GDP.csv",skip =3)  
Educ.data <- read_csv("EDU.csv") 

#GDP data has some irrelevant columns: we pick the relevant ones
GDP.data <- GDP.data[, c(1, 2, 4, 5)]
GDP.data <- GDP.data[-c(1:2),]
#Rename column
colnames(GDP.data) <- c("Country_Code", "Rank", "Country_Name", "GDP_Value")

#Examining  our dataset
head(GDP.data)
head(Educ.data)

#Replace all empty spaces within Educ_data columns  with an underscore
colnames(Educ.data) <- str_replace_all(colnames(Educ.data)," ","_")

#Since we are not goona use all columns in  Education dataset, we'll select the most relavant for our analysis  
Educ.data <- Educ.data %>% 
  select(CountryCode,Long_Name,Short_Name, Income_Group,Region,Currency_Unit)  

# Now check for number of NA's in all columns within GDP and Educ dataset
for (i in GDP.data){
  length1 <- is.na(i)
  print (sum(length1))
}

for (i in Educ.data){
  length1 <- is.na(i)
  print (sum(length1))
}

#Convert the Character GDP column in the GDP_data to Numeric  and remove the commas
GDP.data$GDP_Value <- as.numeric(str_replace_all(GDP.data$GDP_Value,",",""))
#Convert rank column to numbers 
GDP.data$Rank <- as.numeric(GDP.data$Rank)

#Question1 
#Merge our dataset and remove NA's
merge_GDP_EDU <- 
  merge.data.frame(GDP.data,Educ.data, by.x = "Country_Code", by.y = "CountryCode") %>% 
  na.omit()

#Question 2
#Sort the data frame in ascending order by GDP (so United States is last). What is the 13th country in the resulting data frame?
merge_GDP_EDU <- merge_GDP_EDU %>% 
  select(everything()) %>% 
  arrange(GDP_Value) 

merge_GDP_EDU[13,3]

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
merge_GDP_EDU$Quantiles <- cut2(merge_GDP_EDU$Rank, g=5)
table(merge_GDP_EDU$Quantiles, merge_GDP_EDU$Income_Group)
#How many countries are Lower middle income but among the 38 nations with highest GDP
merge_GDP_EDU[151:189,c(1,2,3,4,7)] %>% 
  filter(Income_Group=="Lower middle income") 

#6 countries
  



