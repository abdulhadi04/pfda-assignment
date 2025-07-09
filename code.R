# Group Members:
# Name1, TP000001
# Name2, TP000002
# Name3, TP000003
# Name4, TP000004

# import data - R dataset

data(package = .packages(all.available = TRUE))

# Load required libraries
library(ggplot2)
library(dplyr)

library(data.table)




# Step 1: Load Dataset
######################
BankData <- fread(input = "C:\\Users\\Abdulhadi\\Desktop\\PFDA Assignment\\3. credit_risk_classification.csv" , ##REPLACE WITH YOUR OWN FILE PATH HERE!!!!!
                    sep = "," , header = TRUE) 
BankData

###############################################################################
# Step 2: Data Exploration
###############################################################################
# view headers only
names(BankData)

# data structure
class(BankData)

# no. of column 
length(BankData)

#no. of rows
nrow(BankData)

#view data
head(BankData)  #first 6 rows
head(BankData,10)

tail(BankData)  #last 6 rows
tail(BankData, 20)

# view in tabular form
View(BankData)

# view an overview of the data
summary(BankData)
str(BankData)






###############################################################################
# Step 3: Data Cleaning
###############################################################################

# Check for missing values across all columns
colSums(is.na(BankData))

# Check for duplicate rows
duplicates <- duplicated(BankData)

# Remove duplicates
BankData <- BankData[!duplicated(BankData), ]

#After preprocessing, we save the cleaned dataset.
write.csv(BankData, "C:\\Users\\Abdulhadi\\Desktop\\PFDA Assignment\\3. credit_risk_classification.csv", 
          row.names = FALSE)

# Step 4: Example Visualization
ggplot(BankData, aes(x = age, fill = class)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  labs(title = "Age Distribution by Credit Class", x = "Age", y = "Count")


#Singling out of relevant columns for analysis

PersonalData = BankData %>% select(personal_status, existing_credits, housing, age, own_telephone)
PersonalData

#Splitting of personal_status into  gender and marital status 

library(stringr)
PersonalData$gender = str_extract(PersonalData$personal_status, "^(male|female)")
PersonalData$relationship_status = str_extract(PersonalData$personal_status, "(single|div/dep/mar|div/sep|mar/wid)$")
PersonalData

#Further simplification of relationship status into married or single

convert_relationship = function(relationship_status) {
  if (relationship_status == "mar/wid") return("married")
  if (relationship_status == "div/dep/mar") return("married")
  if (relationship_status == "single") return("single")
  if (relationship_status == "div/sep") return("n/a")
  return(NA)
}
PersonalData$mar_or_sin = sapply(PersonalData$relationship_status, convert_relationship) #Applying the fucntion to the data
PersonalData

#Creation of baseline bar chart
#Creation of a table that only contains customers by their marital status and the mean existing credits
mCredits = PersonalData %>% group_by(mar_or_sin) %>% summarize(AvgCredits = mean(existing_credits)) %>% filter (mar_or_sin %in% c("single", "married")) 
#Creation of a bar chart to display the data from mCredits
ggplot(mCredits, aes(x = mar_or_sin, y = AvgCredits, fill = mar_or_sin)) + 
  geom_bar(stat = "identity" ,position = "dodge") + 
  labs(title = "Average Credits of Customer by Marital Status", x = "Relationship Status", y = "Mean Credits") 

#Analysis 1

#Creation of a table that only contains customers by their marital status, housing type and the mean existing credits
mCreditsByHouse = PersonalData %>% group_by(mar_or_sin, housing) %>% summarize(AvgCredits = mean(existing_credits)) %>% filter (mar_or_sin %in% c("single", "married")) 
#Creation of a bar chart to display the data from mCreditsByHouse
ggplot(mCreditsByHouse, aes(x = mar_or_sin, y = AvgCredits, fill = housing)) + 
  geom_bar(stat = "identity" ,position = "dodge") + 
  labs(title = "Average Credits of Customer by Marital Status and Housing Status", x = "Relationship Status", y = "Mean Credits") 

#Analysis 2

#Creation of a table that only contains customers by their marital status, telephone ownership and the mean existing credits
mCreditsByPhone = PersonalData %>% group_by(mar_or_sin, own_telephone) %>% summarize(AvgCredits = mean(existing_credits)) %>% filter (mar_or_sin %in% c("single", "married")) #Creation of a table that only contains customers by their marital status, telephone ownership and the mean existing credits
#Creation of a bar chart to display the data from mCreditsByPhone
ggplot(mCreditsByPhone, aes(x = mar_or_sin, y = AvgCredits, fill = own_telephone)) + 
  geom_bar(stat = "identity" ,position = "dodge") + 
  labs(title = "Average Credits of Customer by Marital Status and Telehone ownership", x = "Relationship Status", y = "Mean Credits") 

#Analysis 3

#Splitting the different ages into age groups, which allows for each group to have a round, average and for simpler display of data. Without this, all ages will also get rounded to their means and the data will be useless
PersonalData <- PersonalData %>%
  mutate(age_group = cut(age, 
                         breaks = c(18, 25, 35, 45, 60, Inf),#Setting the break points for age groups
                         labels = c("18-25", "26-35", "36-45", "46-60", "60+"), #Setting labels
                         right = FALSE)) 
#Creation of a table that only contains customers by their marital status, age group and the mean existing credits
mCreditsByAge = PersonalData %>% group_by(mar_or_sin, age_group) %>% summarize(AvgCredits = mean(existing_credits)) %>% filter (mar_or_sin %in% c("single", "married"))
#Creation of a line chart to display the data from mCreditsByAge
ggplot(mCreditsByAge, aes(x = age_group, y = AvgCredits, color = mar_or_sin, group = mar_or_sin)) + 
  geom_line() + 
  geom_point() +
  labs(title = "Average Credits of Customers by Marital Status and Age")
