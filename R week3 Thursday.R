# Load the manager dataset
managers_data <- read.csv("managers.csv", na = "")
managers_data
str(managers_data)

#Dealing with missing data
#Listwise deletion
new_data <- na.omit(managers_data)
managers_data
# Doesn't work with char so needs to be converted
managers_data$Q4 <- as.integer(managers_data$Q4)
managers_data$Q5 <- as.integer(managers_data$Q5)
new_data <- na.omit(managers_data)
str(managers_data)
new_data <- na.omit(managers_data)
str(new_data)
new_data

#use complete cases to show where data rows are complete
complete_data <- complete.cases(managers_data)
complete_data
sum(complete_data)
#List the rows where data is not missing
complete_data <- managers_data[complete.cases(managers_data),]
str(managers_data)
complete_data
managers_data$Age <- as.integer(managers_data$Age)
str(managers_data)
complete_data
str(complete_data)
complete_data <- managers_data[complete.cases(managers_data),]
str(complete_data)
complete_data
?complete.cases

# Find the sum of missing age values
sum(is.na(managers_data$Age))
is.na(managers_data)

mean(is.na(managers_data$Age))
mean(is.na(managers_data))


install.packages("mice")
library(mice)

md.pattern(managers_data)

# Use VIM package for missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(managers_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

#prop is proportion
missing_values <- aggr(managers_data, prop = TRUE, numbers = FALSE)
summary(missing_values)
#------------------------------------------------
new_managers_data = read.csv("MoreData.csv", na = "")
new_managers_data
str(new_managers_data)
#---------------------- Merging Data----------------

extracted_data <- subset(new_managers_data, select = c(Date, 
                                                       Country, 
                                                       Gender, 
                                                       Age, 
                                                       Q1, 
                                                       Q2, 
                                                       Q3, 
                                                       Q4, 
                                                       Q5))

extracted_data

combined_data <- rbind(managers_data, extracted_data)

modified_managers_data <- managers_data[c(2:10)]
modified_managers_data
#modified_managers_data$Date <- as.Date(modified_managers_data$Date,
 #                      format = "%m/%d/%yy")

modified_managers_data

combined_data <- rbind(modified_managers_data, extracted_data)
combined_data

str(combined_data)
