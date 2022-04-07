file_data <- read.csv("data_brexit_referendum.csv")
str(file_data)
#Q4
# count no of "-1" values in the leave variable and then replace with NA
# use sum for the count first

sum(file_data$Leave[file_data$Leave == -1])

# replace -1 with NA, there are 267 -1 values
file_data$Leave[file_data$Leave == -1] <- NA

#verify if the replacement is successful

summary(file_data$Leave)

sum(is.na(file_data$Leave))
#another way
na_records <- file_data[!complete.cases(file_data),]
na_records
nrow(na_records)

nrow(file_data)

# use the mice package to evaluate the data
#for missing content
install.packages("mice")
library(mice)

md.pattern(file_data, rotate.names = TRUE)
?md.pattern
?aggr


install.packages("VIM")
library(VIM)
missing_values <- aggr(file_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

# Create a new variable called Proportion that contains a percentage value for each row (ward) within the dataset
# U. Calculate the percent of leave votes and store it in the Proportion variable.

file_data$Proportion <- ((file_data$Leave / file_data$NVotes)*100)
file_data$Proportion

# Now we’ll use this Proportion variable to decide whether voters decided to
#leave or remain for each ward. If the percentage value stored in each row
#(ward) in the Proportion variable is less than or equal to 0.5 then we will
#assume that voters decided to remain in that ward. Otherwise we’ll assume
#that they voted to leave in that ward. Create a variable called Vote to store
#the Leave or Remain decision.
file_data

file_data$Vote[file_data$Proportion <= 50] <- "Remain"
file_data$Vote[file_data$Proportion > 50] <- "Leave"
file_data

#Recheck the missing values

missing_values <- aggr(file_data, prop = FALSE, numbers = TRUE)
summary(missing_values)


# shorten the names of the region names so that we can display results more easily using charts
file_data$Shortened_name[file_data$RegionName == "London"] <- "L"
file_data$Shortened_name[file_data$RegionName == "North West"] <- "NW"
file_data$Shortened_name[file_data$RegionName == "North East"] <- "NE"
file_data$Shortened_name[file_data$RegionName == "South West"] <- "SW"
file_data$Shortened_name[file_data$RegionName == "South East"] <- "SE"
file_data$Shortened_name[file_data$RegionName == "East Midlands"] <- "EM"
file_data$Shortened_name[file_data$RegionName == "West Midlands"] <- "WM"
file_data$Shortened_name[file_data$RegionName == "East of England"] <- "EE"
file_data$Shortened_name[file_data$RegionName == "Yorkshire and The Humber"] <- "Y"

missing_values <- aggr(file_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

#view summary of dataframe
summary(file_data)

is.numeric(file_data$Proportion)
numeric_variable_list <- sapply(file_data, is.numeric)
numeric_variable_list

#Create a subset of dataframe
#containing only numerical data

numerical_data <- file_data[numeric_variable_list]

numerical_data
