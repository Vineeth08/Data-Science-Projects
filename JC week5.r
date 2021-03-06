# Store file in your working directory
# and then read into a data frame. 

brexit_data <- read.csv("data_brexit_referendum.csv")
head(brexit_data)

#What data type is this file?
class(brexit_data)

# Lets look at the structure of the file
str(brexit_data)

# --------------------------------------------------------------------------------------
# Dealing with missing data
# --------------------------------------------------------------------------------------

# The original data has missing values
# represented by the number "-1"
# We need to search for these and replace them

# Check for values in "Leave" column where
# vlaue = -1
sum(brexit_data$Leave[brexit_data$Leave == -1])

# replace this with NA
brexit_data$Leave[brexit_data$Leave == -1] <- NA

# Verify that the replace happened
sum(brexit_data$Leave[brexit_data$Leave == -1])

# View the records with NA
na_records <- brexit_data[!complete.cases(brexit_data),]
na_records

# Same count as when values were -1
nrow(na_records)

install.packages("mice")
library(mice)
# 15 records have missing NI address
# 10 have missing type
md.pattern(brexit_data)

library(VIM)

# Looks like the missing values are all in this 1 column
# for "Leave". We're not going to use it for now
# so we'll leave it as it is
missing_values <- aggr(brexit_data, prop = FALSE, numbers = TRUE)

# --------------------------------------------------------------------------------------
# Describing the brexit data
# --------------------------------------------------------------------------------------

# Lets look at the proportion of voters who
# are in favour of leaving the E.U.
# I'm creating a new variable to store this information in
# The focus of this dataset is brexit voters and their
# various attributes

brexit_data$Proportion <- brexit_data$Leave / brexit_data$NVotes
brexit_data$Proportion

# Store remain or leave in the Vote variable
brexit_data$Vote[brexit_data$Proportion <= 0.5] <- "Remain"
brexit_data$Vote[brexit_data$Proportion > 0.5] <- "Leave"
brexit_data$Vote

attach(brexit_data)
# Note that we still need to refer to the full path to the variable for
# part of the statement

# Need to convert to character from factor before we can convert the variable

brexit_data$RegionName <- as.character(RegionName)
str(brexit_data)
brexit_data$RegionName[RegionName == "London"] <- "L" # Check that this has happened
brexit_data$RegionName[RegionName == "North West"] <- "NW" 
brexit_data$RegionName[RegionName == "North East"] <- "NE" 
brexit_data$RegionName[RegionName == "South West"] <- "SW" 
brexit_data$RegionName[RegionName == "South East"] <- "SE" 
brexit_data$RegionName[RegionName == "East Midlands"] <- "EM" 
brexit_data$RegionName[RegionName == "West Midlands"] <- "WM" 
brexit_data$RegionName[RegionName == "East of England"] <- "EE" 
brexit_data$RegionName[RegionName == "Yorkshire and The Humber"] <- "Y" 

# Now we have a new variable of interest called proportion
# and we will now focus on this variable

# Lets view the summary of the data

# We can see that this shows relevant summaries of the data
# and it is tailored for each data type
summary(brexit_data)

# We're not able to directly reference this data because some is non-numeric
# and does not contai nwhat we want to view such as minimum, first quartile, median, mean, third quartile
# but we can use sapply() and check whether each data variable is a numeric variable or not
# eg to check whether proportion is a numeric value or not
is.numeric(brexit_data$Proportion)
is.numeric(brexit_data$RegionName)

# Use double-tab for structure of sapply()
numeric_variable_list <- sapply(brexit_data, is.numeric)
numeric_variable_list

# We can use this logic to create a subset of the data
numerical_data <- brexit_data[numeric_variable_list]
colnames(numerical_data)

# Store file in your working directory
# and then read into a data frame. 

brexit_data <- read.csv("data_brexit_referendum.csv")
head(brexit_data)

#What data type is this file?
class(brexit_data)

# Lets look at the structure of the file
str(brexit_data)

# --------------------------------------------------------------------------------------
# Dealing with missing data
# --------------------------------------------------------------------------------------

# The original data has missing values
# represented by the number "-1"
# We need to search for these and replace them

# Check for values in "Leave" column where
# vlaue = -1
sum(brexit_data$Leave[brexit_data$Leave == -1])

# replace this with NA
brexit_data$Leave[brexit_data$Leave == -1] <- NA

# Verify that the replace happened
sum(brexit_data$Leave[brexit_data$Leave == -1])

# View the records with NA
na_records <- brexit_data[!complete.cases(brexit_data),]
na_records

# Same count as when values were -1
nrow(na_records)

install.packages("mice")
library(mice)
# 15 records have missing NI address
# 10 have missing type
md.pattern(brexit_data)

library(VIM)

# Looks like the missing values are all in this 1 column
# for "Leave". We're not going to use it for now
# so we'll leave it as it is
missing_values <- aggr(brexit_data, prop = FALSE, numbers = TRUE)

# --------------------------------------------------------------------------------------
# Describing the brexit data
# --------------------------------------------------------------------------------------

# Lets look at the proportion of voters who
# are in favour of leaving the E.U.
# I'm creating a new variable to store this information in
# The focus of this dataset is brexit voters and their
# various attributes

brexit_data$Proportion <- brexit_data$Leave / brexit_data$NVotes
brexit_data$Proportion

# Store remain or leave in the Vote variable
brexit_data$Vote[brexit_data$Proportion <= 0.5] <- "Remain"
brexit_data$Vote[brexit_data$Proportion > 0.5] <- "Leave"
brexit_data$Vote

attach(brexit_data)
# Note that we still need to refer to the full path to the variable for
# part of the statement

# Need to convert to character from factor before we can convert the variable

brexit_data$RegionName <- as.character(RegionName)
str(brexit_data)
brexit_data$RegionName[RegionName == "London"] <- "L" # Check that this has happened
brexit_data$RegionName[RegionName == "North West"] <- "NW" 
brexit_data$RegionName[RegionName == "North East"] <- "NE" 
brexit_data$RegionName[RegionName == "South West"] <- "SW" 
brexit_data$RegionName[RegionName == "South East"] <- "SE" 
brexit_data$RegionName[RegionName == "East Midlands"] <- "EM" 
brexit_data$RegionName[RegionName == "West Midlands"] <- "WM" 
brexit_data$RegionName[RegionName == "East of England"] <- "EE" 
brexit_data$RegionName[RegionName == "Yorkshire and The Humber"] <- "Y" 

# Now we have a new variable of interest called proportion
# and we will now focus on this variable

# Lets view the summary of the data

# We can see that this shows relevant summaries of the data
# and it is tailored for each data type
summary(brexit_data)

# We're not able to directly reference this data because some is non-numeric
# and does not contai nwhat we want to view such as minimum, first quartile, median, mean, third quartile
# but we can use sapply() and check whether each data variable is a numeric variable or not
# eg to check whether proportion is a numeric value or not
is.numeric(brexit_data$Proportion)
is.numeric(brexit_data$RegionName)

# Use double-tab for structure of sapply()
numeric_variable_list <- sapply(brexit_data, is.numeric)
numeric_variable_list

# We can use this logic to create a subset of the data
numerical_data <- brexit_data[numeric_variable_list]
colnames(numerical_data)

# Remove the ID field as it has no meaning
numeric_variable_list["ID"] <- FALSE

# Then remove this again from the main brexit_data dataset
numerical_data <- brexit_data[numeric_variable_list]
colnames(numerical_data)

# We can use the lapply() function to return a named list
# where each list member has a corresponding name
lapply(numerical_data, summary)

?cbind()
# cbind() combines objects by rows and columns
# so we can use this function to create a dataset
# of the lapply() output

# do.call() function will allow us to 
# apply the cbind() to each element generated by lapply()
?do.call
# do.call(what, args, quote = FALSE, envir = parent.frame())
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))

# Error message shows additional ine - caused by NA's
# We could drop NA's but that will only fix this one issue
# Instead we need to go back to the source and drop the NA's

brexit_data <- brexit_data[complete.cases(brexit_data),]
numerical_data <- brexit_data[numeric_variable_list]
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary

# We can do a swap with summary values as columns and variables as rows 
# then we use rbind()
alt_numerical_summary <-  do.call(rbind, lapply(numerical_data, summary))
alt_numerical_summary

# Now we can apply some computations
# such as finding range between wards with the least and most proportions of votes in
# favour of leaving (0.6681), which may be useful to interpret the big difference among the
# types of wards we may find in the UK. If we want to know which wards are being used to
# get to this result, we can search for the wards with the least and most proportion of votes

# We can use this data to show specific values such as 
# the max proportion value - max percent of those who voted to leave
# prop = leave / no of votes in ward
numerical_summary["Max.", "Proportion"]

# And the min proportion value can show us 
numerical_summary["Min.", "Proportion"]

numerical_summary["Max.", "Proportion"] - numerical_summary["Min.", "Proportion"]

# Lets look at values of those who wanted to leave and stay
display_variables <- c("NoQuals", "Proportion", "AdultMeanAge", "L4Quals_plus", "RegionName")

# Values show that those with mean age of 46.9 and 20% no qualifications
# and 27.3 with level 4 quals (degree and above) voted to leave
brexit_data[which.max(brexit_data$Proportion), display_variables]

# Those who wanted to stay were younger with better qualifications
brexit_data[which.min(brexit_data$Proportion), display_variables]

table(brexit_data$RegionName)

?prop.table
# table value expressed as fractions
prop.table(table(brexit_data$RegionName))

# --------------------------------------------------------------------------------------
# Plotting values for initial comparisons
# --------------------------------------------------------------------------------------

barplot(height = prop.table(table(brexit_data$RegionName)),
        main = "Vote proportion by Region", 
        ylab = "Frequency", 
        xlab = "Region",
        col = "white")