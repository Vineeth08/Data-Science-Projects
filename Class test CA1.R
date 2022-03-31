#Q1
# Read the dataset into a data frame called london_crime

london_crime_data <- read.csv("london-crime-data.csv", na = "")
london_crime_data
# show the structure of the dataset.
str(london_crime_data)

dim(london_crime_data)
# amalgamate the month and year variables into a new variable called Date
?paste
london_crime_data$Date <- paste(london_crime_data$month, london_crime_data$year)
str(london_crime_data)

# converting the amalgamated date into date format

#london_crime_data$Date <- as.Date(london_crime_data$Date, "%m/%d/%y")
#str(london_crime_data$Date)

#Q2 Renaming the columns using name function

names(london_crime_data)
names(london_crime_data)[2] <- "Borough"
names(london_crime_data)[3] <- "MajorCategory"
names(london_crime_data)[4] <- "SubCategory"
names(london_crime_data)[5] <- "Value"
names(london_crime_data)[8] <- "CrimeDate"

#Q3

#Q4
# Plot a chart to show the summary of the borough information from 
# the data frame so that we can view where most crimes occur

summary(london_crime_data)
sum(is.na(london_crime_data$Borough))

library(VIM)
missing_values <- aggr(london_crime_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

attach(london_crime_data)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 1))

#installing vcd packages for barplots
install.packages("vcd")
library(vcd)

#Taking frequency count for total crime in Boroughs
counts <- table(london_crime_data$Borough)
counts


barplot(counts,
        main = "Simple Bar Plot",
        xlab = "Borough", ylab = "Frequency")


barplot(counts,
        main = "Simple Bar Plot",
        xlab = "Borough", ylab = "Frequency",
        col = c("red", "yellow", "green", "blue"),
        legend = rownames(counts))


#The barplot indicates that Croydon has highest level of crime where as
#City of London has lowest amount of crime


#Q5 Display the MajorCategory variable data in a pie chart

?piechart
par(mfrow = c(1, 1))
mysummary <- table(london_crime_data$MajorCategory)
mysummary
# Use table headers for chart labels
lbls3 <- paste(names(mysummary), "\n", mysummary, sep = "")
# Create the 3D pie chart
pie(mysummary, labels = lbls3,
    main = "Pie Chart indicating major crimes in London")
#The pie chart indicates that Theft and handling category crime
#is the major contributor in highest level of crimes where as
# Sexual Offence contributes to the lowest level of crimes in London

#Q6 Categorise each borough in the London_crime dataset into the general 
#area where it lies within London
#london_crime_data$Region <- london_crime_data$Borough )


london_crime_data$Region[london_crime_data$Borough == "Barking and Dagenham"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Barnet"] <- "North"
london_crime_data$Region[london_crime_data$Borough == "Bexley"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Brent"] <- "West"
london_crime_data$Region[london_crime_data$Borough == "Bromley"] <- "South"
london_crime_data$Region[london_crime_data$Borough == "Camden"] <- "North"
london_crime_data$Region[london_crime_data$Borough == "Croydon"] <- "South"
london_crime_data$Region[london_crime_data$Borough == "Ealing"] <- "West"
london_crime_data$Region[london_crime_data$Borough == "Enfield"] <- "North"
london_crime_data$Region[london_crime_data$Borough == "Greenwich"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Hackney"] <- "North"
london_crime_data$Region[london_crime_data$Borough == "Hammersmith and Fulham"] <- "West"
london_crime_data$Region[london_crime_data$Borough == "Haringey"] <- "North"
london_crime_data$Region[london_crime_data$Borough == "Harrow"] <- "West"
london_crime_data$Region[london_crime_data$Borough == "Havering"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Hillingdon"] <- "West"
london_crime_data$Region[london_crime_data$Borough == "Hounslow"] <- "West"
london_crime_data$Region[london_crime_data$Borough == "Islington"] <- "Central"
london_crime_data$Region[london_crime_data$Borough == "Kensington and Chelsea"] <- "Central"
london_crime_data$Region[london_crime_data$Borough == "Kingston upon Thames"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Lambeth"] <- "Central"
london_crime_data$Region[london_crime_data$Borough == "Lewisham"] <- "Central"
london_crime_data$Region[london_crime_data$Borough == "Merton"] <- "South"
london_crime_data$Region[london_crime_data$Borough == "Newham"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Redbridge"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Richmond upon Thames"] <- "West"
london_crime_data$Region[london_crime_data$Borough == "Southwark"] <- "Central"
london_crime_data$Region[london_crime_data$Borough == "Sutton"] <- "South"
london_crime_data$Region[london_crime_data$Borough == "Tower Hamlets"] <- "Central"
london_crime_data$Region[london_crime_data$Borough == "Waltham Forest"] <- "Central"
london_crime_data$Region[london_crime_data$Borough == "Wandsworth"] <- "East"
london_crime_data$Region[london_crime_data$Borough == "Westminster"] <- "Central"


sum(is.na(london_crime_data$Region))

#Total 86 NA values. This can be categorised into a different section named "others"


# Display which region in London has the highest recorded crime rate
counts <- table(london_crime_data$Region)
counts


barplot(counts,
        main = "Simple Bar Plot",
        xlab = "Region", ylab = "Frequency")


# Central Region has highest recorded crime rate.
# Total crime rate in central region is 28505.
# South Region has highest recorded crime rate.
# Total crime rate in central region is 15487.

#Q8
#extract out the subset of data that had the highest number of crimes

attach(london_crime_data)
extracted_data <- subset(london_crime_data, Region == "Central")
extracted_data
nrow(extracted_data)

par(mfrow = c(1, 1))
highest_crime <- table(extracted_data$MajorCategory)
highest_crime
# Use table headers for chart labels
lbls3 <- paste(names(highest_crime), "\n", highest_crime, sep = "")
# Create the 3D pie chart
pie(highest_crime, labels = lbls3,
    main = "Pie Chart indicating major crimes in London Central Region")

#And then extract out a subset of data that had the lowest level of crimes.
extracted_data1 <- subset(london_crime_data, Region == "South")
extracted_data1
nrow(extracted_data1)

lowest_crime <- table(extracted_data1$MajorCategory)
lowest_crime
# Use table headers for chart labels
lbls3 <- paste(names(lowest_crime), "\n", lowest_crime, sep = "")
# Create the 3D pie chart
pie(lowest_crime, labels = lbls3,
    main = "Pie Chart indicating major crimes in London South Region")


# Comment
#Theft and handling contributes to the highest number of crime in both regions as plotted above
# The propoprtion of major category looks similar in both cases

#Q9
par(mfrow = c(2, 2))

highest_crime_counts <- table(extracted_data$MajorCategory)
highest_crime_counts


barplot(counts,
        main = "Simple Bar Plot",
        xlab = "Major Category", ylab = "Frequency",
        col = c("red", "yellow", "green", "blue"),
        legend = rownames(highest_crime_counts))

lowest_crime_counts <- table(extracted_data1$MajorCategory)
lowest_crime_counts


barplot(counts,
        main = "Simple Bar Plot",
        xlab = "Major Category", ylab = "Frequency",
        col = c("red", "yellow", "green", "blue"),
        legend = rownames(lowest_crime_counts))




#Q10
# Saving the modified london_crime_data frame as london-crime-modified.csv.

write.csv(london_crime_data, file = "london-crime-modified.csv")


