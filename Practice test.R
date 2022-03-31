
#---------------Q1-------------------------------
ufo_data <- read.csv("ufo.csv", na = "")
ufo_data 
str(ufo_data)
head(ufo_data, 15)
nrow(ufo_data)
ncol(ufo_data)

#Q2
date_time <- ufo_data$datetime
str(date_time)
converted_date <- as.Date(ufo_data$datetime, "%m/%d/%y")
str(converted_date)

#Q3
ufo_data$datetime <- converted_date
str(ufo_data)

#Q4
?names()
names(ufo_data)
names(ufo_data)[6] <- "DurationSeconds"
names(ufo_data)[7] <- "DurationHrsMins"
names(ufo_data)[9] <- "DatePosted"


names(ufo_data)

#Q5
str(ufo_data$latitude)
ufo_data$latitude <- as.integer(ufo_data$latitude)
str(ufo_data$latitude)

#Q6

library(mice)
md.pattern(ufo_data)

library(VIM)
missing_values <- aggr(ufo_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

sum(is.na(ufo_data$datetime))
sum(is.na(ufo_data$country))

complete_data <- ufo_data[complete.cases(ufo_data),]
complete_data

is.na(ufo_data)
sum(is.na(ufo_data))/prod(dim(ufo_data))
dim(ufo_data)
prod(dim(ufo_data))
summary(missing_values)

#Q8
attach(ufo_data)
ufo_data <- ufo_data[order(shape, city),]
ufo_data

extracted_ufo_data <- subset(ufo_data, select = c(datetime, 
                                                  city, 
                                                  country, 
                                                  shape))

extracted_ufo_data

head(extracted_ufo_data,15)

detach(ufo_data)

#Q9
attach(ufo_data)
ufo_gb_disk<-subset(ufo_data, country == "gb" & shape == "disk")
ufo_gb_disk

nrow(ufo_gb_disk)

#Q10
write.csv(ufo_gb_disk, file = "ufo_gb_disk.csv")
