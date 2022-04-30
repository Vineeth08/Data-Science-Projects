# Loading heart attack analysis dataset and replace any missing variable with NA values
heart_data <- read.csv("heart.csv", na = "")
heart_data
str(heart_data)

sum(is.na(heart_data))
na_count <- heart_data[!complete.cases(heart_data),]
na_count

summary(heart_data)

min(heart_data$age)
max(heart_data$age)

heart_data$AgeCatgory[heart_data$age >= 60] <- "Elder"
heart_data$AgeCatgory[heart_data$age >= 41 & heart_data$age <= 59] <- "Middle Aged"
heart_data$AgeCatgory[heart_data$age <= 40] <- "Young"

sum(is.na(heart_data))

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))

hist(heart_data$trtbps, 
     main = "Histogram for Blood Pressure", 
     xlab = "Patients")

hist(heart_data$chol, 
     main = "Histogram for Cholestrol", 
     xlab = "Patients")

hist(heart_data$thalachh, 
     main = "Histogram for maximum heart rate", 
     xlab = "Patients")

hist(heart_data$oldpeak, 
     main = "Histogram for Old Peak", 
     xlab = "Patients")

qqnorm(heart_data$chol, main='Checking for Normality for Cholestrol measurement')
qqline(heart_data$chol)

qqnorm(heart_data$trtbps, main='Checking for Normality for Blood Pressure')
qqline(heart_data$trtbps)

qqnorm(heart_data$thalachh, main='Checking for Normality for maximum heart rate')
qqline(heart_data$thalachh)

qqnorm(heart_data$oldpeak, main='Checking for Normality for old peak')
qqline(heart_data$oldpeak)


# https://www.statology.org/test-for-normality-in-r/

shapiro.test(heart_data$trtbps)
shapiro.test(heart_data$thalachh)
shapiro.test((heart_data$chol))
shapiro.test(heart_data$oldpeak)
ks.test(heart_data$chol, 'pnorm')
ks.test(heart_data$trtbps, 'pnorm')

max(heart_data$chol)
min(heart_data$chol)

heart_data1 <- heart_data

heart_data$cholnew[heart_data$chol > 500] <- NA
heart_data$cholnew[heart_data$chol <= 500] <- "OK"

sum(is.na(heart_data$cholnew))



nrow(heart_data1)
max(heart_data1$chol)
heart_data1$cholnew <- heart_data1$chol
heart_data1$cholnew[heart_data1$chol > 500] <- NA
sum(is.na(heart_data1$cholnew))

heart_data1 <- na.omit(heart_data1)
nrow(heart_data2)

hist(heart_data2$cholnew, 
     main = "Histogram for Cholestrol", 
     xlab = "Patients")

shapiro.test((heart_data1$cholnew))
shapiro.test((heart_data1$thalachh))
shapiro.test((heart_data1$trtbps))
shapiro.test((heart_data1$oldpeak))

# https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

install.packages("moments")
library(moments)
skewness(heart_data1$chol, na.rm = TRUE)
skewness(heart_data1$trtbps, na.rm = TRUE)
skewness(heart_data1$thalachh, na.rm = TRUE)
skewness(heart_data1$oldpeak, na.rm = TRUE)

install.packages("ggpubr")
library(ggpubr)


ggdensity(heart_data1, x = "chol", fill = "lightgray", title = "Cholestrol") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(heart_data1, x = "trtbps", fill = "lightgray", title = "Cholestrol") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

heart_data1$choltrans <- log10(heart_data1$chol)

ggdensity(heart_data1, x = "choltrans", fill = "lightgray", title = "Cholestrol") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
shapiro.test((heart_data1$choltrans))

heart_data1$trbpstrans <- log10(heart_data1$trtbps)

ggdensity(heart_data1, x = "trbpstrans", fill = "lightgray", title = "blood pressure") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
shapiro.test((heart_data1$trbpstrans))

heart_data1$trbpstrans <- sqrt(heart_data1$trtbps)
shapiro.test((heart_data1$trbpstrans))

ggdensity(heart_data1, x = "thalachh", fill = "lightgray", title = "maximum heart rate") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

heart_data1$thalachhtrans <- sqrt(max(heart_data1$thalachh + 1)- heart_data1$thalachh)
ggdensity(heart_data1, x = "thalachhtrans", fill = "lightgray", title = "maximum heart rate") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
shapiro.test((heart_data1$thalachhtrans))


