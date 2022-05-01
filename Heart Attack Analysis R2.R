# Loading heart attack analysis dataset and replace any missing variable 
# with NA values
heart_data <- read.csv("heart.csv", na = "")
heart_data

# studying the structure of data. All attributes are integers. Some are categorical 
# others are numerical

str(heart_data)

# Finding out the total NA values in the dataframe.
sum(is.na(heart_data))
na_count <- heart_data[!complete.cases(heart_data),]
na_count

# When the !complete.cases is implemented, all rows will indicate 0 null values

summary(heart_data)

# Age is a influential criteria in predicting heart attack along with other attributes
# Does for better understanding, a new column can be created and categorize the age
# into 3 segments
# Before categorizing, lets understand the minimum and maximum value in the range 
# of column age

min(heart_data$age)
max(heart_data$age)

# in the current data set, the minimum and maximum age is 29years and 77years
# Hence the age is segmented into 3 groups namely 'young', 'Middle Age' and Elderly

heart_data$AgeCatgory[heart_data$age >= 60] <- "Elder"
heart_data$AgeCatgory[heart_data$age >= 41 & heart_data$age <= 59] <- "Middle Aged"
heart_data$AgeCatgory[heart_data$age <= 40] <- "Young"

sum(is.na(heart_data))

# Setting graphical parameters to 2 rows and 2 columns for a better visualization

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))

# The first approach is to study the columns having continuous data
# Secondly, there is a need to check the normality of each attribute having 
# continuous data. The hypothesis tests for continuous data depends if a data
# follows normal distribution or non-normal distribution. This can help in
# selecting the appropriate test for verifying the hypothesis
# First method to identify if a data is normal is through plotting histogram 
# and checking if it looks like a bell curve

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

# The cholesterol measure and resting blood pressure seems to be normal in shape
# But there are outliers, hence lets explore Q-Q plots to verify.
# in Q-Q plot if points roughly fall ona straight line, then it can be considered
# normal distribution. The function used is qqnorm()

qqnorm(heart_data$chol, main='Checking for Normality for Cholestrol measurement')
qqline(heart_data$chol)

qqnorm(heart_data$trtbps, main='Checking for Normality for Blood Pressure')
qqline(heart_data$trtbps)

qqnorm(heart_data$thalachh, main='Checking for Normality for maximum heart rate')
qqline(heart_data$thalachh)

qqnorm(heart_data$oldpeak, main='Checking for Normality for old peak')
qqline(heart_data$oldpeak)

# Q-Q plot seems accurate for cholestrol and blood pressure but it is slight out of order
# in the end. Thus lets check normality using Shapiro-Wilk test. For data to be normal,
# p-value should be greater than 0.05. 

# https://www.statology.org/test-for-normality-in-r/

shapiro.test(heart_data$trtbps)
shapiro.test(heart_data$thalachh)
shapiro.test((heart_data$chol))
shapiro.test(heart_data$oldpeak)

# All 4 parameters have values p value less than 0.05. Hence no parameter
# has a normal distribution. In order to run parametric tests, 

# Kolmogorov-Smirnov Test

ks.test(heart_data$chol, 'pnorm')
ks.test(heart_data$trtbps, 'pnorm')

max(heart_data$chol)
min(heart_data$chol)

heart_data1 <- heart_data

heart_data$cholnew[heart_data$chol > 500] <- NA
heart_data$cholnew[heart_data$chol <= 500] <- "OK"

sum(is.na(heart_data$cholnew))

# Only 1 value in the dataset for cholestrol reading is above 500 from all readings
# Clearly its an outlier and need to be removed. This might improve the chances 
# of making it a normal distribution (as per histogram)

nrow(heart_data1)
max(heart_data1$chol)
heart_data1$cholnew <- heart_data1$chol
heart_data1$cholnew[heart_data1$chol > 500] <- NA
sum(is.na(heart_data1$cholnew))

heart_data1 <- na.omit(heart_data1)
nrow(heart_data1)

par(opar)

hist(heart_data1$cholnew, 
     main = "Histogram for Cholestrol", 
     xlab = "Patients")

shapiro.test((heart_data1$cholnew))
shapiro.test((heart_data1$thalachh))
shapiro.test((heart_data1$trtbps))
shapiro.test((heart_data1$oldpeak))

# p value for cholestrol after removal of outlier is 0.01. Even though less 
# than 0.05, still very close to it as per Shapiro test. 
# Due to slight right skew, it still cannot be considered as normal distribution
# Does there is a need to apply transformation on all 4 paremeters and again 
# test for normality

# https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

# Transformation methods depends on skewness, does installing moments package
# to check skewness

install.packages("moments")
library(moments)
skewness(heart_data1$chol, na.rm = TRUE)   # Positive Skew
skewness(heart_data1$trtbps, na.rm = TRUE) # Positive Skew
skewness(heart_data1$thalachh, na.rm = TRUE) # Negative Skew
skewness(heart_data1$oldpeak, na.rm = TRUE) # Positive Skew

install.packages("ggpubr")
library(ggpubr)


ggdensity(heart_data1, x = "chol", fill = "lightgray", title = "Cholestrol") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(heart_data1, x = "trtbps", fill = "lightgray", title = "Blood Pressure") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Apply logarithmic transformation as the data is positively skewed (log10(df))

heart_data1$choltrans <- log10(heart_data1$chol)

ggdensity(heart_data1, 
          x = "choltrans", 
          fill = "lightgray", 
          title = "Cholestrol") +
  stat_overlay_normal_density(color = "red", 
                              linetype = "dashed")

shapiro.test((heart_data1$choltrans))

# Shapiro test for transformed cholesterol data indicates P value = 0.6

heart_data1$trbpstrans <- log10(heart_data1$trtbps)

ggdensity(heart_data1, x = "trbpstrans", 
          fill = "lightgray", 
          title = "blood pressure") +
  stat_overlay_normal_density(color = "red", 
                              linetype = "dashed")

shapiro.test((heart_data1$trbpstrans))

heart_data1$trbpstrans <- sqrt(heart_data1$trtbps)

shapiro.test((heart_data1$trbpstrans))

# P-value for blood pressure is still less than 0.05 after transformation. 
# Hence Blood Pressure will undergo non parametric test

ggdensity(heart_data1, x = "thalachh", 
          fill = "lightgray", 
          title = "maximum heart rate") +
  stat_overlay_normal_density(color = "red", 
                              linetype = "dashed")

# Negatively skewed, hence apply square root transformation as below

heart_data1$thalachhtrans <- sqrt(max(heart_data1$thalachh + 1)- heart_data1$thalachh)

ggdensity(heart_data1, 
          x = "thalachhtrans", 
          fill = "lightgray", title = "maximum heart rate") +
  stat_overlay_normal_density(color = "red", 
                              linetype = "dashed")

shapiro.test((heart_data1$thalachhtrans))

# P value is 0.2. Hence transformed maximum heart rate can be treated as normal data

# Summary
# chol trans- Normal
# trbp trans - Non Normal
# thalacch trans -  Normal
# old peak- Non normal

# ---------------Hypothesis 1------------------------------
# To verify if the cholestrol level is different for men and women
# Null hypothesis (Ho) - No difference in cholestrol between men and women
# Alternative hypothesis (Ha)- There is significant difference between cholestrol
# level between men and women
# Hypothesis test-  Two tailed T-test

#http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

install.packages("dplyr")
library(dplyr)

group_by(heart_data1, sex) %>%
  summarise(
    mean = mean(chol, na.rm = TRUE),
    sd = sd(chol, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(heart_data1, x = "sex", y = "chol", 
          color = "sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "cholestrol", xlab = "sex")

# The box plots indicates that there is very less difference among median values
# Note 0- Female, 1- Male

with(heart_data1, shapiro.test(chol[sex == 0]))
with(heart_data1, shapiro.test(chol[sex == 1]))

# Box plot for transformed data

ggboxplot(heart_data1, x = "sex", y = "choltrans", 
          color = "sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "cholestrol", xlab = "sex")

# Compute t-test
#res <- t.test(chol ~ sex, data = heart_data1, var.equal = TRUE)
#res

with(heart_data1, shapiro.test(choltrans[sex == 0]))
with(heart_data1, shapiro.test(choltrans[sex == 1]))

# Compute t-test
res <- t.test(choltrans ~ sex, data = heart_data1, var.equal = TRUE)
res

# P-value for T-test is 0.0054 i.e. less than 0.025 (Two sided test) 
# Hence alternate hypothesis is true.
# There is significant difference between cholesterol level between men and women

#------------------------------hypothesis 2---------------------

# To verify if the Blood Pressure level is different for men and women
# Null hypothesis (Ho) - No difference in Blood Pressure between men and women
# Alternative hypothesis (Ha)- There is significant difference between Blood Pressure
# level between men and women
# Hypothesis test-  Wilcoxin- Mann Whitney Test

# Box plot to compare median values between 2 groups
library("ggpubr")
ggboxplot(heart_data1, x = "sex", y = "trtbps", 
          color = "sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Blood Pressure", xlab = "sex")

res <- wilcox.test(trtbps ~ sex, data = heart_data1)
res

# p value = 0.3056, hence null hypothesis is true
# No difference in Blood Pressure between men and women


#----------------hypothesis 3----------------

# To verify if the cholesterol level is different as per age category
# Null hypothesis (Ho) - No difference in cholesterol level at any age group
# Alternative hypothesis (Ha)- There is significant difference between cholesterol
# level among various age group
# Hypothesis test-  One way ANOVA as comparison of means among 3 independent group

summary(aov(choltrans ~ AgeCatgory, data = heart_data1))

# p value is 0.00201, which is less than 0.05 hence age has a significant effect on cholestrol level

#-------- hypothesis 4------------
# To verify if the cholestrol level has any significant impact on heart attack?
# The target values suggest 0- less chance of heart attack 1- more chance of heart attack
# Null hypothesis (Ho) - Cholesterol level does not impact chances of heart attack
# Alternative hypothesis (Ha)- More cholesterol will lead to more chances in heart attack.
# Ha can be restated that true difference in means between group 0 and group 1 is less than 0, i.e. u0<u1
# Hypothesis test-  One tailed T-test

ggboxplot(heart_data1, x = "output", y = "choltrans", 
          color = "output", palette = c("#00AFBB", "#E7B800"),
          ylab = "Cholesterol", xlab = "Target")

# Surprisingly, the box plot is indicating the mean value of cholesterol with less chances are slightly
# higher than cholesterol level with more chance for heart attack

res <- t.test(choltrans ~ output, data = heart_data1, var.equal = TRUE, alternative = "less")
res
# p value is 0.9706 hence, null hypothesis is true. Cholesterol is not
# impacting more or less chances for heart attack as per data

#---------- hypothesis 5---------
# Study relationship between old peak (ST depression induced by exercise relative to rest) and chest pain
# # Null hypothesis (Ho) - Old peak value is same for 4 types of chest pain
# Alternative hypothesis (Ha)- Old peak value is significantly different as per Chest Pain .
# Ha can be restated that true difference in means between group 0 and group 1 is less than 0, i.e. u0<u1
# Hypothesis test-  One tailed T-test

res <- kruskal.test(oldpeak ~ cp, data = heart_data1)
res
# old peak has significant effect on type of chest pain

#  study of categorical data

chestpain <-  table(heart_data1$cp,heart_data1$output)
prop.table((chestpain))
addmargins(chestpain)

addmargins(prop.table(chestpain,
                      margin = 2))

barplot( prop.table(chestpain,
                    margin = 2),
         legend.text = TRUE,
         ylab = "Proportion as per chest pain",
         xlab = "chances of heart attack"
)

# if 0 is less chance of heart attcak and 1 is more chance of heart attack, 
# its learnt that non anginal pain has can lead to major chances in heart attack

# -------- hypothesis 6 using chi square test-----------
chestpain
res <- chisq.test(heart_data1$cp, heart_data1$output, correct=FALSE)
res
# since p value is very less than 0.05, it indicates that chest pain have significant
# relationship with heart attack


age_analysis <-  table(heart_data1$AgeCatgory,heart_data1$output)
prop.table((age_analysis))
addmargins(age_analysis)

addmargins(prop.table(age_analysis,
                      margin = 2))

barplot( prop.table(age_analysis,
                    margin = 2),
         legend.text = TRUE,
         ylab = "Proportion as per age criteria",
         xlab = "chances of heart attack"
)

# age between 40 to 60 has highest chances of having a heart attack. This might be because of higher sample collection
# within that age group

