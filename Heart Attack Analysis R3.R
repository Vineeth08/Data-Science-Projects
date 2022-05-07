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

# Using the default pairs() option first
# to examine correlations between variables
pairs(heart_data, labels = colnames(heart_data), main = "Heart dataset correlation plot")

# We can use libraries to help improve 
# the chart. Also includes correlations between variables
install.packages("psych")
library(psych)

pairs.panels(heart_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


# Age is a influential criteria in predicting heart attack along with other attributes
# Does for better understanding, a new column can be created and categorize the age
# into 3 segments
# Before categorizing, lets understand the minimum and maximum value in the range 
# of column age

min(heart_data$age)
max(heart_data$age)

# in the current data set, the minimum and maximum age is 29years and 77years
# Hence the age is segmented into 3 groups namely 'young', 'Middle Age' and Elderly

heart_data$AgeCatgory[heart_data$age >= 55] <- "Elder"
heart_data$AgeCatgory[heart_data$age >= 41 & heart_data$age <= 54] <- "Middle Aged"
heart_data$AgeCatgory[heart_data$age <= 40] <- "Young"

sum(is.na(heart_data))

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))

# The first approach is to study the columns having continuous data
# Secondly, there is a need to check the normality of each attribute having 
# continuous data. The hypothesis tests for continuous data depends if a data
# follows normal distribution or non-normal distribution. This can help in
# selecting the appropriate test for verifying the hypothesis
# First method to identify if a data is normal is through plotting histogram 
# and checking if it looks like a bell curve
# Note that for all categorical data, it will be transformed using factor during
# hypothesis testings as normality test cannot be used for dichotomous data

hist(heart_data$trtbps, 
     main = "Histogram for Blood Pressure", 
     xlab = "Blood Pressure in mm/hg", 
     col = "red")

hist(heart_data$chol, 
     main = "Histogram for Cholestrol", 
     xlab = "Cholestrol in mg/dl", 
     col = "red")

hist(heart_data$thalachh, 
     main = "Histogram for maximum heart rate", 
     xlab = "Max heart rate", 
     col = "red")

hist(heart_data$oldpeak, 
     main = "Histogram for Old Peak", 
     xlab = "ST- Segment", 
     col = "red")

# The cholesterol measure and resting blood pressure seems to be normal in shape
# But there are outliers, hence lets explore Q-Q plots to verify.
# in Q-Q plot if points roughly fall on a straight line, then it can be considered
# normal distribution. The function used is qqnorm()

qqnorm(heart_data$chol, main='Checking Normality for Cholestrol measurement')
qqline(heart_data$chol, col = "blue")

qqnorm(heart_data$trtbps, main='Checking Normality for Blood Pressure')
qqline(heart_data$trtbps, col = "blue")

qqnorm(heart_data$thalachh, main='Checking Normality for maximum heart rate')
qqline(heart_data$thalachh, col = "blue")

qqnorm(heart_data$oldpeak, main='Checking Normality for old peak')
qqline(heart_data$oldpeak, col = "blue")

# Q-Q plot seems accurate for cholestrol and blood pressure but it is slight out of order
# in the end. Thus lets check normality using Shapiro-Wilk test. For data to be normal,
# p-value should be greater than 0.05.

# https://www.statology.org/test-for-normality-in-r/

par(opar)

shapiro.test(heart_data$trtbps)
shapiro.test(heart_data$thalachh)
shapiro.test((heart_data$chol))
shapiro.test(heart_data$oldpeak)

# All 4 parameters have values p value less than 0.05. Hence no parameter
# has a normal distribution. In order to run parametric tests, 
# Finding unique data in columns

unique(heart_data$chol, descending = TRUE)

hist(heart_data$chol, 
     main = "Histogram for Cholestrol before removing outlier", 
     xlab = "Cholestrol in mg/dl", 
     col = "red")

heart_data1 <- heart_data

heart_data1$cholnew[heart_data1$chol > 500] <- NA
heart_data1$cholnew[heart_data1$chol <= 500] <- "OK"

sum(is.na(heart_data1$cholnew))

# Only 1 value in the dataset for cholestrol reading as 564mg/dl after the reading 417mg/dl 
# from all readings. Clearly its an outlier and need to be removed (refer histogram). 
# This might improve the chances of making it a normal distribution

heart_data1 <- na.omit(heart_data1)
nrow(heart_data1)

hist(heart_data1$chol, 
     main = "Histogram for cholesterol after removing outlier", 
     xlab = "cholesterol in mg/dl", 
     col = "blue")
# No outliers, lets check the p value using Shapiro Wilk test for Cholestrol

shapiro.test((heart_data1$chol))

# P value = 0.00109, less than 0.05. Hence data is still not Normal but still very close 
# to it as per Shapiro test. 
# Due to slight right skew, it still cannot be considered as normal distribution
# Does there is a need to apply transformation on all 4 parameters and again 
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


ggdensity(heart_data1, x = "chol", fill = "lightgray", title = "Density curve for Cholestrol") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(heart_data1, x = "trtbps", fill = "lightgray", title = "Density curve for Blood Pressure") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Apply logarithmic transformation as the data is positively skewed (log10(df))

heart_data1$choltrans <- log10(heart_data1$chol)

ggdensity(heart_data1, 
          x = "choltrans", 
          fill = "lightgray", 
          title = "Density curve for Cholestrol after transformation") +
  stat_overlay_normal_density(color = "red", 
                              linetype = "dashed")

shapiro.test((heart_data1$choltrans))

# Shapiro test for transformed cholesterol data indicates P value = 0.6

heart_data1$trbpstrans <- log10(heart_data1$trtbps)

ggdensity(heart_data1, x = "trbpstrans", 
          fill = "lightgray", 
          title = "Density curve for blood pressure after transformation") +
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
          fill = "lightgray", title = "maximum heart rate after transformation") +
  stat_overlay_normal_density(color = "red", 
                              linetype = "dashed")

shapiro.test((heart_data1$thalachhtrans))

# P value is 0.2. Hence transformed maximum heart rate can be treated as normal data

# Summary
# chol trans- Normal
# trbp trans - Non Normal
# thalacch trans -  Normal
# old peak- Non normal (No transformation applied)

# ---------------Hypothesis 1------------------------------
# Research Question- To verify if the cholesterol level is different for men and women
# Null hypothesis (Ho) - No difference in cholesterol between men and women
# Alternative hypothesis (H1)- There is significant difference between cholesterol
# level between men and women
# Transformed cholesterol value is continuous data
# Sex column is categorical. Hence needs to be converted
  
heart_data1$sex_transform <- factor(heart_data1$sex, labels = c("Female", "Male"))

attach(heart_data1)
plot(sex_transform, choltrans, pch = 19, col = "lightblue")

install.packages("dplyr")
library(dplyr)

group_by(heart_data1, sex_transform) %>%
  summarise(
    mean = mean(choltrans, na.rm = TRUE),
    sd = sd(choltrans, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(heart_data1, x = "sex_transform", y = "choltrans", 
          main = "Boxplot for cholesterol as per gender",
          color = "sex_transform", palette = c("#00AFBB", "#E7B800"),
          ylab = "cholestrol mg/dl", xlab = "sex")

# The box plots indicates that there is very less difference among mean values

library("lattice")
attach(heart_data1)
histogram(~choltrans | sex_transform, 
          data = heart_data1, 
          main = "Distribution of cholestrol in male and female", 
          xlab = "cholestrol mg/dl", 
          ylab = "sex")
detach(heart_data1)


with(heart_data1,
     qqplot(choltrans[sex_transform == "Male"],
            choltrans[sex_transform == "Female"], 
            main = "Comparing Male and Female data", 
            xlab = "Male",
            ylab =  "Female"))

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution

with(heart_data1, {
  qqnorm(choltrans[sex_transform == "Male"], 
         main = "Male cholesterol level")
  qqline(choltrans[sex_transform == "Male"])
})


with(heart_data1, {
  qqnorm(choltrans[sex_transform == "Female"], 
         main = "Female cholesterol level")
  qqline(choltrans[sex_transform == "Female"])
})

with(heart_data1, tapply(choltrans, sex_transform, shapiro.test))
# Individual p-value for Shapiro Wilk test for Male and Female suggest that it is
# Normally distributed


# Selecting the appropriate test
# From the chart uploaded on blackboard, transformed cholesterol value is dependent
# Transformed sex value is independent

shapiro.test(heart_data1$choltrans)

# Transformed cholesterol value is normally distributed since P value from Shapiro-Wilk
# test is 0.67 (greater than 0.05)
# Hypothesis test-  Parametric test (Two tailed T-test)

# Compute t-test
res <- t.test(choltrans ~ sex_transform, data = heart_data1, var.equal = TRUE)
res

# P-value for T-test is 0.0054 i.e. less than 0.025 (Two sided test) 
# Hence alternate hypothesis is true.
# There is significant difference between cholesterol level between men and women

#-------- hypothesis 2------------
# Research Question- Verify if high cholesterol level increase chances heart attack?
# The target values suggest 0- less chance of heart attack 1- more chance of heart attack
# Null hypothesis (Ho) - Cholesterol level does not impact chances of heart attack
# Alternative hypothesis (H1)- More cholesterol will lead to more chances in heart attack.
# Ha can be restated that true difference in means between 2 group, i.e. u0<u1

heart_data1$output_transform <- factor(heart_data1$output, labels = c("Less Chances", "More Chances"))

attach(heart_data1)
plot(output_transform, choltrans, pch = 19, col = "lightblue")

#install.packages("dplyr")
library(dplyr)

group_by(heart_data1, output_transform) %>%
  summarise(
    mean = mean(choltrans, na.rm = TRUE),
    sd = sd(choltrans, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(heart_data1, x = "output_transform", y = "choltrans", 
          main = "Boxplot for cholesterol and chances for heart attack",
          color = "output_transform", palette = c("#00AFBB", "#E7B800"),
          ylab = "cholestrol mg/dl", xlab = "output")

# The box plots indicates that there is very less difference among mean values
# Surprisingly, the box plot is indicating the mean value of cholesterol with less chances are slightly
# higher than cholesterol level with more chance for heart attack

library("lattice")
attach(heart_data1)
histogram(~choltrans | output_transform, 
          data = heart_data1, 
          main = "Distribution of cholestrol as per chances in heart attack", 
          xlab = "cholestrol mg/dl", 
          ylab = "chances in heart attack")
detach(heart_data1)


with(heart_data1,
     qqplot(choltrans[output_transform == "Less Chances"],
            choltrans[output_transform == "More Chances"], 
            main = "Comparing chances of heart attack", 
            xlab = "Less Chances",
            ylab =  "More Chances"))

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution

with(heart_data1, {
  qqnorm(choltrans[output_transform == "Less Chances"], 
         main = "cholesterol level (Less chances)")
  qqline(choltrans[output_transform == "Less Chances"])
})

# And we can change for active period
# = "yes"
# "yes" occurrences seem to be more 
# normally distributed than the "no" answers
with(heart_data1, {
  qqnorm(choltrans[output_transform == "More Chances"], 
         main = "cholesterol level (more chances)")
  qqline(choltrans[output_transform == "More Chances"])
})

with(heart_data1, tapply(choltrans, output_transform, shapiro.test))
# Individual p-value for Shapiro Wilk test for heart attack chances suggest that it is
# Normally distributed


# Selecting the appropriate test
# From the chart uploaded on blackboard, transformed cholesterol value is dependent
# transformed output value is independent

shapiro.test(heart_data1$choltrans)

# choltrans is a normally distributed data, hence parametric test
# Hypothesis test-  One tailed T-test
# One tailed is because, alternate hypothesis is to prove whether more cholesterol can lead to higher chances in heart attack 
# Mean (more chances) > Mean (less chances)
help(t.test)

res <- t.test(choltrans ~ output, data = heart_data1, var.equal = TRUE, alternative = "greater")
res
# The 2-sample t-test describes the p-value to be 0.02944 which is less than 0.05, 
# the research state that alternate hypothesis is true. 
#There is a statistical significance between more value of cholesterol level
# impacting chances of heart attack.

#------------------- hypothesis 3-----------------
# Research Question- Verify if the Blood Pressure level has any significant impact on heart attack?
# The target values suggest 0- less chance of heart attack 1- more chance of heart attack
# Null hypothesis (Ho) - Blood Pressure level does not impact chances of heart attack
# Alternative hypothesis (H1)- Blood pressure has significant impact on chances of heart attack.
# Ha can be restated that true difference in means between 2 group, i.e. u0<u1

group_by(heart_data1, output_transform) %>%
  summarise(
    mean = mean(trtbps, na.rm = TRUE),
    sd = sd(trtbps, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(heart_data1, x = "output_transform", y = "trtbps", 
          main = "Boxplot for blood pressure with chances of heart attack",
          color = "output_transform", palette = c("#00AFBB", "#E7B800"),
          ylab = "blood pressure mm/hg", xlab = "output")

# The box plots indicates that there is very less difference among mean values

# plotting histogram of categorical data

attach(heart_data1)
histogram(~trtbps | output_transform, 
          data = heart_data1, 
          main = "Distribution of blood pressure as per chances in heart attack", 
          xlab = "blood pressure mm/hg", 
          ylab = "chances in heart attack")
detach(heart_data1)

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution

with(heart_data1, {
  qqnorm(trtbps[output_transform == "Less Chances"], 
         main = "blood pressure level (Less chances)")
  qqline(trtbps[output_transform == "Less Chances"])
})


with(heart_data1, {
  qqnorm(trtbps[output_transform == "More Chances"], 
         main = "blood pressure level (More chances)")
  qqline(trtbps[output_transform == "More Chances"])
})

with(heart_data1, tapply(trtbps, output_transform, shapiro.test))
# Individual p-value for Shapiro Wilk test for heart attack chances suggest that it is
# not Normally distributed


# Selecting the appropriate test
# From the chart uploaded on blackboard, Blood Pressure value is dependent
# transformed output value is independent

shapiro.test(heart_data1$trtbps)

# Blood Pressure value is a normally distributed data, hence non-parametric test
# Hypothesis test-  Wilcoxon- Mann Whitney Test

res <- wilcox.test(trtbps ~ output_transform, data = heart_data1, 
                   alternative = "two.sided")
res


# p value is 0.0395, p value > 0.025 (two tailed) hence null hypothesis is true. 
# Blood Pressure is indicating not having significant impact
# in causing more or less chances for heart attack as per data


#--------------------hypothesis 4-----------------------

# Research Question-  verify if the maximum heart rate level is different as per age category
# Null hypothesis (Ho) - No difference in maximum heart rate level at any age group
# Alternative hypothesis (H1)- There is significant difference between maximum heart rate
# level among various age group

group_by(heart_data1, AgeCatgory) %>%
  summarise(
    mean = mean(thalachhtrans, na.rm = TRUE),
    sd = sd(thalachhtrans, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(heart_data1, x = "AgeCatgory", y = "thalachhtrans", 
          main = "Boxplot for maximum heart rate as per age category",
          color = "AgeCatgory", 
          ylab = "Maximum Heart Rate", xlab = "Age Category")

# The box plots indicates that there is significant difference among mean values

# plotting histogram of categorical data

attach(heart_data1)
histogram(~thalachhtrans | AgeCatgory, 
          data = heart_data1, 
          main = "Distribution of max. heart rate as per age category", 
          xlab = "maximum heart rate", 
          ylab = "age category")
detach(heart_data1)

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,3))

with(heart_data1, {
  qqnorm(thalachhtrans[AgeCatgory == "Young"], 
         main = "max heart rate (Young)")
  qqline(thalachhtrans[AgeCatgory == "Young"])
})


with(heart_data1, {
  qqnorm(thalachhtrans[AgeCatgory == "Middle Aged"], 
         main = "max heart rate (Middle Age)")
  qqline(thalachhtrans[AgeCatgory == "Middle Aged"])
})

with(heart_data1, {
  qqnorm(thalachhtrans[AgeCatgory == "Elder"], 
         main = "max heart rate (Elder)")
  qqline(thalachhtrans[AgeCatgory == "Elder"])
})


with(heart_data1, tapply(thalachhtrans, AgeCatgory, shapiro.test))
# Individual p-value for Shapiro Wilk test for heart attack chances suggest that it is
# Normally distributed


# Selecting the appropriate test
# From the chart uploaded on blackboard, maximum heart rate value is dependent
# Age category value is independent

shapiro.test(heart_data1$thalachhtrans)

# Transformed max heart rate is a normally distributed data, hence parametric test
# Hypothesis test-  One way ANOVA as comparison of means among 3 independent group

summary(aov(thalachhtrans ~ AgeCatgory, data = heart_data1))

# p value is 0.0000...., hence alternate hypothesis is true. max heart rate 
# has significant difference among different age categories

par(opar)

# -------- hypothesis 5 using chi square test-----------

# To verify if the type of chest pain and heart attack chances has any significant relationship
# Null hypothesis (Ho) - No significant relationship between chest pain type and chances of heart attack
# Alternative hypothesis (H1)- There is significant relationship between chest pain type and chances of heart attack
# Hypothesis test-  Chi-Square Test (2 dependent variable is categorical)

heart_data1$cp_transform <- factor(heart_data1$cp, labels = c("typical angina",
                                                              "atypical angina",
                                                              "non-anginal pain", 
                                                              "asymptomatic"))

chestpain <-  table(heart_data1$cp_transform,heart_data1$output_transform)
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

res <- chisq.test(heart_data1$cp, heart_data1$output, correct=FALSE)
res
# since p value is 2x10^-16 which is very less than 0.05, it indicates that chest pain have significant
# relationship with heart attack

