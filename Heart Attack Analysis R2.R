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

# chol trans- Normal
# trbp trans - Non Normal
# thalacch trans - Non normal
# old peak- Non normal

# 2 sample T test for cholestrol difference in men and women

# Hypothesis 1------------------------------
# Null hypothesis- No difference in cholestrol
# Alternative hypothesis- There is significant difference
# two tailes T-test

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


with(heart_data1, shapiro.test(chol[sex == 0]))
with(heart_data1, shapiro.test(chol[sex == 1]))

# Compute t-test
res <- t.test(chol ~ sex, data = heart_data1, var.equal = TRUE)
res
# p value 0.001656. Hence significant difference between cholestrol level of male and female

with(heart_data1, shapiro.test(choltrans[sex == 0]))
with(heart_data1, shapiro.test(choltrans[sex == 1]))

# Compute t-test
res <- t.test(choltrans ~ sex, data = heart_data1, var.equal = TRUE)
res

#------------------------------hypothesis 2--------------

# Checking the impact of sex on Blood Pressure using Wilcoxin- Mann Whitney Test
res <- wilcox.test(trtbps ~ sex, data = heart_data1)
res
# p value 0.3056, hence sex has no impact on blood pressure

library("ggpubr")
ggboxplot(heart_data1, x = "sex", y = "trtbps", 
          color = "sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Blood Pressure", xlab = "sex")

#----------------hypothesis 3----------------

#Cholestrol vs age category. One way Anova

summary(aov(choltrans ~ AgeCatgory, data = heart_data1))

# p value is 0.00201 hence age has a significant effect on cholestrol level

#-------- hypothesis 4---------
#impact of cholestrol in heart attack 0- less chance 1- more chance

res <- t.test(choltrans ~ output, data = heart_data1, var.equal = TRUE)
res
# p value is 0.05888 hence, it can be termed as significant

#---------- hypothesis 5---------
# impact of old peak (ST depression induced by exercise relative to rest)
# chest pain

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

