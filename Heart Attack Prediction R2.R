heart_data <- read.csv("heart.csv", na = "")
heart_data

str(heart_data)

describe(heart_data)
# Finding out the total NA values in the dataframe.
sum(is.na(heart_data))
na_count <- heart_data[!complete.cases(heart_data),]
na_count

summary(heart_data)

# Renaming columns

names(heart_data)[3] <- "Chest_Pain"
names(heart_data)[4] <- "Resting_BP"
names(heart_data)[5] <- "Cholesterol"
names(heart_data)[6] <- "Fasting_BP"
names(heart_data)[8] <- "Max_heart_rate"
names(heart_data)[9] <- "Exercise_Induced_Angina"
names(heart_data)[11] <- "Slope"
names(heart_data)[12] <- "Major_Vessels"
names(heart_data)[13] <- "Thalassemia"
names(heart_data)[14] <- "Chances_of_Heart Attack"


# Using the default pairs() option first
# to examine correlations between variables
pairs(heart_data, labels = colnames(heart_data), main = "Heart dataset correlation plot")

# We can use libraries to help improve 
# the chart. Also includes correlations between variables
#install.packages("psych")
library(psych)

pairs.panels(heart_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = TRUE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Factorising the values to specific description that can be used for plotting and regression

heart_data$sex <- factor(heart_data$sex, labels = c("Female", "Male"))
heart_data$Chest_Pain <- factor(heart_data$Chest_Pain, labels = c("Typical Angina", 
                                                                      "Atypical angina", 
                                                                      "Non Anginal Pain", 
                                                                      "Asymptomatic"))


heart_data$restecg <- factor(heart_data$restecg, labels = c("Normal", 
                                                            "Abnormal", 
                                                            "Ventricular Hypertrophy"))

heart_data$`Exercise_Induced_Angina` <- factor(heart_data$`Exercise_Induced_Angina`, 
                                               labels = c("No", "Yes"))

heart_data$Slope <- factor(heart_data$Slope, labels = c("Upsloping",
                                                        "Flat", 
                                                        "Downsloping"))

heart_data$Fasting_BP <- factor(heart_data$Fasting_BP, labels = c("False", "True"))

table(heart_data$sex)
table(heart_data$`Chest_Pain`)
table(heart_data$restecg)
table(heart_data$`Exercise_Induced_Angina`)
table(heart_data$Slope)
table(heart_data$`Fasting_BP`)

# Plotting scatterplot among variables

scatter.smooth(x = heart_data$Cholesterol,
               y = heart_data$`Chances_of_Heart Attack`,
               xlab = "Cholesterol in mg/dl",
               ylab = "Chances of Heart Attack", 
               main = "Correlation of Heart Attack and Cholesterol")
cor(heart_data$Cholesterol, heart_data$`Chances_of_Heart Attack`)

scatter.smooth(x = heart_data$`Resting_BP`,
               y = heart_data$`Chances_of_Heart Attack`,
               xlab = "Resting Blood Pressure in mm/Hg",
               ylab = "Chances of Heart Attack", 
               main = "Correlation of Heart Attack and Blood Pressure")
cor(heart_data$`Resting_BP`, heart_data$`Chances_of_Heart Attack`)

scatter.smooth(x = heart_data$`Max_heart_rate`,
               y = heart_data$`Chances_of_Heart Attack`,
               xlab = "Maximum heart rate",
               ylab = "Chances of Heart Attack", 
               main = "Correlation of Heart Attack and maximum heart rate")
cor(heart_data$`Max_heart_rate`, heart_data$`Chances_of_Heart Attack`)

scatter.smooth(x = heart_data$age,
               y = heart_data$`Chances_of_Heart Attack`,
               xlab = "Age",
               ylab = "Chances of Heart Attack", 
               main = "Correlation of Heart Attack and Age")
cor(heart_data$age, heart_data$`Chances_of_Heart Attack`)

scatter.smooth(x = heart_data$oldpeak,
               y = heart_data$`Chances_of_Heart Attack`,
               xlab = "oldpeak",
               ylab = "Chances of Heart Attack", 
               main = "Correlation of Heart Attack and oldpeak")
cor(heart_data$oldpeak, heart_data$`Chances_of_Heart Attack`)


# Research question
# Based on the collected data of attributes, create a model that can determine 
# whether a person has less chances or more chances of having a heart attack
# Output (Dependent) variable- 'Chances of Heart Attack'
# Determine the values in output
unique(heart_data$`Chances_of_Heart Attack`)

# The values are 0 and 1 where 0 determines as 'Low Chances of Heart Attack' 
# and 1 determine as 'High Chances of Heart Attack

# Dependent variable is binary in nature. Hence Logistic Regression can be 
# implemented to create a prediction model in determining chances of heart attack

# Assumptions of Logistics Regression
# Assumption 1. Lack of strong influential outliers in variables. 
# Hence lets check outliers
attach(heart_data)
boxplot(Cholesterol, 
        main = "Cholesterol", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Cholesterol)$out)) 
boxplot.stats(Cholesterol)$out
# one extreme outlier. 
unique(heart_data$Cholesterol, descending = TRUE)
max(heart_data$Cholesterol)

# Remove the outlier row
heart_data <- subset(heart_data, heart_data$Cholesterol!=564)

boxplot(`Resting_BP`, 
        main = "Resting Blood Pressure", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`Resting BP`)$out)) 
boxplot.stats(`Resting_BP`)$out

# Even though outliers but not big difference. Hence let it remain

boxplot(`age`, 
        main = "Age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`age`)$out)) 
boxplot.stats(`age`)$out

# No outliers

boxplot(`Max_heart_rate`, 
        main = "Maximum Heart Rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`Max heart rate`)$out)) 
boxplot.stats(`Max_heart_rate`)$out
heart_data <- subset(heart_data, heart_data$`Max_heart_rate`!=71)

boxplot(oldpeak, 
        main = "Oldpeak", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`oldpeak`)$out)) 
boxplot.stats(oldpeak)$out

nrow(heart_data)

# No need to remove outliers as variance among values are very less


# Assumption 2. Check linearity in logit for independent variables
# The logit is the logarithm of the odds ratio, 
# where p = probability of a positive outcome (chances of heart attack)
# To check linearity among predictors and logit, use Box-Tidwell Test.
# It is used to check linearity among continuous independent variable and outcome
#install.packages("car")
library(car)
attach(heart_data)
boxTidwell(`Chances_of_Heart Attack` ~ age, data=heart_data)
# p-value is 0.13 which is more than 0.05. 
# Hence, Age is linearly related to the logit of the outcome variable 
# and that the assumption is satisfied.

boxTidwell(`Chances_of_Heart Attack` ~ `Max_heart_rate`, data=heart_data)
# p-value is 0.38 which is more than 0.05. If p > 0.05, linear relation exists
# Hence both variables are linearly related

boxTidwell(`Chances_of_Heart Attack` ~ `Resting_BP`, data=heart_data)
# p-value is 0.65 which is more than 0.05. If p > 0.05, linear relation exists
# Hence both variables are linearly related

boxTidwell(`Chances_of_Heart Attack` ~ Cholesterol, data=heart_data)

# Assumption 3. Absence of Multicollinearity
# Multicollinearity will be checked once the model is fit to estimate VIF values
# VIF values will indicate whether predictors in data has any correlation among themselves

attach(heart_data)

no_rows_data <- nrow(heart_data)
no_rows_data

# set.seed() will ensure that the randomness remains constant
set.seed(1)
model <- glm(formula = `Chances_of_Heart Attack` ~ ., 
             family = "binomial", 
             data = heart_data)

# Listing summary to make decisions whether to keep variables or not
# based on significance value
summary(model)

# With p-value, 6 parameters have statistical infuence in model. 
# Chest Pain, Sex, Exercise Induced Angina, oldpeak, Major vessels and Thalassemia

# R-sq value is not estimated in summary unlike for linear regression.
# Hence let's check the R-sq value by applying McFadden’s pseudo-R squared
# For logistic regression. https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/

library(pscl)
pR2(model)
# Cragg and Uhler's pseudo r-squared value is 0.67. This indicates strong relationship

library(car)
vif(model)

# VIF values are all less are more than 1 and less than 2. Hence no strong
# correlation among predictors

# Assumption 4 Residual errors of the model are normally distributed.
# Studentized residuals

library(car)
#qqPlot(model, 
#labels = row.names(heart_data), 
#id.method = "identify", 
#simulate = TRUE, 
#main = "Q-Q plot") - Not successful

# Q-Q of quantile residuals
mgcv::qq.gam(model,pch=1)

# The Q-Q plot for the model shows all points on line. This indicates that
# Residuals are normally distributed

student_fit <- rstudent(model)
hist(student_fit,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")
rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE,
      col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)


nrow(heart_data)
sample_data <- sample(1:no_rows_data, size = round(0.8 * no_rows_data), 
                      replace = FALSE)


training_data <- heart_data[sample_data, ]
testing_data <- heart_data[-sample_data, ]

nrow(training_data)
nrow(testing_data)

attach(heart_data)
set.seed(1)
model <- glm(formula = `Chances_of_Heart Attack` ~ sex + `Chest_Pain` 
             + `Exercise_Induced_Angina` + oldpeak + `Major_Vessels` 
             + Thalassemia,  
             family = "binomial", 
             data = training_data)

summary(model)
pR2(model)

predicted_heartattack  <- predict(model, testing_data)
predicted_heartattack

actuals_predictions <- data.frame(cbind(Actuals = testing_data$`Chances_of_Heart Attack`, 
                                        Predicted = predicted_heartattack))

actuals_predictions

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

fitted.results <- predict(model,testing_data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(fitted.results)
fitted.results

# Calculating accuracy and mean error
misClasificError <- mean(fitted.results != testing_data$`Chances_of_Heart Attack`)
misClasificError
print(paste('Accuracy',1-misClasificError))
# or accuracy can be described as follows (Accuracy is 0.8 or 80%)
mean(fitted.results == testing_data$`Chances of Heart Attack`)

# ------------------------- Forecasting with multiple predictor values ---------------------------------
coefficients(model)
residuals(model)

fitted.results <- data.frame(fitted.results)
nrow(fitted.results)



glm_pred = cbind(testing_data, fitted.results)

summary(model)
#---------------------Entering new values in Data Frame----------------

df <- data.frame(sex = c("Male"), `Chest_Pain` = c("Asymptomatic"), `Exercise_Induced_Angina` = c("No"),
                 oldpeak = c(3), `Major_Vessels`= c(1), Thalassemia = c(2))
df
predicted_chances <- predict(model, df)
predicted_chances
predicted_chances <- ifelse(predicted_chances > 0.5,1,0)
predicted_chances

#---------------------Example 2----------------

df <- data.frame(sex = c("Male", "Female", "Male"), 
                 `Chest_Pain` = c("Asymptomatic", "Non Anginal Pain", "Atypical angina"),
                 `Exercise_Induced_Angina` = c("No", "No", "Yes"),
                 oldpeak = c(3, 1.5, 2.3), `Major_Vessels`= c(1,0,2), 
                 Thalassemia = c(2,2,3))
df
predicted_chances <- predict(model, df)
predicted_chances
predicted_chances <- ifelse(predicted_chances > 0.5,1,0)
predicted_chances


# --------------------- The End---------------------------

