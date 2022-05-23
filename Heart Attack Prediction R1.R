heart_data <- read.csv("heart.csv", na = "")
heart_data

str(heart_data)

# Finding out the total NA values in the dataframe.
sum(is.na(heart_data))
na_count <- heart_data[!complete.cases(heart_data),]
na_count

summary(heart_data)

# Renaming columns

names(heart_data)[3] <- "Chest Pain"
names(heart_data)[4] <- "Resting BP"
names(heart_data)[5] <- "Cholesterol"
names(heart_data)[6] <- "Fasting BP"
names(heart_data)[8] <- "Max heart rate"
names(heart_data)[9] <- "Exercise Induced Angina"
names(heart_data)[11] <- "Slope"
names(heart_data)[12] <- "Major Vessels"
names(heart_data)[13] <- "Thalassemia"
names(heart_data)[14] <- "Chances of Heart Attack"


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

heart_data$sex <- factor(heart_data$sex, labels = c("Female", "Male"))
heart_data$`Chest Pain` <- factor(heart_data$`Chest Pain`, labels = c("Typical Angina", 
                                                  "Atypical angina", 
                                                  "Non Anginal Pain", 
                                                  "Asymptomatic"))


heart_data$restecg <- factor(heart_data$restecg, labels = c("Normal", 
                                                  "Abnormal", 
                                                  "Ventricular Hypertrophy"))

heart_data$`Exercise Induced Angina` <- factor(heart_data$`Exercise Induced Angina`, labels = c("No", "Yes"))

heart_data$Slope <- factor(heart_data$Slope, labels = c("Upsloping",
                                                    "Flat", 
                                                    "Downsloping"))

heart_data$`Fasting BP` <- factor(heart_data$`Fasting BP`, labels = c("False", "True"))

table(heart_data$sex)
table(heart_data$`Chest Pain`)
table(heart_data$restecg)
table(heart_data$`Exercise Induced Angina`)
table(heart_data$Slope)
table(heart_data$`Fasting BP`)

# Finding outliers
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

boxplot(`Resting BP`, 
        main = "Resting Blood Pressure", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`Resting BP`)$out)) 
boxplot.stats(`Resting BP`)$out

# Even though outliers but not big difference. Hence let it remain

boxplot(`age`, 
        main = "Age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`age`)$out)) 
boxplot.stats(`age`)$out

# No outliers

boxplot(`Max heart rate`, 
        main = "Maximum Heart Rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`Max heart rate`)$out)) 
boxplot.stats(`Max heart rate`)$out

boxplot(oldpeak, 
        main = "Oldpeak", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(`oldpeak`)$out)) 
boxplot.stats(oldpeak)$out

# No need to remove outliers as variance among values are very less


attach(heart_data)

no_rows_data <- nrow(heart_data)
no_rows_data

sample_data <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- heart_data[sample_data, ]
testing_data <- heart_data[-sample_data, ]

nrow(training_data)
nrow(testing_data)

set.seed(1)
model <- glm(formula = `Chances of Heart Attack` ~ ., 
             family = "binomial", 
            data = training_data)

summary(model)

install.packages("pscl")
library(pscl)
pR2(model)

predicted_heartattack  <- predict(model, testing_data)
predicted_heartattack

actuals_predictions <- data.frame(cbind(Actuals = testing_data$`Chances of Heart Attack`, 
                                        Predicted = predicted_heartattack))

actuals_predictions

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

fitted.results <- predict(model,testing_data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(fitted.results)

misClasificError <- mean(fitted.results != testing_data$`Chances of Heart Attack`)
print(paste('Accuracy',1-misClasificError))
mean(fitted.results == testing_data$`Chances of Heart Attack`)

model <- glm(formula = `Chances of Heart Attack` ~ sex + `Chest Pain` + `Resting BP` + `Major Vessels` + Thalassemia,  
             family = "binomial", 
             data = training_data)
summary(model)

fitted.results <- predict(model,testing_data,type='response')
fitted.results
fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(fitted.results)

misClasificError <- mean(fitted.results != testing_data$`Chances of Heart Attack`)
print(paste('Accuracy',1-misClasificError))
mean(fitted.results == testing_data$`Chances of Heart Attack`)


