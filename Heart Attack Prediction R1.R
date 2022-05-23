heart_data <- read.csv("heart.csv", na = "")
heart_data

str(heart_data)

# Finding out the total NA values in the dataframe.
sum(is.na(heart_data))
na_count <- heart_data[!complete.cases(heart_data),]
na_count

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
             lm = TRUE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

heart_data$sex <- factor(heart_data$sex, labels = c("Female", "Male"))
heart_data$cp <- factor(heart_data$cp, labels = c("Typical Angina", 
                                                  "Atypical angina", 
                                                  "Non Anginal Pain", 
                                                  "Asymptomatic"))


heart_data$restecg <- factor(heart_data$restecg, labels = c("Normal", 
                                                  "Abnormal", 
                                                  "Ventricular Hypertrophy"))

heart_data$exng <- factor(heart_data$exng, labels = c("No", "Yes"))

heart_data$slp <- factor(heart_data$slp, labels = c("Upsloping",
                                                    "Flat", 
                                                    "Downsloping"))

heart_data$fbs <- factor(heart_data$fbs, labels = c("False", "True"))

table(heart_data$sex)
table(heart_data$cp)
table(heart_data$restecg)
table(heart_data$exng)
table(heart_data$slp)
table(heart_data$fbs)


attach(heart_data)

no_rows_data <- nrow(heart_data)
no_rows_data

sample_data <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- heart_data[sample_data, ]
testing_data <- heart_data[-sample_data, ]

nrow(training_data)
nrow(testing_data)

set.seed(1)
model <- glm(formula = output ~ age + sex + cp + trtbps + chol + 
               fbs + restecg + thalachh + exng + oldpeak + 
               slp + caa + thall, 
             family = "binomial", 
            data = training_data)

summary(model)

install.packages("pscl")
library(pscl)
pR2(model)

predicted_heartattack  <- predict(model, testing_data)
predicted_heartattack

actuals_predictions <- data.frame(cbind(Actuals = testing_data$output, 
                                        Predicted = predicted_heartattack))

actuals_predictions

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

fitted.results <- predict(model,testing_data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testing_data$output)
print(paste('Accuracy',1-misClasificError))


