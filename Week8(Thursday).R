# Cars linear regression example

scatter.smooth(x = cars$dist, 
               y = cars$speed, 
               main = "Distance ~ Speed",
               xlab = "Stopping distance",
               ylab = "Car speed")

# This is a high correlation value and suggests 
# a high positive correlation between both variables.
cor(cars$speed, cars$dist)

# Check for outliers in variables
# Generally, any data point that lies outside the 
# 1.5 * interquartile-range (1.5 * IQR) is considered 
# an outlier, Where:

# IQR is calculated as the distance between the 
# 25th percentile and 75th percentile values for that 
# variable.

# We’ll need to check both distance and speed.

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
attach(cars)
boxplot(speed, 
        main = "Speed", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(speed)$out)) # box plot for 'speed'

boxplot(dist, 
        main = "Distance", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(dist)$out)) # box plot for 'distance'

detach(cars)
par <- opar

# The boxplots suggest that there is 1 outlier 
# in the data in the distance variable where the 
# speed is 120.

nrow(cars)

# Remove the outlier row
cars <- subset(cars, cars$dist!=120)

# Check that outlier row has been removed
nrow(cars)

# Rerun the boxplot to verify that outliers have been removed
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
attach(cars)
boxplot(speed, 
        main = "Speed", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(speed)$out)) # box plot for 'speed'

boxplot(dist, 
        main = "Distance", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(dist)$out)) # box plot for 'distance'

detach(cars)
par <- opar


# check normality with histogram and Q-Q plot
# Skewness function from e1071 library
install.packages("e1071")
library(e1071)
par(mfrow = c(1,2))
# density plot for speed
plot(density(cars$speed), main = "Density Plot : Speed", 
     ylab = "Frequency", 
     xlab = "Speed", 
     sub = paste("skewness: ", round(e1071::skewness(cars$speed), 2)))
     
# Fill the area under the plot with a colour
# in this example it is red
polygon(density(cars$speed), col = "red")

# Skewness less than -1 or greater than 1 is highly skewed
# -1 to -0.5 or 0.5 to 1 = moderately skewed
# -0.5 to 0.5 = approx symmetrical

plot(density(cars$dist), main = "Density Plot : Distance", 
     ylab = "Frequency", 
     xlab = "Distance", 
     sub = paste("skewness: ", round(e1071::skewness(cars$dist), 2)))

# Fill the area under the plot with a colour
# in this example it is red
polygon(density(cars$dist), col = "red")

hist(cars$dist, main = "Normality proportion of distance", 
     xlab = "Distance")

qqnorm(cars$dist, main = "Q-Q plot for distance")
qqline(cars$dist, col = "blue")


shapiro.test(cars$dist)

# split the data into training and testing
# set the initial random variability seed
set.seed(1)

# split the dataset into 70% training and 30% testing
no_rows_data <- nrow(cars)
no_rows_data
sample_data <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- cars[sample_data, ]
testing_data <- cars[-sample_data, ]

nrow(training_data)
nrow(testing_data)


linear_model <- lm(dist ~ speed, data = training_data)
summary(linear_model)

AIC(linear_model)
?AIC
BIC(linear_model)

# predict values using the model 
# distance will be predicted and compared to 
# test data
predicted_distance  <- predict(linear_model, testing_data)
predicted_distance

actuals_predictions <- data.frame(cbind(Actuals = testing_data$dist, 
                                        Predicted = predicted_distance))

actuals_predictions

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

par(opar)
# MAPE value represents the mean forecast by which the results
# the results will produce an error
mape <- mean(abs(actuals_predictions$Predicted - actuals_predictions$Actuals) / actuals_predictions$Actuals)
mape

install.packages("DAAG")
library(DAAG)
cv_results <- suppressWarnings(CVlm(data = cars, 
                                    form.lm = dist ~ speed, 
                                    dots = FALSE, 
                                    seed =1, legend.pos = "bottom", 
                                    main = "small symbols are predicted values and bigger are actuals"))
predict_data <- data.frame((speed = c(10)))

predicted_distance <- predict(linear_model, predict_data)
predicted_distance



# ----------------------- part2---------------------------------------

state.x77
class(state.x77)

states <- as.data.frame(state.x77)
class(states)
head(states)

# move state name into a new variable
states$name <- state.name
colnames(states)[colnames(states) == "Life Exp"] <- "Life_exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_grad"

# Find correlation among variables

install.packages("psych")
library(psych)

pairs.panels(states,
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

states_comparison <- as.data.frame(state.x77)
pairs(states_comparison)
