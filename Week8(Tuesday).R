# simple example using the women dataset
str(women)

# predict weight from height
# dependent variable = weight
# indpendent variable = height
# 
simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model
summary(simple_linear_model)

plot(women$height, 
     women$weight, 
     xlab = "height (inches)", 
     ylab = "weight (lbs)", 
     main = "Scatter plot showing the regression line for weight predicted from height")

abline(simple_linear_model)

# Analyse the correlation coefficient
# measure level of association between 2 variables
# -1 = perfect negative correlation
# +1 = perfect positive correlation

# A value -0.2 < x < 0.2 suggests that  much of the variation in
# outcome variable is not explained by the predictor
# Then we should look for better predictor variables

confint(simple_linear_model)

cor(women$height, women$weight)

# Model accuracy - Goodness of Fit
# 3 quantities
# R-Squared (r^2)
# F-test statistic

summary(simple_linear_model)

#  RSE = 1.525 = prediction error rate
# when comparing 2 models, smallest RSE is best
# In this model, observed weight values deviate from the true regression by approx 1.5 units on average

# r2
# Ranges from 0-1
# High r2 = good indicator that the model variability in the
# outcome can be explained by the model
# A number close to 0 = model does not explain much of the variability

# f statistic - A large F statistic corresponds to a significant p-value
# p < 0.05

height <- 120
test_data <- data.frame(height1)


prdicted_weight <- predict(simple_linear_model, test_data)
prdicted_weight


# Build a model to predict distance from speed
# using the cars dataset

# 1st step - check model assumptions
# These are the core assumptions
# Linearity among variables
# Normality - normal distribution
# No collinearity - vars are not a linear combination of others
# Independence - residuals are independent and not correlated

# check linearity first using scatter plot
# x axis  = independent variable
# y axis  = dependent variable
# if relationship exists then the linearity assumption is validated

cars
scatter.smooth(x = cars$speed, y = cars$dist, 
               main = "Distance ~ Speed", 
               xlab = "car speed (mph)", 
               ylab = "stopping distance")
cor(cars$speed, cars$dist)

# check outliers
# An outlier = 1.5 x interquartile range
# we need to check speed and distance for outliers

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
attach(cars)
boxplot(speed, main = "speed",
        sub = paste("outliers rows:", boxplot.stats(speed)$out))


boxplot(dist, main = "distance",
        sub = paste("outliers rows:", boxplot.stats(dist)$out))

par(opar)
detach(cars)

# delete value with distance 120 because its an outlier

cars <- subset(cars, cars$dist != 120)

nrow(cars)


simple_linear_model <- lm(dist ~ speed, data = cars)
simple_linear_model
summary(simple_linear_model)




