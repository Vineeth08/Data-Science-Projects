# Insurance dataset 
# Load the dataset into a data frame first

insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# Smoker - yes = 1, no = 0
# Region contains 4 categories
# N = 4, so we need n-1 indicator variables
# = 3 indicator variables
# Code variables in alphabetical order
head(insurance_data$region, 15)

# Convert variables as described above
names(insurance_data)
attach(insurance_data)

insurance_data$sex <- factor(sex,
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(region,  
                                levels = c("northeast", "northwest", "southeast", "southwest"), 
                                ordered = FALSE)

str(insurance_data)

# View the split of males and females within the data frame
table(insurance_data$sex)
table(insurance_data$smoker)
table(insurance_data$region)

pairs(insurance_data)
# Initial investigation of data variables
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# smoker and charges, perhaps charges and age
# and BMI and charges
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# if we build the model now, R will automatically split the
# factor variables
# Alternatively we will control this process

# in linear regresison, mode lis:
# y = b0 + B1x1 + B2x2 + B3x3 + e
# where y = insurance charges
# x1 = age of the person
# x2  sex of the person
# x3 = bmi of the person
# x4 = children
# x5 = smoker
# x6 = region
# It is clear that x1, and x3 are continuous and x2, x4, x5, x6 are categorical
# therefore we need to create dummy variables for the categorical
# variables
# For the sex variable x2
# x2 = 1 if person is male
# x2 = 0 if person is female

set.seed(1)
model <- lm(formula = charges ~ age + sex + bmi + children + smoker + region, 
            data = insurance_data)

summary(model)

# obviously age, bmi, children, smokerno have an influence over the dependent variable "charges"
# p- value less than significance value
# sexfemale is not influential on the model
# keep region for now as we need to use it for research question

insurance_data <- insurance_data[c(1, 3:7)]


insurance_data$bmi <- round(bmi, 1)
insurance_data$charges <- round(charges, 2)

# create the model again with ammended changes

set.seed(1)
model <- lm(formula = charges ~ age + bmi + children + smoker + region, 
            data = insurance_data)
summary(model)

# Check assumptions 
# Linearity
# We can examine if linear correlation
# correlation exists between continuous variables

scatter.smooth(x = age, y = charges, main = "Insurance Charges ~ age", 
               xlab = "Age (years)", 
               ylab = "Insurance charges (000)")

scatter.smooth(x = bmi, y = charges, main = "Insurance Charges ~ bmi", 
               xlab = "bmi", 
               ylab = "Insurance charges (000)")

scatter.smooth(x = children, y = charges, main = "Insurance Charges ~ children", 
               xlab = "children", 
               ylab = "Insurance charges (000)")

# No information available for plot of continuous vs categorical

scatter.smooth(x = smoker, y = charges, main = "Insurance Charges ~ smoker", 
               xlab = "smoker", 
               ylab = "Insurance charges (000)")


# No info available for plot of continuous vs categorical
# instead we will use boxplot

plot(insurance_data$smoker, 
     insurance_data$charges, 
     main = "charges by smoker status", 
     xlab = "smoker", 
     ylab =  "insurance charges")

plot(insurance_data$region, 
     insurance_data$charges, 
     main = "charges by region", 
     xlab = "region", 
     ylab =  "insurance charges")


# Only use this method if we are sure about the underlying nature 
# of the variables you are correlating

insurance_data$cor_smoker <- ifelse(smoker == "yes", 1,0)
cor(insurance_data$charges, insurance_data$cor_smoker)

# Normality
with(insurance_data, 
     {qqnorm(age, main = "Normality analysis of age data")
      qqline(age)})

shapiro.test(insurance_data$age)


# repeat for all continuous data

with(insurance_data, 
     {qqnorm(charges[smoker == "yes"], main = "Normality analysis of smoker =  yes")
       qqline(charges[smoker == "yes"])})

with(insurance_data, 
     {qqnorm(charges[smoker == "no"], main = "Normality analysis of smoker =  no")
       qqline(charges[smoker == "no"])})

shapiro.test(insurance_data$charges)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))

with(insurance_data, 
     {qqnorm(charges[region == "northeast"], main = "Normality analysis of region =  northeast")
       qqline(charges[region == "northeast"])})


with(insurance_data, 
     {qqnorm(charges[region == "northwest"], main = "Normality analysis of region =  northwest")
       qqline(charges[region == "northwest"])})


with(insurance_data, 
     {qqnorm(charges[region == "southeast"], main = "Normality analysis of region =  southeast")
       qqline(charges[region == "southeast"])})

with(insurance_data, 
     {qqnorm(charges[region == "southwest"], main = "Normality analysis of region =  southwest")
       qqline(charges[region == "southwest"])})


# check for normality
with(insurance_data, tapply(charges, smoker, shapiro.test))
with(insurance_data, tapply(charges, region, shapiro.test))

par(opar)

# measure colinearity which is the relationship between multiple variables
# Tolerance is an indication of the percent
# variance in the predictor that cannot be accounted for the other predictors
# hence very small values indicate a predictor in redundant

# VIF score should be close to 1 but under 5
# 10+ indicates that variable is not needed
# and can be removed from the model


library(car)
vif(model)













