insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)


# sex male- 0, female =1
# smoker- yes =1, no = 0
# Regions - 4 regions, so we need n-1 indicator variables

insurance_data$sex <- factor(insurance_data$sex, 
                             levels = c("male", "female"), 
                             ordered = FALSE)

str(insurance_data)

# convert smoker variable

insurance_data$smoker <- factor(insurance_data$smoker, 
                             levels = c("yes", "no"), 
                             ordered = FALSE)
str(insurance_data)

# convert region variable
insurance_data$sex <- factor(insurance_data$region, 
                             levels = c("northeast", "northwest", "southeast", "southwest"), 
                             ordered = FALSE)
str(insurance_data)


# Examine correlation between variables
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# temp and activ
pairs.panels(insurance_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

attach(insurance_data)
set.seed(1)
model <- lm(formula = charges ~ age + sex + bmi + smoker + region,
            data = insurance_data)
summary(model)

# Instead we will control the process 
# We will create our own variables

insurance_data$male <- ifelse(sex == "male", 1, 0) 
insurance_data$female <- ifelse(sex == "female", 1, 0)

# 
insurance_data$smokes <- ifelse(smoker == "yes", 1, 0)
insurance_data$no_smokes <- ifelse(smoker == "no", 1, 0)

# region
insurance_data$ne <- ifelse(region == "northeast", 1,0)
insurance_data$nw <- ifelse(region == "northwest", 1,0)
insurance_data$se <- ifelse(region == "southeast", 1,0)
insurance_data$sw <- ifelse(region == "southwest", 1,0)

names(insurance_data)

# drop uneeded variables
# using age, male, female, BMI, smoker
# nonsmoker, ne, nw, se, sw
insurance_data <- insurance_data[c(1, 3, 7, 8, 9, 10, 11, 12, 13, 14, 15)]

# round the bmi and charges value
insurance_data$bmi <- round(bmi, 1)
insurance_data$charges <- round(charges, 1)

# check model assumptions

scatter.smooth(x=charges,
               y=age,
               main = "Insurance charges ~age", 
               xlab = "Insurance charges (,000)", 
               ylab = "Age (years)")

scatter.smooth(x=age, y=charges)

scatter.smooth(x=charges, y=bmi)

scatter.smooth(x=bmi, y=charges)

# check correlation between both variables
cor(charges, male)
cor(age, charges)
cor(bmi, charges)

# Need to check also for outliers, normality, collinearity

# measure collinearity
# this is a measure of the relationship between multiple variables
# check the collinearity of the model
model <- lm(formula = charges ~ age + bmi + male + female + smokes + no_smokes + ne + nw + se + sw, 
            data = insurance_data)
summary(model)

install.packages("car")
library(car)

qqPlot(model, 
       labels = row.names(insurance_data), 
       id.method = "identify", 
       simulate = TRUE)
