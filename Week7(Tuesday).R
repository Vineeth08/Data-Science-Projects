#Read the data into dataframe

normal_data <- read.csv("NormalData.csv")
non_normal_data <- read.csv("NonNormalData.csv")
normal_data
non_normal_data

#Create histogram to show distribution

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))

hist(normal_data$x, 
     main = "Frequency chart Normal Data", 
     col = 'red')

hist(non_normal_data$x, 
     main = "Frequency chart Non Normal Data", 
     col = 'red')


#  Create a Q-Q plot for both data frames
# Check if points fall along one diagonal

qqnorm(normal_data$x, main = "Q-Q plot for Normal Data")
qqline(normal_data$x)

qqnorm(non_normal_data$x, main = "Q-Q plot for Normal Data")
qqline(non_normal_data$x)

# Shapiro-Wilk test
# If p-value > 0.05 then data variable is normally distributed
# If p-value < 0.05 then data variable is not normally distributed
# Test also presents a W statistic; we'll use the p-value

shapiro.test(normal_data$x)

# Shapiro- Wilk test for Non- Parametric Data

shapiro.test(non_normal_data$x)

# Perform the Kolmogorov-Smirnov test
# if p-value > 0.05 then data is normally distributed
# If p-value < 0.05 then data variable is not normally distributed

ks.test(normal_data$x, 'pnorm')
ks.test(non_normal_data$x, 'pnorm')

# Convert non-parametric data to transform
# log transform

log_non_normal_data <- log10(non_normal_data$x)

# If badly skewed data then use this

neg_skewed_log_non_normal_data <- log10(max(non_normal_data$x + 1) - non_normal_data$x )

hist(log_non_normal_data, 
     main = "Frequency chart Non Normal Data", 
     col = 'red')

#--------------------------------------Data questions for Beaver's dataset---------------------------------------
?beavers
str(beaver2)
beaver2

hist(beaver2$temp, 
     main = "Frequency chart temperature Data", 
     col = 'red')

qqnorm(beaver2$temp, main = "Q-Q plot for temperature data")
qqline(beaver2$temp, col = "Red")

shapiro.test(beaver2$temp)

# Research question1
# We would like to examine the mean body temperature of the beavers to determine
# whether activity has any effect on beaver body temperature.
# Devise a hypothesis test that we use for this analysis.

# Ans H0: Beaver activity has no effect on body temperature
# H1: Body activity has an effect on temperature

# Describe the variables you will use for the analysis. What type of variables are they?
# Do they need to be converted?

# look at the correlation between variables

pairs(beaver2, 
      labels  = colnames(beaver2), 
      main = "Beavers dataset correlation plot")

# we're examining body temp and activity
# so the vars need to be converted first

beaver2$activity <- factor(beaver2$activ, 
                           labels = c("no", "yes"))

# Analyse correlation of activity

plot(beaver2$activity, beaver2$temp)

library(lattice)
attach(beaver2)
histogram(~ temp | activity, 
          data = beaver2, 
          main = "Distribution of beaver activity data", 
          xlab = "Temp(degrees)",
          ylab = "Activity")

tapply(temp, activity, mean)

# Q-Q plot of activity
with(beaver2, {
  qqnorm(temp[activity == "no"],
         main = "Inactive data")
  qqline(temp[activity == "no"])
})


with(beaver2, 
     tapply(temp, activity, shapiro.test))




       