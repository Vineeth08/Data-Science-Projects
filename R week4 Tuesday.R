

column_names <- c("Dosage", "Response to DrugA", "Response to DrugB")
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

drugs <- data.frame(dose, drugA, drugB)

drugs
plot(drugs)
attach(drugs)
plot(dose, type = "o", col = "blue")

# option type = "b" means
# we can plot both points and lines

plot(dose, drugA, type = "b")

#store the content of par() to a variable
help(par)

opar <- par(no.readonly = TRUE)
opar
plot(dose, drugB, type = "b")

#lty = dashed line
#pch = 17(triangle)

par(lty =2, pch = 17)
plot(dose, drugA, type = "b")
par(opar)

# specify graphical parameters
plot(dose, drugA, type ='b', lty = 2, pch =17)

# dotted line 3 times wider than the default
plot(drugA, 
     type = "b", 
     lty = 3, 
     lwd = 3, 
     pch = 15, 
     ylim = c(0, 100))
title(main = "Drug Dosage", 
      col.main = "purple", 
      font.main = 4)

#plot drugB with red dashed line and square points
# pch = 22

plot(drugB, 
     type = "b", 
     col = "red",
     lty = 3, 
     lwd = 3, 
     pch = 22, 
     ylim = c(0,100))

graph_range <- range(0, drugA, drugB)
graph_range

plot(drugB, 
     type = "b", 
     col = "red",
     lty = 3, 
     lwd = 3, 
     pch = 22, 
     ylim = c(0, graph_range[2]))

#label axis values
# make the x axis have my labels

axis(1, 
     at = 1:5, 
     lab = c("20 ml", "40 ml", "60 ml", "80 ml", "100 ml") )

axis(2, at =0:graph_range[2]/5)

# FULL EXAMPLE OF CHART
#PLOT DOSE WITH DRUGA with labels
plot(dose, 
     drugA, 
     type = "b", 
     col = "red", 
     lty =2, 
     pch =2, 
     lwd =2, 
     main = "Clinical trial for drugA", 
     sub = "This is hypothethical data", 
     xlab = "Dosage", 
     ylab = "Drug Response", 
     xlim = c(0,60), ylim = c(0,70))


# use mtcars
#capture parameters
opar <- par(no.readonly = TRUE)
# include mfrow = c(nrow, ncol)

mtcars
par(mfrow = c(2,2))
attach(mtcars)
str(mtcars)
plot(wt, mpg, main = "scatterplot of weight vs mpg")
plot(wt, disp, main = "scatterplot of weight vs disp")

par(opar)
# show 3 plots in 3 rows and 1 col
#of wt, mpg and disp

opar <- par(no.readonly = TRUE)
par(mfrow = c(3,1))
#attach(mtcars)
plot(wt, main = "scatterplot of weight", xlab = "100", ylab = "weight")
plot(disp, main = "scatterplot disp", xlab = "150", ylab = "displacement")
plot(mpg, main = "scatterplot of mpg", xlab = "10", ylab = "miles per gallons")

par(opar)

# use graph range with the charts




