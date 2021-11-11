##########################################################
# Name: Sangeet Sagar (7009050), Philipp Schuhmacher (XXX)
# Email: {sasa00001,phsc00003}@stud.uni-saarland.de
# Project: EML-HW1
# Date: Tue 09 Nov 2021
# Organization: University des Saarlandes
##########################################################

# Clear all variables in the workspace
rm(list = ls())

############ PART 1 ############
# Load ISLR dataset and attach the "Auto" dataset
library(ISLR)
attach(Auto)

# Print all variable names
cat("Variable names:\n")
names(Auto)

# Plotting scatter plots, but exclude "name" variable
# The save it as PDF
data <- subset(Auto, select = -c(name))
# Open a pdf file
pdf("rplot.pdf") 
plot(data, pch = '*', col='blue', 
     frame = FALSE, main = "Scatter plot between all variables")
# Close the pdf file
dev.off()


############ PART 2 ############
# Creating correlation matrix
data.cor = cor(data)

cat("\nMost highly correlated variables: 
    (cylinder-displacement) and (displacement-weight)")
cat("\nMost highly anti-correlated variables: 
    (mpg-displacement) and (mpg-displacement)\n")

############ PART 3 ############
# Linear regression: 
# response (y)  : 'mpg'
# variables (x) : cylinders, displacement, horsepower, year

fit_lm <- function(response, variable, string){
  lm.variable = lm(response~variable, data=Auto)
  cat("\n*******",string,"*********")
  cat("\nCoefficients:\n")
  print(coef(summary(lm.variable)))
  cat("\nR-sq value:")
  print(summary(lm.variable)$r.sq)
  cat("\n")
}

fit_lm(mpg,cylinders, "cylinder")
fit_lm(mpg,displacement, "displacement")
fit_lm(mpg,horsepower, "horsepower")
fit_lm(mpg,year, "year")



############ PART 4 ############
# response (y)  : 'mpg'
# variables (x) : cylinders+displacement+horsepower+weight+acceleration+year
cat("\n********* Multiple linear regression (all-variable)**********\n")
lm.fit=lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year, data = Auto)
cat("\nModel Summary:")
summary(lm.fit)

cat("\nCoefficients:\n")
coef(summary(lm.fit))

cat("\nR-sq value:")
summary(lm.fit)$r.sq

############ PART 5 ############
# Open a pdf file
pdf("diagplot.pdf")
par(mfrow=c(2,2))
plot(lm.fit, pch = '*', col='blue', main = "Diagonistic plot")
# Close the pdf file
dev.off()


############ PART 6 ############
# Dropped

############ PART 7 ############
# Dropped
