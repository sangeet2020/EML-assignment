##########################################################
# Name: Sangeet Sagar (7009050), Philipp Schuhmacher (7010127)
# Email: {sasa00001,phsc00003}@stud.uni-saarland.de
# Project: EML-HW3
# Date: Tue 13 Jan 2022
# Organization: University des Saarlandes
##########################################################

# Clear all variables in the workspace
rm(list = ls())


############ PART 1 ############
cat("\n############ PART 1 ############\n")
library ( boot )
load("prostate.Rdata")

############ PART 2 ############
cat("\n############ PART 2 ############\n")
library( glmnet )
X_train =  model.matrix(lpsa~.,prostate.train)[ ,-1] # variables (x)
y_train = prostate.train$lpsa  # response (y)

X_test =  model.matrix(lpsa~.,prostate.test)[ ,-1] # variables (x)
y_test = prostate.test$lpsa  # response (y)


grid = 10^seq(-3, 5, length.out = 100)
ridge.mod = glmnet(X_train,y_train,alpha=0,lambda=grid )

# Open a pdf file
pdf("prob5.1.pdf")
# par(mfrow=c(2,2))
plot(ridge.mod, xvar = "lambda", main="Ridge Regression")
legend("topright", lwd = 1, col = 1:6, legend = colnames(X_train), cex = 0.8)
# Close the pdf file
dev.off()

############ PART 3 ############
cat("\n############ PART 3 ############\n")
# cv.glmnet by default performs 10-fold CV
ridge.cv = cv.glmnet(X_train,y_train,alpha=0,lambda=grid )
cat("Optimal value of λ: ", ridge.cv$lambda.min,'\n')

# Determine train and test error
train_pred = predict(ridge.cv, X_train, s = "lambda.min")
test_pred = predict(ridge.cv, X_test, s = "lambda.min")

mse_train = mean((train_pred - y_train)^2)
mse_test = mean((test_pred - y_test)^2)

cat('MSE on train set:', mse_train,'\n')
cat('MSE on test set:', mse_test,'\n')

############ PART 4 ############
cat("\n############ PART 4 ############\n")
lasso.mod = glmnet(X_train,y_train,alpha=1,lambda=grid )

# Open a pdf file
pdf("prob5.2.pdf")
# par(mfrow=c(2,2))
plot(lasso.mod, xvar = "lambda", main="Lasso Regression")
legend("topright", lwd = 1, col = 1:6, legend = colnames(X_train), cex = 0.8)
# Close the pdf file
dev.off()

############ PART 5 ############
cat("\n############ PART 5 ############\n")
lasso.cv = cv.glmnet(X_train,y_train,alpha=1,lambda=grid )
cat("Optimal value of λ: ", lasso.cv$lambda.min,'\n')

# Determine train and test error
train_pred = predict(lasso.cv, X_train, s = "lambda.min")
test_pred = predict(lasso.cv, X_test, s = "lambda.min")

mse_train = mean((train_pred - y_train)^2)
mse_test = mean((test_pred - y_test)^2)

cat('MSE on train set:', mse_train,'\n')
cat('MSE on test set:', mse_test,'\n')
