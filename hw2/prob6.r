##########################################################
# Name: Sangeet Sagar (7009050), Philipp Schuhmacher (XXX)
# Email: {sasa00001,phsc00003}@stud.uni-saarland.de
# Project: EML-HW2
# Date: Tue 30 Nov 2021
# Organization: University des Saarlandes
##########################################################

# Clear all variables in the workspace
rm(list = ls())

############ Helper functions here ############
train_test_split = function(train_df, test_df){
  
  X_train = train_df[, -which(names(train_df) == "g")]
  y_train = train_df[, which(names(train_df) == "g")]
  
  X_test = test_df[, -which(names(test_df) == "g")]
  y_test = test_df[, which(names(test_df) == "g")]
  
  cat("Dimension of train df: ",dim(train_df),'\n')
  cat("Dimension of test df: ",dim(test_df),'\n')
  return(list("X_train" = X_train, "y_train" = y_train, "X_test" = X_test, "y_test" = y_test))
}

error_stats = function(fit_model, X_train, y_train, X_test, y_test){
  # Compute train error
  train_pred = predict(fit_model, newdata=X_train)
  train_pred_class = train_pred$class
  train_error = mean(train_pred_class != y_train)
  cat("Train error: ",train_error,'\n')
  
  # Compute test error
  test_pred = predict(fit_model, newdata=X_test)
  test_pred_class = test_pred$class
  test_error = mean(test_pred_class != y_test)
  cat("Test error: ",test_error,'\n')
  
  # Generate confusion matrix
  cnf_train = table(Train_predicted=train_pred_class, Actual=y_train)
  cnf_test = table(Test_predicted=test_pred_class, Actual=y_test)
  return(list("cnf_train" = cnf_train, "cnf_test" = cnf_test))
}

##########################################################


############ PART 1 ############
# Load phoneme dataset
cat("\n############ PART 1 ############\n")
data = read.csv("phoneme.csv")
response = data['g']
data = subset(data, select=-c(row.names,g))

cat("Dimension of dataset: ",dim(data),'\n')

#  Perform train test split on speaker column
train = grepl("^train", data$speaker)
predictors = subset(data, select=-c(speaker))
df = cbind(predictors, response)

train_df = subset(df, train)
test_df = subset(df, !train)

split = train_test_split(train_df, test_df)
X_train = split$X_train
y_train = split$y_train
X_test = split$X_test
y_test = split$y_test

############ PART 2 ############
cat("\n############ PART 2 (LDA) ############\n")
# Fit an lda model and compute error stats
library (MASS)
lda.fit = lda(X_train, grouping=y_train)
cnf_lda = error_stats(lda.fit, X_train, y_train, X_test, y_test)


############ PART 3 ############
cat("\n############ PART 3 (LDA: only two phonemes) ############\n")
# Select only two phonemes: `aa` and `ao`
train_response = grepl("aa|ao", train_df$g)
test_response = grepl("aa|ao", test_df$g)

binary_train_df = subset(train_df, train_response)
binary_test_df = subset(test_df, test_response)

split = train_test_split(binary_train_df, binary_test_df)
X_train_b = split$X_train
y_train_b = split$y_train
X_test_b = split$X_test
y_test_b = split$y_test

y_train_b = droplevels(y_train_b)
y_test_b = droplevels(y_test_b)

lda.fit = lda(X_train_b, grouping=y_train_b)
cnf_lda_b = error_stats(lda.fit, X_train_b, y_train_b, X_test_b, y_test_b)

############ PART 4 ############
cat("\n############ PART 4.1 (QDA) ############\n")
# Fit QDA model and computer error stats
qda.fit = qda(X_train, grouping=y_train)
cnf_qda = error_stats(qda.fit, X_train, y_train, X_test, y_test)

cat("\n############ PART 4.2 (QDA: only two phonemes) ############\n")
# Fit QDA model and computer error stats
qda.fit = qda(X_train_b, grouping=y_train_b)
cnf_qda_b = error_stats(qda.fit, X_train_b, y_train_b, X_test_b, y_test_b)

############ PART 5 ############
cat("\n############ PART 5.1 Confusion Matrix: LDA  ############\n")
cnf_lda_b$cnf_train
cnf_lda_b$cnf_test

cat("\n############ PART 5.1 Confusion Matrix: QDA  ############\n")
cnf_qda_b$cnf_train
cnf_qda_b$cnf_test

############ PART 6 ############
cat("\n############ PART 6 ############\n")
library ( boot )
load("prostate.Rdata")
X_train = subset(prostate.train, select=-c(lpsa)) # variables (x)
y_train = subset(prostate.train, select=c(lpsa))  # response (y)

X_test = subset(prostate.test, select=-c(lpsa))
y_test = subset(prostate.test, select=c(lpsa))

# LOOCV
start.time = Sys.time()
glm.fit = glm(lpsa~.,data=prostate.train)
cv.err = cv.glm(prostate.test, glm.fit)$delta[1]
end.time <- Sys.time()
time.taken <- end.time - start.time
cat("LOOCV test error: ",cv.err,'\n')
cat("LOOCV runtime: ",time.taken,'s\n')

# K-fold CV
set.seed(123)
k_fold_cv = function(prostate.train, prostate.test, k){
  cv.error.k=rep(0,k)
  start.time = Sys.time()
  
  for (i in 1:k){
    glm.fit = glm(lpsa~.,data=prostate.train)
    cv.error.k[i] = cv.glm(prostate.test, glm.fit, K =k)$delta[1]
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  return(list("cv.error" = cv.error.k, "runtime" = time.taken))
}

fold_cv.5 = k_fold_cv(prostate.train, prostate.test, 5)
cat("5-fold CV test error: ",fold_cv.5$cv.error,'\n')
cat("5-fold CV runtime: ",fold_cv.5$runtime,'s\n')

fold_cv.10 = k_fold_cv(prostate.train, prostate.test, 10)
cat("10-fold CV test error: ",fold_cv.10$cv.error,'\n')
cat("10-fold CV runtime: ",fold_cv.10$runtime,'s\n')
  