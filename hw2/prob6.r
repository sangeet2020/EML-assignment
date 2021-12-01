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
y_train = subs                [29;1H~                                                                                                                     [30;1H~                                                                                                                     [31;1H~                                                                                                                     [32;1H~                                                                                                                     [33;1H~                                                                                                                     [34;1H~                                                                                                                     [35;1H~                                                                                                                     [36;1H~                                                                                                                     [37;1H~                                                                                                                     [38;1H~                                                                                                                     [39;1H~                                                                                                                     [40;1H~                                                                                                                     [41;1H~                                                                                                                     [42;1H~                                                                                                                     [43;1H~                                                                                                                     [44;1H~                                                                                                                     [45;1H~                                                                                                                     [46;1H~                                                                                                                     [47;1H~                                                                                                                     [48;1H~                                                                                                                     [49;1H~                                                                                                                     [50;1H~                                                                                                                     [51;1H~                                                                                                                     [52;1H~                                                                                                                     [53;1H~                                                                                                                     [54;1H~                                                                                                                     [m[55;101H0,0-1[9CAll[1;1H[?25h[?25l[55;91H^[[1;1H[55;91H  [1;1H[55;91H^[[1;1H[55;91H  [1;1H[?25h[?25l[55;91H:[1;1H[55;1H[K[55;1H:[?2004h[?25hq[?25l[55;2H[K[55;2H[?25hq![?25l[?2004l[23;2t[23;1t[55;1H[K[55;1H[?2004l[?1l>[?25h[>4;m[?1049l[23;0;0t(base) [31msangeet(B[m@[32msagar(B[m:[36m/mnt/C2AA3265AA32565F/ws-21-22/EML/EML-assignment/hw2(B[m$ ll
total 12M
-rwxrwxrwx 1 root root 4.1K Nov 13 21:36 [0m[01;32mprostate.Rdata[0m
-rwxrwxrwx 1 root root  12M Nov 13 21:36 [01;32mphoneme.csv[0m
-rwxrwxrwx 1 root root 183K Nov 13 21:36 [01;32mExercise_sheet_2.pdf[0m
-rwxrwxrwx 1 root root  15K Nov 30 21:49 [01;32mUntitled.ipynb[0m
-rwxrwxrwx 1 root root 5.2K Dec  1 13:06 [01;32mprob6.r[0m
(base) [31msangeet(B[m@[32msagar(B[m:[36m/mnt/C2AA3265AA32565F/ws-21-22/EML/EML-assignment/hw2(B[m$ vim prob6.r 
[?1049h[22;0;0t[>4;2m[?1h=[?2004h[1;55r[?12h[?12l[22;2t[22;1t[27m[23m[29m[m[H[2J[?25l[55;1H"prob6.r" [noeol] 132L, 8192C[2;1H▽[6n[2;1H  [1;1H[>c]10;?]11;?[55;1H[1;1H[?25h[?2004l[?2004hP+q436f\P+q6b75\P+q6b64\P+q6b72\P+q6b6c\P+q2332\P+q2334\P+q2569\P+q2a37\P+q6b31\[?2004l[?2004h[?2004l[?2004h[?2004l[?2004h[?25l[34m##########################################################
# Name: Sangeet Sagar (7009050), Philipp Schuhmacher (XXX)
# Email: {sasa00001,phsc00003}@stud.uni-saarland.de
# Project: EML-HW2
# Date: Tue 30 Nov 2021
# Organization: University des Saarlandes
##########################################################

# Clear all variables in the workspace[m
[36mrm[m[35m([m[32mlist[m [38;5;130m=[m [36mls[m[35m())[m

[34m############ Helper functions here ############[m
train_test_split [38;5;130m=[m [32mfunction[m[35m([mtrain_df[35m,[m test_df[35m){[m

  X_train [38;5;130m=[m train_df[35m[,[m [38;5;130m-[m[36mwhich[m[35m([m[36mnames[m[35m([mtrain_df[35m)[m [38;5;130m==[m [31m"g"[m[35m)][m
  y_train [38;5;130m=[m train_df[35m[,[m [36mwhich[m[35m([m[36mnames[m[35m([mtrain_df[35m)[m [38;5;130m==[m [31m"g"[m[35m)][m

  X_test [38;5;130m=[m test_df[35m[,[m [38;5;130m-[m[36mwhich[m[35m([m[36mnames[m[35m([mtest_df[35m)[m [38;5;130m==[m [31m"g"[m[35m)][m
  y_test [38;5;130m=[m test_df[35m[,[m [36mwhich[m[35m([m[36mnames[m[35m([mtest_df[35m)[m [38;5;130m==[m [31m"g"[m[35m)][m

  [36mcat[m[35m([m[31m"Dimension of train df: "[m[35m,[m[36mdim[m[35m([mtrain_df[35m),[m[31m'[m[35m\n[m[31m'[m[35m)[m
  [36mcat[m[35m([m[31m"Dimension of test df: "[m[35m,[m[36mdim[m[35m([mtest_df[35m),[m[31m'[m[35m\n[m[31m'[m[35m)[m
  [38;5;130mreturn[m[35m([m[32mlist[m[35m([m[31m"X_train"[m [38;5;130m=[m X_train[35m,[m [31m"y_train"[m [38;5;130m=[m y_train[35m,[m [31m"X_test"[m [38;5;130m=[m X_test[35m,[m [31m"y_test"[m [38;5;130m=[m y_test[35m))
}[m

error_stats [38;5;130m=[m [32mfunction[m[35m([mfit_model[35m,[m X_train[35m,[m y_train[35m,[m X_test[35m,[m y_test[35m){[m
  [34m# Compute train error[m
  train_pred [38;5;130m=[m [36mpredict[m[35m([mfit_model[35m,[m newdata[38;5;130m=[mX_train[35m)[m
  train_pred_class [38;5;130m=[m train_pred[35m$[mclass
  train_error [38;5;130m=[m [36mmean[m[35m([mtrain_pred_class [38;5;130m!=[m y_train[35m)[m
  [36mcat[m[35m([m[31m"Train error: "[m[35m,[mtrain_error[35m,[m[31m'[m[35m\n[m[31m'[m[35m)[m

  [34m# Compute test error[m
  test_pred [38;5;130m=[m [36mpredict[m[35m([mfit_model[35m,[m newdata[38;5;130m=[mX_test[35m)[m
  test_pred_class [38;5;130m=[m test_pred[35m$[mclass
  test_error [38;5;130m=[m [36mmean[m[35m([mtest_pred_class [38;5;130m!=[m y_test[35m)[m
  [36mcat[m[35m([m[31m"Test error: "[m[35m,[mtest_error[35m,[m[31m'[m[35m\n[m[31m'[m[35m)[m

  [34m# Generate confusion matrix[m
  cnf_train [38;5;130m=[m [36mtable[m[35m([mTrain_predicted[38;5;130m=[mtrain_pred_class[35m,[m Actual[38;5;130m=[my_train[35m)[m
  cnf_test [38;5;130m=[m [36mtable[m[35m([mTest_predicted[38;5;130m=[mtest_pred_class[35m,[m Actual[38;5;130m=[my_test[35m)[m
  [38;5;130mreturn[m[35m([m[32mlist[m[35m([m[31m"cnf_train"[m [38;5;130m=[m cnf_train[35m,[m [31m"cnf_test"[m [38;5;130m=[m cnf_test[35m))
}[m

[34m##########################################################


############ PART 1 ############
# Load phoneme dataset[m
[36mcat[m[35m([m[31m"[m[35m\n[m[31m############ PART 1 ############[m[35m\n[m[31m"[m[35m)[m
data [38;5;130m=[m [36mread.csv[m[35m([m[31m"phoneme.csv"[m[35m)[m
response [38;5;130m=[m data[35m[[m[31m'g'[m[35m][m
data [38;5;130m=[m [36msubset[m[35m([mdata[35m,[m select[38;5;1