library(tidyverse)
library(cyphid)
library(R.matlab)

pathname = "./Dataset/Cleaned/yelp_review_bag_of_words_1500.mat"
data = readMat(pathname, sparseMatrixClass = "matrix")
train.mat = data$train.mat
test.mat = data$test.mat
words = data$vocabulary.words
colnames(train.mat) = words
colnames(test.mat) = words
text.m = train.mat("./yelp_academic_dataset_review_train.csv")
text.m = train.mat
text.m = add.col(text.m,review.train$stars)
colnames(text.m)[ncol(text.m)] = "rating"
dim(text.m)

#Train test split
set.seed(0)
index = sample(1:nrow(text.m), nrow(text.m) * 0.75, replace = FALSE) #Select 75% as train data
text.m.train = text.m[index, ]
text.m.test = text.m[-index, ]
rating.col = ncol(text.m)
actual.rating = text.m.test[,rating.col]

# Run random forest
library(randomForest)
set.seed(1)
rf.mod.class = randomForest(text.m.train[ ,-rating.col], as.factor(unlist(text.m.train[ ,rating.col])), ntrees=50)
rf.mod.pred.class = predict(rf.mod.class, newdata = text.m.test[ ,-rating.col])
rf.mse.class = mean((as.numeric(as.character(rf.mod.pred.class)) - actual.rating)^2)

# Random forest with regression
set.seed(1)
rf.mod.reg = randomForest(as.matrix(text.m.train[ ,-rating.col]), as.matrix(text.m.train[ ,rating.col]), ntrees=50)
rf.mod.pred.reg = predict(rf.mod.reg, newdata = text.m.test[ ,-rating.col])
rf.mse.reg = mean((rf.mod.pred.reg - actual.rating)^2)

# Run boosting
library(gbm)
boost.mod = gbm(rating ~ ., data = data.frame(text.m.train), distribution = "gaussian", interaction.depth = 2, n.trees = 500, shrinkage = 0.01)
boost.mod.pred = predict(boost.mod, newdata = data.frame(text.m.test[,-rating.col]), type = "response", n.trees = 500)
boost.mse = mean((boost.mod.pred - actual.rating)^2)

# Run SVM
library(e1071)
svm.model.class = svm(as.matrix(text.m.train[,-rating.col]), as.factor(unlist(text.m.train[,rating.col])), cost = 1, gamma = 1,type = "C-classification")
svm.pred.class  = predict(svm.model.class, as.matrix(text.m.test[,-rating.col]))
svm.mse.class = mean(((as.numeric(as.character(svm.pred.class)) - actual.rating)^2))

# Run SVM Reg
svm.model.reg = svm(as.matrix(text.m.train[,-rating.col]), as.matrix(text.m.train[,rating.col]), cost = 1, gamma = 1,type = "nu-regression")
svm.pred.reg  = predict(svm.model.reg, as.matrix(text.m.test[,-rating.col]))
svm.mse.class = mean(((as.numeric(as.character(svm.pred.reg)) - actual.rating)^2))

# Run kernel support vector machine
library(kernlab)
ksvm.mod.class = ksvm(as.matrix(text.m.train[,-rating.col]), as.factor(unlist(text.m.train[,rating.col])), type = "C-svc", kernel = "rbfdot", C = 1)
ksvm.mod.pred.class = predict(ksvm.mod.class, as.matrix(text.m.test[,-rating.col]))
ksvm.mse.class = mean(((as.numeric(as.character(ksvm.mod.pred.class)) - actual.rating)^2))

# Run kernel Support vector machine regression
library(kernlab)
ksvm.mod.reg = ksvm(as.matrix(text.m.train[,-rating.col]), as.character(text.m.train[,rating.col]), type = "eps-svr", kernel = "rbfdot", C = 1)
ksvm.mod.pred.reg = predict(ksvm.mod.reg, as.matrix(text.m.test[,-rating.col]))
ksvm.mse.reg = mean((ksvm.mod.pred.reg - actual.rating)^2)

# Run bagging
library(ipred)
bagging.mod = bagging(rating ~ ., data = data.frame(text.m.train))
bagging.mod.pred = predict(bagging.mod, newdata = data.frame(text.m.test[,-rating.col]))
bagging.mse = mean((bagging.mod.pred - actual.rating)^2)

# Run KNN
library(FNN)
knn.mod <- knn.reg(as.data.frame(text.m.train[, -rating.col]), test = as.data.frame(text.m.test[, -rating.col]), y = text.m.train[,rating.col], k = 30, algorithm = "brute")
knn.mod.pred <- knn.mod$pred
knn.mse = mean((knn.mod.pred - actual.rating)^2)

# Run Smooth KNN
knn.kernel.mod <- ksmooth(text.m.train[,-rating.col], text.m.train[,rating.col], kernel = "normal")


# Run OLS regression
ols.mod = lm(rating ~ ., data = text.m.train)
ols.mod.pred = predict(ols.mod, data = text.m.test)
ols.mse = mean((ols.mod.pred - actual.rating)^2)

# Run Lasso regression
library(glmnet)
set.seed(0)
lasso.mod = cv.glmnet(as.matrix(text.m.train[, -rating.col]), as.matrix(text.m.train[, rating.col]), intercept = FALSE, standardize = TRUE)
lasso.coef = coef(lasso.mod)
lasso.mod.pred = predict(lasso.mod, s = lasso.mod$lambda.min, newx = as.matrix(text.m.test[, -rating.col]))
lasso.mse = mean((lasso.mod.pred - actual.rating)^2)

length(lasso.coef[which(lasso.coef!= 0),])
lasso.coef[which(lasso.coef!= 0),]

# Run Ridge regression
ridge.mod = cv.glmnet(as.matrix(text.m.train[, -rating.col]), as.matrix(text.m.train[, rating.col]), intercept = FALSE, standardize = TRUE, alpha = 0)
ridge.mod.pred = predict(ridge.mod, s = ridge.mod$lambda.min, newx = as.matrix(text.m.test[, -rating.col]))
ridge.mse = mean((ridge.mod.pred - actual.rating)^2)
