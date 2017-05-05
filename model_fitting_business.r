### Model selection on business.train.subattr
# Train test split
set.seed(100)
indexbus = sample(1:nrow(business.train.sub), nrow(business.train.sub) * 0.75, replace = FALSE) #Select 75% as train data

bus.train.train = business.train.sub[indexbus, ]  #1722   14
bus.train.test = business.train.sub[-indexbus, ]  #575  14

rating.col.bus = ncol(business.train.sub)
actual.rating.bus = bus.train.test[,rating.col.bus]

# Run OLS regression
ols.mod.bus = lm(rating ~ ., data = data.frame(bus.train.train))
summary(ols.mod.bus)
ols.mod.pred.bus = predict.lm(ols.mod.bus, data = bus.train.test[,-rating.col.bus])
ols.mse.bus = mean((ols.mod.pred.bus - actual.rating.bus)^2)

# Run Lasso regression
library(glmnet)
set.seed(100)
bus.train.train = as.matrix(bus.train.train)
lasso.mod.bus = cv.glmnet(bus.train.train[, -rating.col.bus], bus.train.train[, rating.col.bus],
                          intercept = FALSE, standardize = TRUE)
lasso.coef.bus = coef(lasso.mod.bus)
lasso.mod.pred.bus = predict(lasso.mod.bus, s = lasso.mod.bus$lambda.min, newx = as.matrix(bus.train.test[, -rating.col.bus]))
lasso.mse.bus = mean((lasso.mod.pred.bus - actual.rating.bus)^2)

# Run Ridge regression
ridge.mod.bus = cv.glmnet(as.matrix(bus.train.train[, -rating.col.bus]), as.matrix(bus.train.train[, rating.col.bus]), intercept = FALSE, standardize = TRUE, alpha = 0)
ridge.mod.pred.bus = predict(ridge.mod.bus, s = ridge.mod.bus$lambda.min, newx = as.matrix(bus.train.test[, -rating.col.bus]))
ridge.mse.bus = mean((ridge.mod.pred.bus - actual.rating.bus)^2)

# Run random forest
library(randomForest)
set.seed(100)
rf.mod.class.bus = randomForest(bus.train.train[ ,-rating.col.bus], as.factor(unlist(bus.train.train[ ,rating.col.bus])), ntrees=1500)
rf.mod.pred.class.bus = predict(rf.mod.class.bus, newdata = bus.train.test)
rf.mse.class.bus = mean((as.numeric(as.character(rf.mod.pred.class.bus)) - actual.rating.bus)^2)

# Random forest with regression
set.seed(100)
rf.mod.reg.bus = randomForest(as.matrix(bus.train.train[ ,-rating.col.bus]), as.matrix(bus.train.train[ ,rating.col.bus]), ntrees=500)
rf.mod.pred.reg.bus = predict(rf.mod.reg.bus, newdata = bus.train.test)
rf.mse.reg.bus = mean((rf.mod.pred.reg.bus - actual.rating.bus)^2)  #close to boosting

# Run boosting
library(gbm)
boost.mod.bus = gbm(rating ~ ., data = data.frame(bus.train.train), distribution = "gaussian", interaction.depth = 2, n.trees = 500, shrinkage = 0.01)
boost.mod.pred.bus = predict(boost.mod.bus, newdata = as.data.frame(bus.train.test[,-rating.col.bus]), type = "response", n.trees = 500)
boost.mse.bus = mean((boost.mod.pred.bus - actual.rating.bus)^2)

# Run support vector machine
library(kernlab)
ksvm.mod.class.bus = ksvm(as.matrix(bus.train.train[,-rating.col.bus]), as.factor(unlist(bus.train.train[,rating.col.bus])), type = "C-svc", kernel = "rbfdot", C = 1)
ksvm.mod.pred.class.bus = predict(ksvm.mod.class.bus, as.matrix(bus.train.test[,-rating.col.bus]))
ksvm.mse.class.bus = mean(((as.numeric(as.character(ksvm.mod.pred.class.bus)) - actual.rating.bus)^2))

# Support vector machine regression
library(kernlab)
ksvm.mod.reg.bus = ksvm(as.matrix(bus.train.train[,-rating.col.bus]), bus.train.train[,rating.col.bus], type = "eps-svr", kernel = "rbfdot", C = 1)
ksvm.mod.pred.reg.bus = predict(ksvm.mod.reg.bus, as.matrix(bus.train.test[,-rating.col.bus]))
ksvm.mse.reg.bus = mean((ksvm.mod.pred.reg.bus - actual.rating.bus)^2)

# Run bagging
library(ipred)
bagging.mod.bus = bagging(rating ~ ., data = data.frame(bus.train.train))
bagging.mod.pred.bus = predict(bagging.mod.bus, newdata = data.frame(bus.train.test[,-rating.col.bus]))
bagging.mse.bus = mean((bagging.mod.pred.bus - actual.rating.bus)^2)

# Run KNN
library(FNN)
knn.mod.bus = knn.reg(as.data.frame(bus.train.train[, -rating.col.bus]), test = as.data.frame(bus.train.test[,-rating.col.bus]), 
                      y =as.data.frame(bus.train.train)$rating , k = 80, algorithm = "brute") 
knn.mod.pred.bus = knn.mod.bus$pred
knn.mse.bus = mean((knn.mod.pred.bus - actual.rating.bus)^2)

# emsemble boost+bagging
boost.bag.pred = apply(cbind(bagging.mod.pred.bus,boost.mod.pred.bus),1, mean)
boost.bag.mse = mean((boost.bag.pred - actual.rating.bus)^2)


### Model refitting on selected models and prediction on test set
### business.test data cleaning for predition
# delete index add response variable
business.test.sub = cbind(business.test.subattr[,-1])   # 440 x 13
rating.pred.bus = rep(NA, nrow(business.test.subattr))
business.test.sub = as.matrix(business.test.sub)
business.test.sub[is.na(business.test.sub)] = 0 #replace rest NA value to 0
business.test.sub = data.frame(business.test.sub)
business.test.sub$category = as.numeric(business.test.sub$category)
for (i in c(1:13)[-3]){
  business.test.sub[,i] = as.numeric(as.character(business.test.sub[,i]))
}
review.pred.test = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/raw_prediction_boosting_20.csv")
business.test.sub$review_pred = review.pred.test$x

### model refitting on training and prediction on test
roundfunc = function(output){
  output = round(output/0.5)*0.5
}

# Random forest with regression
set.seed(0)
rf.reg.bus = randomForest(as.matrix(business.train.sub[ ,-rating.col.bus]), as.matrix(business.train.sub[ ,rating.col.bus]), ntrees=500)
rf.pred.reg.bus = predict(rf.reg.bus, newdata = business.test.sub)
bus.rf = cbind(as.character(business.test$business_id), sapply(rf.pred.reg.bus,roundfunc))
colnames(bus.rf)= c("business_id", "stars")
#write.csv(bus.rf, file = "prediction_bus_rf.csv")

# boosting
library(gbm)
boost.bus = gbm(rating ~ ., data = data.frame(business.train.sub), distribution = "gaussian", interaction.depth = 2, n.trees = 500, shrinkage = 0.01)
boost.pred.bus = predict(boost.bus, newdata = as.data.frame(business.test.sub), type = "response", n.trees = 500)
bus.boost = cbind(as.character(business.test$business_id), sapply(boost.pred.bus,roundfunc))
colnames(bus.boost)= c("business_id", "stars")
#write.csv(bus.boost, file = "prediction_busrev_boost.csv", row.names = FALSE)


# Support vector machine regression
library(kernlab)
ksvm.reg.bus = ksvm(as.matrix(business.train.sub[,-rating.col.bus]), business.train.sub[,rating.col.bus], type = "eps-svr", kernel = "rbfdot", C = 1)
ksvm.pred.reg.bus = predict(ksvm.reg.bus, as.matrix(business.test.sub))
bus.ksvm = cbind(as.character(business.test$business_id), sapply(ksvm.pred.reg.bus,roundfunc))
colnames(bus.ksvm)= c("business_id", "stars")
#write.csv(bus.ksvm, file = "prediction_bus_svm.csv", row.names = FALSE)

# bagging
library(ipred)
bagging.bus = bagging(rating ~ ., data = data.frame(business.train.sub))
bagging.pred.bus = predict(bagging.bus, newdata = data.frame(business.test.sub))
bus.bag = cbind(as.character(business.test$business_id), sapply(bagging.pred.bus,roundfunc))
colnames(bus.bag)= c("business_id", "stars")
#write.csv(bus.bag, file = "prediction_busrev_bag.csv", row.names = FALSE)

# Ensemble prediction
bus.boost.bag.pred = apply(cbind(boost.pred.bus,bagging.pred.bus),1,mean)
bus.boost.bag = cbind(as.character(business.test$business_id), sapply(bus.boost.bag.pred,roundfunc))
colnames(bus.boost.bag)= c("business_id", "stars")
#write.csv(bus.boost.bag, file = "prediction_busrev_boost_bag.csv", row.names = FALSE)


